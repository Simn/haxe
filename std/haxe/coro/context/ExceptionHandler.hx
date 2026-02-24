package haxe.coro.context;

import haxe.CallStack.StackItem;
import haxe.Exception;
import haxe.coro.BaseContinuation;
import haxe.coro.CoroStackItem;
import haxe.ds.ObjectMap;

/**
	An abstract context element that handles exception stack trace management for coroutines.
	`BaseContinuation.startException` and `BaseContinuation.buildCallStack` delegate to this element.

	The default implementation is `DefaultExceptionHandler`.
**/
abstract class ExceptionHandler implements IElement<ExceptionHandler> {
	public static final key = new Key<ExceptionHandler>('ExceptionHandler');

	/**
		Called when an exception is first encountered in a coroutine to process its stack trace.
		Returns the (potentially modified) exception.
	**/
	abstract public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception;

	/**
		Called as an exception propagates up the coroutine continuation chain, to insert each
		continuation frame's stack item into the exception stack.
	**/
	abstract public function buildCallStack(cont:BaseContinuation<Any>):Void;

	public function getKey() {
		return key;
	}
}

/**
	The default `ExceptionHandler` implementation, which reconstructs the coroutine call stack
	from the continuation chain and inserts it into the exception's stack trace.

	The insert-index map is shared across threads (protected by a lock on threaded targets)
	so that exceptions which cross coroutine-context / thread boundaries (e.g. re-thrown by
	`CoroRun.run`) are correctly recognized as already-processed.
**/
class DefaultExceptionHandler extends ExceptionHandler {
	#if debug
	#if target.threaded
	static final _lock = new sys.thread.Mutex();
	#end
	static final _insertIndexByException = new ObjectMap<Exception, Int>();

	static function getInsertIndex(exception:Exception):Null<Int> {
		#if target.threaded
		_lock.acquire();
		#end
		final v = _insertIndexByException.get(exception);
		#if target.threaded
		_lock.release();
		#end
		return v;
	}

	static function setInsertIndex(exception:Exception, idx:Int):Void {
		#if target.threaded
		_lock.acquire();
		#end
		_insertIndexByException.set(exception, idx);
		#if target.threaded
		_lock.release();
		#end
	}
	#end

	static inline function toStackItem(item:CoroStackItem):StackItem {
		return switch (item) {
			case ClassFunction(cls, func, file, line, column):
				StackItem.FilePos(StackItem.Method(cls, func), file, line, column);
			case LocalFunction(id, file, line, column):
				StackItem.FilePos(StackItem.LocalFunction(id), file, line, column);
		};
	}

	static inline function itemMatchesCoroFrame(item:StackItem, frameItem:CoroStackItem):Bool {
		return switch [item, frameItem] {
			case [FilePos(Method(cls2, func2), _, _, _), ClassFunction(cls, func, _, _, _)]:
				cls == cls2 && func == func2;
			case [FilePos(LocalFunction(id2), _, _, _), LocalFunction(id, _, _, _)]:
				id == id2;
			case _:
				false;
		}
	}

	static function itemMatchesAnyCoroFrame(item:StackItem, frames:Array<CoroStackItem>):Bool {
		for (frame in frames) {
			if (itemMatchesCoroFrame(item, frame)) {
				return true;
			}
		}
		return false;
	}

	public function new() {}

	public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception {
		#if js
		return exception;
		#end
		#if debug
		final existingIdx = getInsertIndex(exception);

		if (existingIdx != null) {
			// Exception was already processed by an inner coroutine chain.
			// Don't reprocess the stack. Instead, extract the sync bridge frames
			// from the native exception stack and append them.
			// On eval the native exception stack is ordered outermost-to-innermost:
			//   [syncFun1, syncFun2, CoroRun.run, ..., resolveTask]
			// We collect entries before the first CoroRun entry (the sync bridge),
			// then reverse them to get innermost-first order.
			var syncFrames:Array<StackItem> = [];
			var nativeExcStack = haxe.CallStack.exceptionStack(true);
			if (nativeExcStack != null) {
				for (nativeItem in nativeExcStack) {
					switch (nativeItem) {
						case FilePos(Method("hxcoro.CoroRun", _), _):
							break;
						case _:
							syncFrames.push(nativeItem);
					}
				}
				syncFrames.reverse();
			}

			if (syncFrames.length > 0) {
				var stack = exception.stack.asArray();
				for (frame in syncFrames) {
					stack.push(frame);
				}
				exception.stack = stack;
			}

			// Use negative value to signal that the immediate buildCallStack call
			// (from the catching continuation's exception handler) should be skipped.
			setInsertIndex(exception, -(exception.stack.asArray().length + 1));
			return exception;
		}

		var stack = [];
		var localInsertIndex = 0;
		var frameItem = cont.getStackItem();
		var seenInvokeResume = false;
		var skippedChainFrame = false;
		var chainFrames = [];

		/*
			Find first coro stack element
		*/
		var currentFrame:Null<haxe.coro.IStackFrame> = cont;
		while (frameItem == null) {
			currentFrame = currentFrame.callerFrame();
			if (currentFrame == null) {
				break;
			}
			frameItem = currentFrame.getStackItem();
		}
		while (currentFrame != null) {
			final item = currentFrame.getStackItem();
			if (item != null) {
				chainFrames.push(item);
			}
			currentFrame = currentFrame.callerFrame();
		}

		switch (frameItem) {
			case null:
				return exception;
			case ClassFunction(_, _, _, _, _) | LocalFunction(_, _, _, _):
				for (item in exception.stack.asArray()) {
					switch (item) {
						case FilePos(Method(_, "invokeResume"), _) if (!seenInvokeResume):
							seenInvokeResume = true;
							stack.push(item);
							localInsertIndex = stack.length;
						case FilePos(Method(_, "invokeResume"), _):
						case FilePos(Method("hxcoro.CoroRun", "run"), _):
						case _ if (itemMatchesAnyCoroFrame(item, chainFrames)):
							if (!skippedChainFrame) {
								skippedChainFrame = true;
								localInsertIndex = stack.length;
							}
						case _:
							stack.push(item);
					}
				}
			case _:
				return exception;
		}
		exception.stack = stack;
		// Use negative value to signal that the immediate buildCallStack call
		// (from the catching continuation's exception handler) should be skipped.
		setInsertIndex(exception, -(localInsertIndex + 1));
		#end
		return exception;
	}

	public function buildCallStack(cont:BaseContinuation<Any>):Void {
		#if js
		return;
		#end
		#if debug
		final error = cont.error;
		var idx = getInsertIndex(error);
		if (idx == null) {
			return;
		}

		if (idx < 0) {
			// First buildCallStack call after startException — this is the catching
			// continuation itself. Skip inserting its (stale) stack item, but decode
			// and store the real insert index for subsequent calls.
			setInsertIndex(error, -(idx + 1));
			return;
		}

		final frameItem = cont.getStackItem();
		if (frameItem != null) {
			final stackItem = toStackItem(frameItem);
			final stack = error.stack.asArray();
			stack.insert(idx, stackItem);
			error.stack = stack;
			setInsertIndex(error, idx + 1);
		}
		#end
	}
}
