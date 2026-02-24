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

	`insertIndex` is stored in thread-local storage, making this implementation safe for
	concurrent use across multiple coroutines running on different threads.
**/
class DefaultExceptionHandler extends ExceptionHandler {
	#if debug
	final insertIndexByException = new haxe.coro.Tls<ObjectMap<Exception, Int>>();
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

	#if debug
	inline function getInsertIndexByException() {
		if (insertIndexByException.value == null) {
			insertIndexByException.value = new ObjectMap();
		}
		return insertIndexByException.value;
	}
	#end

	public function new() {}

	public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception {
		#if js
		return exception;
		#end
		#if debug
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
		getInsertIndexByException().set(exception, localInsertIndex);
		#end
		return exception;
	}

	public function buildCallStack(cont:BaseContinuation<Any>):Void {
		#if js
		return;
		#end
		#if debug
		final error = cont.error;
		final insertIndexByException = getInsertIndexByException();
		final idx = insertIndexByException.get(error);
		if (idx == null) {
			return;
		}

		final frameItem = cont.getStackItem();
		if (frameItem != null) {
			final stackItem = toStackItem(frameItem);
			final stack = error.stack.asArray();
			stack.insert(idx, stackItem);
			error.stack = stack;
			insertIndexByException.set(error, idx + 1);
		}
		if (cont.callerFrame() == null) {
			insertIndexByException.remove(error);
		}
		#end
	}
}
