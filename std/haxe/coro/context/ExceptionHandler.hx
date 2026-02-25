package haxe.coro.context;

import haxe.CallStack.StackItem;
import haxe.Exception;
import haxe.coro.BaseContinuation;
import haxe.coro.CoroStackItem;

/**
	An abstract context element that handles exception stack trace management for coroutines.
	`BaseContinuation.startException` and `BaseContinuation.buildCallStack` delegate to this element.

	The default implementation is `DefaultExceptionHandler`.
**/
abstract class ExceptionHandler implements IElement<ExceptionHandler> {
	public static final key = new Key<ExceptionHandler>('ExceptionHandler');

	abstract public function registerSynchronousEntrypoint(p:PosInfos):Void;

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

class StartedException {
	public final exception:Exception;
	public final coroStack:Array<CoroStackItem>;

	public function new(exception:Exception, coroStack:Array<CoroStackItem>) {
		this.exception = exception;
		this.coroStack = coroStack;
	}

	#if sys
	public function dump() {
		Sys.println("Exception stack:");
		for (item in exception.stack.asArray()) {
			Sys.print("\t");
			Sys.println(item);
		}
		Sys.println("Coro stack:");
		for (item in coroStack) {
			Sys.print("\t");
			Sys.println(item);
		}
	}
	#end
}

/**
	The default `ExceptionHandler` implementation, which reconstructs the coroutine call stack
	from the continuation chain and inserts it into the exception's stack trace.
**/
class DefaultExceptionHandler extends ExceptionHandler {
	final thrownException:Tls<StartedException>;
	final syncEntrypoints:Tls<Array<Array<StackItem>>>;

	public function new() {
		thrownException = new Tls();
		syncEntrypoints = new Tls();
	}

	public function registerSynchronousEntrypoint(p:PosInfos) {
		if (syncEntrypoints.value == null)
			syncEntrypoints.value = [];
		syncEntrypoints.value.push(CallStack.callStack());
	}

	public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception {
		#if js
		return exception;
		#end

		var frameItem = cont.getStackItem();
		if (frameItem == null) {
			// If we have no frane item on our continuation, just bail.
			return exception;
		}

		// Collect coro frames from the continuation chain.
		var chainFrames = [];
		var currentFrame:Null<IStackFrame> = cont;
		while (currentFrame != null) {
			final item = currentFrame.getStackItem();
			if (item != null) {
				chainFrames.push(item);
			}
			currentFrame = currentFrame.callerFrame();
		}

		thrownException.value = new StartedException(exception, chainFrames);
		return exception;
	}

	public function buildCallStack(cont:BaseContinuation<Any>):Void {
		#if js
		return;
		#end

		final exception = thrownException.value;
		if (exception == null || exception.coroStack.length == 0) {
			return;
		}
		thrownException.value = null;

		final newStack = [];
		final coroStack = exception.coroStack;
		final exceptionStack = exception.exception.stack.asArray();

		function patchFirstCoroStack(file:String, line:Int, column:Int) {
			switch (coroStack[0]) {
				case ClassFunction(cls, func, _, _, _):
					coroStack[0] = ClassFunction(cls, func, file, line, column);
				case LocalFunction(id, _, _, _):
					coroStack[0] = LocalFunction(id, file, line, column);
				case PosInfo(_):
			}
		}

		var foundInvokeResume = false;
		for (item in exceptionStack) {
			switch (item) {
				// TODO: More patterns probably
				case FilePos(StackItem.Method(_, "invokeResume"), file, line, column):
					patchFirstCoroStack(file, line, column);
					foundInvokeResume = true;
					break;
				case _:
					newStack.push(item);
			}
		}

		// If no invokeResume was found in the exception stack (nested coro scenario),
		// try to use the captured synchronous entrypoint stack to:
		// 1. Patch the first coro stack item position
		// 2. Insert the synchronous call chain between the two coro worlds
		if (!foundInvokeResume) {
			final entrypoints = syncEntrypoints.value;
			if (entrypoints != null && entrypoints.length > 0) {
				final captured = entrypoints.pop();
				final syncFrames = [];
				var pastFramework = false;
				var skippedFirst = false;
				for (frame in captured) {
					switch (frame) {
						case FilePos(StackItem.Method(_, "invokeResume"), file, line, column):
							patchFirstCoroStack(file, line, column);
							break;
						case FilePos(StackItem.Method(cls, _), _, _)
							if (!pastFramework && (cls.indexOf("haxe.coro.") == 0 || cls.indexOf("hxcoro.") == 0)):
							// Skip framework-internal frames before user code
							continue;
						case _:
							pastFramework = true;
							// Skip the first user frame — it's the CoroRun.run call site,
							// already represented by the PosInfo entry from the inner coro stack.
							if (!skippedFirst) {
								skippedFirst = true;
								continue;
							}
							syncFrames.push(frame);
					}
				}
				// Remove trailing PosInfo-generated entry from the inner coro stack
				// (the "coro" method entry) since it duplicates the lambda entry and
				// is superseded by the captured sync frames.
				if (newStack.length > 0) {
					switch (newStack[newStack.length - 1]) {
						case FilePos(StackItem.Method(_, "coro"), _, _):
							newStack.pop();
						case _:
					}
				}
				for (frame in syncFrames) {
					newStack.push(frame);
				}
			}
		}

		for (frame in coroStack) {
			switch (frame) {
				case ClassFunction(cls, func, file, line, column):
					newStack.push(StackItem.FilePos(StackItem.Method(cls, func), file, line, column));
				case LocalFunction(id, file, line, column):
					newStack.push(StackItem.FilePos(StackItem.LocalFunction(id), file, line, column));
				case PosInfo(p):
					newStack.push(StackItem.FilePos(StackItem.Method(p.className, "coro"), p.fileName, p.lineNumber));
			}
		}

		// Append the bottom stack — the synchronous call chain that brought us
		// into the coroutine world. This comes from the outermost sync entrypoint.
		final entrypoints = syncEntrypoints.value;
		if (entrypoints != null && entrypoints.length > 0) {
			final bottomCaptured = entrypoints[0];
			var pastFramework = false;
			var skippedFirst = false;
			for (frame in bottomCaptured) {
				switch (frame) {
					case FilePos(StackItem.Method(_, "invokeResume"), _, _, _):
						break;
					case FilePos(StackItem.Method(cls, _), _, _)
						if (!pastFramework && (cls.indexOf("haxe.coro.") == 0 || cls.indexOf("hxcoro.") == 0)):
						continue;
					case _:
						pastFramework = true;
						// Skip the first user frame — it's the entrypoint call site,
						// already represented by the last PosInfo entry in the coro stack.
						if (!skippedFirst) {
							skippedFirst = true;
							continue;
						}
						newStack.push(frame);
				}
			}
			// Clear entrypoints since we've consumed them
			syncEntrypoints.value = null;
		}

		exception.exception.stack = newStack;
	}
}
