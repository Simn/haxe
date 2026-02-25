package haxe.coro.context;

import haxe.zip.Entry;
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

	public function new() {
		thrownException = new Tls();
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
		if (exception == null) {
			return;
		}
		thrownException.value = null;

		final newStack = [];
		final exceptionStack = exception.exception.stack.asArray();

		for (item in exceptionStack) {
			switch (item) {
				// TODO: More patterns probably
				case FilePos(StackItem.Method(_, "invokeResume"), _, _, _):
					break;
				case _:
					newStack.push(item);
			}
		}

		for (frame in exception.coroStack) {
			switch (frame) {
				case ClassFunction(cls, func, file, line, column):
					newStack.push(StackItem.FilePos(StackItem.Method(cls, func), file, line, column));
				case LocalFunction(id, file, line, column):
					newStack.push(StackItem.FilePos(StackItem.LocalFunction(id), file, line, column));
				case CoroEntrypoint:
			}
		}

		exception.exception.stack = newStack;
	}
}
