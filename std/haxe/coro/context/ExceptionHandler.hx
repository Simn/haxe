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

	abstract public function startSynchronousRun(context:Context, p:PosInfos):SynchronousRun;

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

private class StartedException {
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

private class SynchronousRun implements IElement<SynchronousRun> {
	public static final key = new Key<SynchronousRun>('SynchronousRun');

	public final context:Context;

	final entryPos:PosInfos;
	final thrownException:Tls<StartedException>;

	public function new(context:Context, entryPos:PosInfos) {
		this.context = context.with(this);
		this.entryPos = entryPos;
		thrownException = new Tls();
	}

	public function startException(cont:BaseContinuation<Any>, exception:Exception) {
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
		final exception = thrownException.value;
		if (exception == null || exception.coroStack.length == 0) {
			return;
		}
		thrownException.value = null;

		exception.dump();

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

		for (item in exceptionStack) {
			switch (item) {
				// TODO: More patterns probably
				case FilePos(StackItem.Method(_, "invokeResume"), file, line, column):
					patchFirstCoroStack(file, line, column);
					break;
				case _:
					newStack.push(item);
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

		exception.exception.stack = newStack;
	}

	public function complete() {
		// Clear the exception to avoid keeping unnecessary references around.
		thrownException.value = null;
	}

	public function getKey() {
		return key;
	}
}

/**
	The default `ExceptionHandler` implementation, which reconstructs the coroutine call stack
	from the continuation chain and inserts it into the exception's stack trace.
**/
class DefaultExceptionHandler extends ExceptionHandler {
	public function new() {

	}

	public function startSynchronousRun(context:Context, p:PosInfos) {
		return new SynchronousRun(context, p);
	}

	public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception {
		#if js
		return exception;
		#end
		return cont.context.get(SynchronousRun).startException(cont, exception);
	}

	public function buildCallStack(cont:BaseContinuation<Any>):Void {
		#if js
		return;
		#end
		cont.context.get(SynchronousRun).buildCallStack(cont);
	}
}
