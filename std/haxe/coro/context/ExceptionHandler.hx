package haxe.coro.context;

import haxe.Exception;

class ExceptionHandler implements IElement<ExceptionHandler> {
	public static final key = new Key<ExceptionHandler>("ExceptionHandler");

	public function new() {}

	public function getKey():Key<ExceptionHandler> {
		return key;
	}

	public function onException(error:Exception):Void {
		trace(error);
	}
}

class DefaultExceptionHandler extends ExceptionHandler {
	public function new() {
		super();
	}
}
