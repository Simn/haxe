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
	// Kept as a named subclass for compatibility with hxcoro's public setup API.
	public function new() {
		super();
	}
}
