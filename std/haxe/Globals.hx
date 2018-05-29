package haxe;

import haxe.extern.Rest;

extern class Globals {
	/**
		Outputs the given `arguments`.

		On most targets, a call to this function is rewritten by the compiler as a
		call to `haxe.Log.trace` with the appropriate arguments. If there is only
		a single argument, some targets may use a native outputting function instead.
	**/
	static public function trace(arguments:Rest<Dynamic>):Void;
}