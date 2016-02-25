package unit.issues.misc;

class Issue2803Class {
	@:op(A * B)
	static public inline function stringTimesInt(lhs:String, rhs:Int) {
		return [for (i in 0...rhs) lhs].join("");
	}

	@:op(A => B)
	@:commutative
	static public inline function doSomethingWeird<T:Bool>(lhs:String, rhs:T) {
		return lhs + ": " + rhs;
	}
}