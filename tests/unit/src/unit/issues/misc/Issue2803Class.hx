package unit.issues.misc;

class Issue2803Class {
	@:op(A * B)
	static inline function stringTimesInt(lhs:String, rhs:Int) {
		return [for (i in 0...rhs) lhs].join("");
	}

	@:op(A => B)
	@:commutative
	static inline function doSomethingWeird<T:Bool>(lhs:String, rhs:T) {
		return lhs + ": " + rhs;
	}
}