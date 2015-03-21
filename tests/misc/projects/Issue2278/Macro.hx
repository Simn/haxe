import haxe.macro.Expr;

class Macro {
	macro static public function call(efun:ExprOf<Int->Int>, eval):Expr {
		var vfun = untyped $toValue(efun);
		var vval = untyped $toValue(eval);
		var r = vfun(vval);
		return macro $v{r};
	}
}