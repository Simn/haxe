package cases;

class Issue7046 extends DisplayTestCase {
	/**
	import Ar{-1-}ray;
	using Ar{-2-}ray;
	**/
	function test() {
		eq("Class<Array>", type(pos(1)));
		eq("Class<Array>", type(pos(2)));
	}
}