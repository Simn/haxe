package cases;

class Issue7060 extends DisplayTestCase {
	/**
	import Type.Valu{-1-}eType;

	class Main {
		static function main() {}
	}
	**/
	function test() {
		eq("Enum<ValueType>", type(pos(1)));
	}
}