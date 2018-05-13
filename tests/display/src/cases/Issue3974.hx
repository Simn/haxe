package cases;

class Issue3974 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			var s = "foo";
			var s2 = '\t${s.{-1-}}';
		}
	}

	class UserMove { }
	**/
	function testMark() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
