package cases;

class Issue7029 extends DisplayTestCase {
	/**
	class Main implements {-1-} {
		static public function main() {

		}
	}

	interface IFoo { }
	**/
	function test() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "Main"));
	}
}
