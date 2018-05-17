package cases;

class Issue7029 extends DisplayTestCase {
	/**
	class Main implements {-1-} {
		static public function main() {

		}
	}

	interface IFoo { }
	**/
	function test1() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "Main"));
	}

	/**
	class Main extends {-1-} {
		static public function main() {

		}
	}

	class C { }
	interface IFoo { }
	**/
	function test2() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "C"));
		eq(false, hasToplevel(typesCompletion, "type", "IFoo"));
	}

	/**
	class C { }
	interface IFoo { }
	interface IFoo2 extends {-1-} { }
	**/
	function test3() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "Main"));
	}
}
