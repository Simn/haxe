package cases;

class Issue7029 extends DisplayTestCase {
	/**
	class C implements {-1-}

	interface IFoo { }
	**/
	function test1() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "C"));
	}

	/**
	class C1 extends {-1-}

	class C2 { }
	interface IFoo { }
	**/
	function test2() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "C2"));
		eq(false, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "C1"));
	}

	/**
	class C { }
	interface IFoo { }
	interface IFoo2 extends {-1-} { }
	**/
	function test3() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "IFoo"));
		eq(false, hasToplevel(typesCompletion, "type", "C"));
		eq(false, hasToplevel(typesCompletion, "type", "IFoo2"));
	}

	/**
	typedef T1 = { };
	class C1 { }

	typedef T2 = {
		> {-1-}
	}
	**/
	function test4() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "T1"));
		eq(false, hasToplevel(typesCompletion, "type", "C1"));
	}

	/**
	typedef T1 = { };
	class C1 { }

	typedef T2 = {
		> T{-1-}
	}
	**/
	function test5() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "T1"));
		eq(false, hasToplevel(typesCompletion, "type", "C1"));
	}

	/**
	typedef T1 = { };
	typedef T2 = { };
	class C1 { }

	typedef T3 = {
		> T1,
		> {-1-}
	}
	**/
	function test6() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "T2"));
		eq(false, hasToplevel(typesCompletion, "type", "C1"));
	}
}
