package unit.issues;

class Issue5803 extends unit.Test {
	function test() {
		eq("a: foo", unit.issues.misc.Issue5803Macro.getFunction()("foo"));
	}
}