package unit.issues;

class Issue5074 extends Test {
	@:keep
	function dontRunMe() {
		var a:Array<Int> = null;
		a.concat(null);
	}
}