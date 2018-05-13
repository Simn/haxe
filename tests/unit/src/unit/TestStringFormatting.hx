package unit;

class TestStringFormatting extends Test {
	function test() {
		eq("'", '${'\''}');
		var x = "a";
		eq("a", '${ '${x}'}' );
	}
}