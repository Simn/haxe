package ;

class TestNullChecker extends TestBase {

	static function main() {
		new TestNullChecker();
	}

	public function new() {
		super();
		TestBaseMacro.run();
	}

	// Tests that the null analysis doesn't break basic null-flow patterns

	function testAssignment() {
		var ns = getNullString();
		ns = "foo";
		useString(ns);
	}

	function testReassignment() {
		var s = getString();
		useString(s);
		s = getNullString();
	}

	function testNullCheckThen() {
		var ns = getNullString();
		if (ns == null) {
			ns = getString();
		}
		useString(ns);
	}

	function testNullCheckNotNull() {
		var ns = getNullString();
		if (ns != null) {
			ns = getNullString();
		}
	}

	function testNullCheckElse() {
		var ns = getNullString();
		if (ns != null) {
			useString(ns);
		} else {
			ns = getString();
		}
		useString(ns);
	}

	function testNestedNullCheck() {
		var ns = getNullString();
		if (ns != null) {
			useString(ns);
		} else {
			if (ns == null) {
				ns = getString();
			}
		}
		useString(ns);
	}

	function testReturn1() {
		var ns = getNullString();
		if (ns == null) {
			return;
		}
		useString(ns);
	}

	function testReturn2() {
		var ns = getNullString();
		if (ns != null) {

		} else {
			return;
		}
		useString(ns);
	}

	function testBreak() {
		var ns = getNullString();
		while (true) {
			if (ns == null) {
				break;
			}
			useString(ns);
		}
	}

	function testContinue() {
		var ns = getNullString();
		while (true) {
			if (getTrue()) {
				break; // to terminate
			}
			if (ns == null) {
				continue;
			}
			useString(ns);
		}
	}

	function testThrow() {
		var ns = getNotNullString();
		if (ns == null) {
			throw false;
		}
		useString(ns);
	}

	function useString(s:String) {
		// Consume a non-null String value, ensuring the analysis tracks nullability correctly
	}

	function getString() {
		return "foo";
	}

	function getNullString():Null<String> {
		return null;
	}

	function getNotNullString():Null<String> {
		return "foo";
	}

	function getTrue() {
		return true;
	}

	function getFalse() {
		return false;
	}
}