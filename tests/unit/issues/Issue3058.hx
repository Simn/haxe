package unit.issues;

class Issue3058 extends Test {
	function test() {
		var node = Xml.createElement("foo");
		node.set("bar", '"value"');
	}
}