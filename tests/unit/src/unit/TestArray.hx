package unit;

class TestArray extends unit.Test {
	@:analyzer(ignore)
	public function testIssue2103() {
		var a:Array<Dynamic> = [1, 2, 3];
		var b:Array<Int> = cast a;
		b.push(4);
		t(a == b);
		eq(a, b);
		eq(4, a.length);
		eq(4, a[3]);
	}
}