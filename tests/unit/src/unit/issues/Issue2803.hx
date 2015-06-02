package unit.issues;

using unit.issues.misc.Issue2803Class;

class Issue2803 extends Test {
	public function test() {
		eq("foofoofoo", "foo" * 3);
		t(unit.TestType.typeError(3 * "foo"));
		eq("value: true", "value" => true);
		eq("value: true", true => "value");
	}
}