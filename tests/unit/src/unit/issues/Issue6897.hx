package unit.issues;

class Issue6897 extends unit.Test {
	function test() {
		var inChars   = 'àáâãäçèéêëìíîïñòóôõöùúûüýÿ'.split('');
		eq(26, inChars.length);
	}
}