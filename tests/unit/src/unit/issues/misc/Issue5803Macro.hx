package unit.issues.misc;

class Issue5803Macro {
	macro static public function getFunction() {
		return macro function (a) return 'a: $a';
	}
}