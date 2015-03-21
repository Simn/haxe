using Macro;

class Main {
	static function main() {
		write(Macro.call(function(x) return x * 3, 3));
		write(Macro.call(MyTools.double, 3));
		write(MyTools.double.call(3));
		write(Macro.call(MyTools.double, Macro.call(MyTools.double, 3)));
	}

	static function write(i:Int) {
		Sys.stderr().writeString(i + "\n");
	}
}