package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import runci.targets.Cs.*;
import runci.targets.Python.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["--times", "compile-macro.hxml", "--hxb", "bin/unit.hxb", "-D", "hxb-texpr-pos=relative"].concat(args));
		runCommand("haxe", ["--times", "--run", "bin/unit.hxb"]);

		changeDirectory(displayDir);
		runCommand("haxe", ["build.hxml"]);

		changeDirectory(miscDir);
		getCsDependencies();
		getPythonDependencies();
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-macro.hxml", "--hxb", "bin/sys.hxb"]);
		runCommand("haxe", ["--run", "bin/sys.hxb"]);
		runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);
	}
}