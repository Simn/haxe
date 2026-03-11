import haxe.display.DisplayServer.CoroHaxeServer;
import utest.ui.Report;
import utest.Runner;
import utils.Vfs;

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");

		var runner = new Runner();
		runner.addCases('cases');
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		var cwd = Sys.getCwd();

		final server = new CoroHaxeServer("haxe", ["-v"]);
		var defaultArgs = [];
		#if disable_hxb_cache defaultArgs = defaultArgs.concat(["-D", "disable-hxb-cache"]); #end
		#if disable_hxb_optimizations defaultArgs = defaultArgs.concat(["-D", "disable-hxb-optimizations"]); #end
		#if optimistic_display_requests defaultArgs = defaultArgs.concat(["-D", "optimistic-display-requests"]); #end
		if (defaultArgs.length > 0) server.setDefaultRequestArguments(defaultArgs);

		TestCase.server = server;
		TestCase.rootCwd = cwd;
		runner.onComplete.add(_ -> server.close());
		runner.run();
	}
}
