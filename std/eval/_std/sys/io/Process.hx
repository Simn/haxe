/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package sys.io;

import haxe.io.Input;
import haxe.io.Output;

@:coreApi
class Process {
	@:ifFeature("sys.io.Process.*") public var stdout(default, null):haxe.io.Input;
	@:ifFeature("sys.io.Process.*") public var stderr(default, null):haxe.io.Input;
	@:ifFeature("sys.io.Process.*") public var stdin(default, null):haxe.io.Output;

	@:ifFeature("sys.io.Process.*") var running:Bool;
	@:ifFeature("sys.io.Process.*") var pid:Int;
	@:ifFeature("sys.io.Process.*") var code:Int;

	extern public function new(cmd:String, ?args:Array<String>, ?detached:Bool):Void;

	extern public function close():Void;

	public function getPid():Int {
		return pid;
	}

	public function exitCode(?block:Bool = true):Int {
		if (block) {
			wait();
		}
		return code;
	}

	extern public function kill():Void;

	extern function wait():Void;
}
