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

package eval.vm;

import sys.io.FileInput;
import sys.io.FileOutput;

enum abstract ProcessStatusKind(Int) {
	var WEXITED;
	var WSIGNALED;
	var WSTOPPED;
}

typedef ProcessStatus = {
	final kind:ProcessStatusKind;
	final code:Int;
}

/**
	Experimental Process API.
**/
class Process {
	@:keep public final stdout:FileInput;
	@:keep public final stdin:FileOutput;
	@:keep public final stderr:FileInput;

	@:keep final pid:Int;

	/**
		Creates a new process that executes `command` with the given `arguments`.
	**/
	extern public function new(command:String, arguments:Array<String>):Void;

	extern public function close():ProcessStatus;

	public function getPid() {
		return pid;
	}

	extern public function kill():Void;
}
