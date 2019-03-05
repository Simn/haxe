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
import haxe.io.Bytes;
import haxe.io.BytesBuffer;

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

	/**
		True while the process is running, false otherwise.
	**/
	@:keep public final running:Bool;

	@:keep final pid:Int;

	/**
		Creates a new process that executes `command` with the given `arguments`.
	**/
	extern public function new(command:String, arguments:Array<String>):Void;

	/**
		Waits for the process to finish running and returns its status.
	**/
	extern public function close():ProcessStatus;

	/**
		Returns the process ID. This ID is determined when the process is started
		and is still available after it has finished running.
	**/
	public function getPid() {
		return pid;
	}

	/**
		Forces the process to stop.
	**/
	extern public function kill():Void;
}

/**
	Wrapper class for `Process` that reads stdout and stderr in threads.

	Use this if you need a blocking-process but run the risk of full pipes.
**/
class BufferedProcess {
	var process:Process;
	var stdoutBuffer:BytesBuffer;
	var stderrBuffer:BytesBuffer;

	/**
		Creates a new process that executes `command` with the given `arguments`. If
		`stdin` is provided, its contents are sent to the processes stdin before closing
		it.

		Call `close` to obtain the contents of stdout, stderr as well as the process status.
	**/
	public function new(command:String, arguments:Array<String>, ?stdin:Bytes) {
		stdoutBuffer = new BytesBuffer();
		stderrBuffer = new BytesBuffer();

		process = new Process(command, arguments);

		if (stdin != null) {
			process.stdin.write(stdin);
			process.stdin.close();
		}

		new Thread(function() {
			while (process.running) {
				stderrBuffer.add(process.stderr.readAll());
			}
		});

		new Thread(function() {
			while (process.running) {
				stdoutBuffer.add(process.stdout.readAll());
			}
		});
	}

	/**
		Waits for the process to finish running and returns its status, alongside
		the contents of stdout and stderr.
	**/
	public function close() {
		var exit = process.close();
		return {
			exit: exit,
			stdout: stdoutBuffer.getBytes(),
			stderr: stderrBuffer.getBytes()
		}
	}
}
