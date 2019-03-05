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

import sys.io.Process;
import haxe.io.BytesBuffer;
import haxe.io.Bytes;

/**
	Wrapper class for `Process` that reads stdout and stderr in threads.

	Use this if you need a blocking-process but run the risk of full pipes.
**/
class BufferedProcess {
	var process:sys.io.Process;
	var stdoutBuffer:BytesBuffer;
	var stderrBuffer:BytesBuffer;
	var stdoutThread:Thread;
	var stderrThread:Thread;

	/**
		Creates a new process that executes `command` with the given `arguments`. If
		`stdin` is provided, its contents are sent to the processes stdin before closing
		it.

		Call `close` to obtain the contents of stdout, stderr as well as the process status.
	**/
	@:access(sys.io.Process.running)
	public function new(command:String, arguments:Array<String>, ?stdin:Bytes) {
		stdoutBuffer = new BytesBuffer();
		stderrBuffer = new BytesBuffer();

		process = new Process(command, arguments);

		if (stdin != null) {
			process.stdin.write(stdin);
			process.stdin.close();
		}

		stderrThread = new Thread(function() {
			while (process.running) {
				stderrBuffer.add(process.stderr.readAll());
			}
			process.stderr.close();
		});

		stdoutThread = new Thread(function() {
			while (process.running) {
				stdoutBuffer.add(process.stdout.readAll());
			}
			process.stdout.close();
		});
	}

	/**
		Waits for the process to finish running and returns its status, alongside
		the contents of stdout and stderr.
	**/
	public function close() {
		process.close();
		Thread.join(stdoutThread);
		Thread.join(stderrThread);
		return {
			exit: process.exitCode(),
			stdout: stdoutBuffer.getBytes(),
			stderr: stderrBuffer.getBytes()
		}
	}
}
