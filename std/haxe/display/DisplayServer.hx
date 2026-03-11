import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.display.Protocol;
import haxe.io.Bytes;
import haxe.io.BytesOutput;
import hxcoro.Coro.*;
import hxcoro.CoroRun;
import sys.io.Process;
import sys.net.Host;
import sys.net.Socket;
import sys.thread.Deque;

// ---------------------------------------------------------------------------
// Request result – no dependency on any external library
// ---------------------------------------------------------------------------
typedef RequestResult = {
	/** True if the server signalled a compilation error (TAG_ERROR frame). */
	var hasError:Bool;
	/** Accumulated stdout (display JSON-RPC responses arrive here in v2). */
	var stdout:String;
	/** Accumulated stderr (compiler messages arrive here in v2). */
	var stderr:String;
}

// ---------------------------------------------------------------------------
// Pending request placed on the deque by a suspended coroutine
// ---------------------------------------------------------------------------
private typedef PendingRequest = {
	arguments:Array<String>,
	stdin:Null<Bytes>,
	/** Called on the I/O thread as each stdout frame (TAG_STDOUT) arrives. */
	?onStdout:String->Void,
	/** Called on the I/O thread as each stderr frame (TAG_STDERR) arrives. */
	?onStderr:String->Void,
	cont:IContinuation<RequestResult>,
}

// ---------------------------------------------------------------------------
// CoroHaxeServer
//
// Replicates the behaviour of HaxeServerProcessNode for sys targets:
//
//   1. Creates a TCP server socket bound to port 0 (OS assigns the port).
//   2. Spawns `command` with `--server-connect 127.0.0.1:<port>` so the
//      Haxe process connects back to us.
//   3. All blocking I/O runs on a dedicated background thread so the
//      coroutine event-loop is never stalled.
//
// Wire protocol (Haxe 5, protocol v2 via `mk_streaming_communication`):
//
//   Client → socket : [LE int32 payload-len] [arg1\narg2\n…]
//                     optionally followed by [0x01][stdin-bytes]
//
//   Server → socket : streaming binary frames, terminated by TAG_DONE:
//     [tag : 1 byte][length : 4 bytes BE uint32][payload : length bytes]
//     TAG_STDOUT (0x01) – stdout chunk  (display JSON-RPC response)
//     TAG_STDERR (0x02) – stderr chunk  (compiler messages, warnings)
//     TAG_ERROR  (0x03) – error flag    (empty payload)
//     TAG_DONE   (0x04) – end-of-request sentinel
//
// Protocol v2 is activated by prepending `-D haxe.protocol-version=2` to
// each request's argument list (done automatically by this class).
// ---------------------------------------------------------------------------
class CoroHaxeServer {
	/** Requests queued by suspended coroutines; null sentinel = shut down. */
	final requestDeque:Deque<Null<PendingRequest>>;

	public function new(command:String, arguments:Array<String>) {
		requestDeque = new Deque();

		// Bind a server socket on an OS-assigned port
		final server = new Socket();
		server.bind(new Host("127.0.0.1"), 0);
		server.listen(1);
		final port = server.host().port;
		trace('Listening on port $port');

		// Spawn the Haxe compiler, telling it to connect back to us
		final proc = new Process(command, arguments.concat(["--server-connect", '127.0.0.1:$port']));

		sys.thread.Thread.create(() -> {
			// Accept Haxe's single inbound connection (blocking)
			final conn = server.accept();
			server.close();
			trace("Haxe connected!");

			// I/O worker loop – processes one request at a time
			while (true) {
				final req = requestDeque.pop(true);
				if (req == null) {
					conn.close();
					proc.close();
					return;
				}
				try {
					final result = doRequest(conn, req.arguments, req.stdin, req.onStdout, req.onStderr);
					req.cont.resume(result, null);
				} catch (e:Exception) {
					req.cont.resume(null, e);
				}
			}
		});
	}

	// -----------------------------------------------------------------------
	// Internal I/O – runs on the worker thread
	// -----------------------------------------------------------------------

	// Protocol v2 frame tags (server → client)
	static inline final TAG_STDOUT = 0x01; // stdout chunk  (display JSON responses)
	static inline final TAG_STDERR = 0x02; // stderr chunk  (compiler messages)
	static inline final TAG_ERROR  = 0x03; // error flag    (empty payload)
	static inline final TAG_DONE   = 0x04; // end-of-request sentinel

	static function doRequest(
		conn:Socket,
		arguments:Array<String>,
		stdin:Null<Bytes>,
		onStdout:Null<String->Void>,
		onStderr:Null<String->Void>
	):RequestResult {
		// Prepend the protocol-version define so the server switches to v2
		// tagged-frame output.  The server detects it via Protocol.detect_version
		// and calls conn.set_version(2) before sending any response frames.
		final args = ["-D", "haxe.protocol-version=2"].concat(arguments);

		// Build and send request payload:  [LE int32 length][arg1\narg2\n…]
		// optionally followed by [0x01][stdin bytes]
		final payload = new BytesOutput();
		for (arg in args) {
			payload.writeString(arg);
			payload.writeByte("\n".code);
		}
		if (stdin != null) {
			payload.writeByte(0x01);
			payload.write(stdin);
		}
		final msgBytes = payload.getBytes();
		conn.output.writeInt32(msgBytes.length); // LE int32 (extlib IO default)
		conn.output.write(msgBytes);
		conn.output.flush();

		// Read streaming v2 frames until TAG_DONE:
		//   [tag : 1 byte][length : 4 bytes BE uint32][payload : length bytes]
		// Every frame (including TAG_DONE) has the 4-byte length field.
		final stdoutBuf = new StringBuf();
		final stderrBuf = new StringBuf();
		var hasError = false;

		while (true) {
			final tag = conn.input.readByte();
			final len = readBeUint32(conn.input);        // always present
			if (tag == TAG_DONE) break;                  // zero-length payload

			final chunk = len > 0 ? conn.input.read(len).toString() : "";

			switch tag {
				case TAG_STDOUT:
					stdoutBuf.add(chunk);
					if (onStdout != null) onStdout(chunk);
				case TAG_STDERR:
					stderrBuf.add(chunk);
					if (onStderr != null) onStderr(chunk);
				case TAG_ERROR:
					hasError = true;
				case t:
					throw new Exception('Unknown v2 frame tag: $t');
			}
		}

		return {
			hasError: hasError,
			stdout: stdoutBuf.toString(),
			stderr: stderrBuf.toString(),
		};
	}

	/** Reads a big-endian unsigned 32-bit integer from `input`. */
	static function readBeUint32(input:haxe.io.Input):Int {
		final b0 = input.readByte();
		final b1 = input.readByte();
		final b2 = input.readByte();
		final b3 = input.readByte();
		return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
	}

	// -----------------------------------------------------------------------
	// Public coroutine API
	// -----------------------------------------------------------------------

	/**
	 * Sends a request to the Haxe server and suspends until all response
	 * frames have been received (TAG_DONE).
	 *
	 * `onStdout` and `onStderr` are optional streaming callbacks invoked on
	 * the I/O thread as individual frames arrive, *before* the coroutine is
	 * resumed.  Enables eager progress display or feeding a `Deque`.
	 */
	@:coroutine public function request(
		arguments:Array<String>,
		?stdin:Bytes,
		?onStdout:String->Void,
		?onStderr:String->Void
	):RequestResult {
		return suspend(cont -> {
			requestDeque.push({
				arguments: arguments,
				stdin: stdin,
				onStdout: onStdout,
				onStderr: onStderr,
				cont: cont,
			});
		});
	}

	/** Shuts down the I/O worker thread and closes the Haxe process. */
	public function close():Void {
		requestDeque.push(null);
	}
}

// ---------------------------------------------------------------------------
// Typed display-protocol helper
//
// In v2, the Haxe server routes the JSON-RPC response through `io.print_err`
// → `comm.write_err` → TAG_STDERR frames, so `result.stderr` holds the JSON.
// Compiler messages (warnings, errors) also arrive via TAG_STDERR.
// TAG_STDOUT frames carry any direct stdout output from compilation (rare for
// display requests).
//
// The optional `onStderr` callback is forwarded to `server.request`, so
// callers can eagerly stream TAG_STDERR frames on the I/O thread as they
// arrive, before the coroutine is resumed.
// ---------------------------------------------------------------------------
@:coroutine function displayRequest<TParams, TResponse>(
		server:CoroHaxeServer,
		method:HaxeRequestMethod<TParams, TResponse>,
		params:TParams,
		?id:Int,
		?onStderr:String->Void):TResponse {
	final json = haxe.Json.stringify({
		jsonrpc: "2.0",
		id: id ?? 1,
		method: (method : String),
		params: params,
	});
	final raw = server.request(["--display", json], null, null, onStderr);
	final responseText = StringTools.trim(raw.stderr);
	if (responseText == "") {
		throw new Exception('No display response (stderr empty). stdout:\n${raw.stdout}');
	}
	// The server returns a JSON-RPC envelope: {jsonrpc, id, result: TResponse}.
	final envelope:{result:TResponse, ?error:Dynamic} = haxe.Json.parse(responseText);
	if (envelope.error != null) {
		throw new Exception('Display error: ${haxe.Json.stringify(envelope.error)}');
	}
	return envelope.result;
}