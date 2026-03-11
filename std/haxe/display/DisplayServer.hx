package haxe.display;

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
	/** True if the server signalled a compilation error (TAG_DONE status byte ≠ 0). */
	var hasError:Bool;

	/** Process stdout of the Haxe server (verbose messages: reusing, skipping, not-cached, etc.).
		These come from `print_endline` calls in `serverMessage.ml` when the server is started with `-v`. */
	var serverOutput:String;

	/** Accumulated TAG_LOG frames: compiler diagnostic messages (errors, warnings, info). */
	var log:String;

	/** Accumulated TAG_RESULT frames: display JSON-RPC response. */
	var response:String;

	/** Accumulated TAG_PRINT frames: trace/print output from compiled user code. */
	var prints:String;
}

// ---------------------------------------------------------------------------
// Pending request placed on the deque by a suspended coroutine
// ---------------------------------------------------------------------------
private typedef PendingRequest = {
	arguments:Array<String>,
	stdin:Null<Bytes>,
	/** Called on the I/O thread as each TAG_PRINT frame (trace/print output) arrives. */
	?onPrint:String->Void,
	/** Called on the I/O thread as each TAG_LOG frame (compiler diagnostics) arrives. */
	?onLog:String->Void,
	cont:IContinuation<RequestResult>,
}

enum abstract ProtocolTag(Int) {
	final TAG_PRINT  = 0x01; // trace/print output from compiled user code
	final TAG_LOG    = 0x02; // compiler diagnostic messages (errors, warnings, info)
	final TAG_RESULT = 0x03; // display JSON-RPC response
	final TAG_DONE   = 0x04; // end-of-request; payload: 1 byte (0x00 = ok, 0x01 = error)
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
//     TAG_PRINT  (0x01) – trace/print output from compiled user code
//     TAG_LOG    (0x02) – compiler diagnostic messages (errors, warnings, info)
//     TAG_RESULT (0x03) – display JSON-RPC response
//     TAG_DONE   (0x04) – end-of-request; payload: 1 byte (0x00 = ok, 0x01 = error)
//
// Protocol v2 is activated by prepending `-D haxe.protocol-version=2` to
// each request's argument list (done automatically by this class).
//
// Verbose server messages (reusing, skipping, etc.) from `print_endline` in
// `serverMessage.ml` go to the actual process stdout (not through the socket).
// These are captured by reading `proc.stdout` in a background thread and
// returned as `RequestResult.stdout`.
// ---------------------------------------------------------------------------
class CoroHaxeServer {
	/** Requests queued by suspended coroutines; null sentinel = shut down. */
	final requestDeque:Deque<Null<PendingRequest>>;

	/** Signalled by the I/O thread when shutdown is complete; makes close() synchronous. */
	final closedDeque:Deque<Bool>;

	/** Chunks of process stdout (verbose server messages). */
	final procStdoutDeque:Deque<Null<String>>;

	/** Arguments prepended to every request (e.g. `-D disable-hxb-cache`). */
	var defaultArguments:Array<String> = [];

	public function new(command:String, arguments:Array<String>) {
		requestDeque = new Deque();
		procStdoutDeque = new Deque();
		closedDeque = new Deque();

		// Bind a server socket on an OS-assigned port
		final server = new Socket();
		server.bind(new Host("127.0.0.1"), 0);
		server.listen(1);
		final port = server.host().port;

		// Spawn the Haxe compiler, telling it to connect back to us
		final proc = new Process(command, arguments.concat(["--server-connect", '127.0.0.1:$port']));

		// Background thread: read verbose messages from process stdout line by line.
		// These are `print_endline` calls in serverMessage.ml (reusing, skipping, etc.)
		// and go to the real process stdout rather than through the socket protocol.
		sys.thread.Thread.create(() -> {
			try {
				while (true)
					procStdoutDeque.push(proc.stdout.readLine());
			} catch (_:haxe.io.Eof) {
				procStdoutDeque.push(null); // EOF sentinel
			}
		});

		sys.thread.Thread.create(() -> {
			// Accept Haxe's single inbound connection (blocking)
			final conn = server.accept();
			server.close();

			// I/O worker loop – processes one request at a time
			while (true) {
				final req = requestDeque.pop(true);
				if (req == null) {
					conn.close();
					proc.kill();
					proc.close();
					closedDeque.push(true);
					return;
				}
				try {
					final result = doRequest(conn, procStdoutDeque, req.arguments, req.stdin, req.onPrint, req.onLog);
					req.cont.resume(result, null);
				} catch (e:Exception) {
					req.cont.resume(null, e);
				}
			}
		});
	}

	/** Sets arguments prepended to every request (e.g. `-D disable-hxb-cache`). */
	public function setDefaultRequestArguments(args:Array<String>):Void {
		defaultArguments = args;
	}

	static function doRequest(conn:Socket, procStdoutDeque:Deque<Null<String>>, arguments:Array<String>, stdin:Null<Bytes>, onPrint:Null<String->Void>,
			onLog:Null<String->Void>):RequestResult {
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
		// TAG_DONE carries a 1-byte status payload: 0x00 = ok, 0x01 = error.
		final printBuf = new StringBuf();
		final logBuf = new StringBuf();
		final responseBuf = new StringBuf();
		var hasError = false;

		while (true) {
			final tag:ProtocolTag = cast conn.input.readByte();
			final len = readBeUint32(conn.input); // always present
			final chunk = len > 0 ? conn.input.read(len).toString() : "";

			switch tag {
				case TAG_PRINT:
					printBuf.add(chunk);
					if (onPrint != null)
						onPrint(chunk);
				case TAG_LOG:
					logBuf.add(chunk);
					if (onLog != null)
						onLog(chunk);
				case TAG_RESULT:
					responseBuf.add(chunk);
				case TAG_DONE:
					hasError = chunk.length > 0 && chunk.charCodeAt(0) != 0;
					break;
				case t:
					throw new Exception('Unknown v2 frame tag: $t');
			}
		}

		// Drain process-stdout lines that arrived during this request.
		// The Haxe server writes verbose messages (reusing, skipping, etc.) to its
		// real stdout via `print_endline`. By the time TAG_DONE arrives, those writes
		// have already been flushed (io.close joins the background threads before
		// comm.close sends TAG_DONE), so whatever is in the deque now belongs to
		// this request.
		final procStdoutBuf = new StringBuf();
		var line:Null<String>;
		while ((line = procStdoutDeque.pop(false)) != null)
			procStdoutBuf.add(line + "\n");

		return {
			hasError: hasError,
			serverOutput: procStdoutBuf.toString(),
			log: logBuf.toString(),
			response: responseBuf.toString(),
			prints: printBuf.toString(),
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
	@:coroutine public function request(arguments:Array<String>, ?stdin:Bytes, ?onPrint:String->Void, ?onLog:String->Void):RequestResult {
		return suspend(cont -> {
			requestDeque.push({
				arguments: defaultArguments.concat(arguments),
				stdin: stdin,
				onPrint: onPrint,
				onLog: onLog,
				cont: cont,
			});
		});
	}

	/** Shuts down the I/O worker thread and closes the Haxe process.
		Blocks until the I/O thread has fully cleaned up. */
	public function close():Void {
		requestDeque.push(null);
		closedDeque.pop(true); // wait for I/O thread to finish
	}
}

// ---------------------------------------------------------------------------
// Typed display-protocol helper
//
// In v2, the Haxe server routes the JSON-RPC response through
// `io.print_result` → `comm.write_result` → TAG_RESULT frames, so
// `result.response` holds the JSON.  Compiler diagnostic messages (warnings,
// errors) arrive separately via TAG_LOG frames in `result.log`.
//
// The optional `onLog` callback is forwarded to `server.request`, so callers
// can eagerly stream TAG_LOG frames on the I/O thread as they arrive.
// ---------------------------------------------------------------------------
@:coroutine function displayRequest<TParams, TResponse>(server:CoroHaxeServer, method:HaxeRequestMethod<TParams, TResponse>, params:TParams, ?id:Int,
		?onLog:String->Void):TResponse {
	final json = haxe.Json.stringify({
		jsonrpc: "2.0",
		id: id ?? 1,
		method: (method : String),
		params: params,
	});
	final raw = server.request(["--display", json], null, null, onLog);
	final responseText = StringTools.trim(raw.response);
	if (responseText == "") {
		throw new Exception('No display response (response empty). prints:\n${raw.prints}');
	}
	// The server returns a JSON-RPC envelope: {jsonrpc, id, result: TResponse}.
	final envelope:{result:TResponse, ?error:Dynamic} = haxe.Json.parse(responseText);
	if (envelope.error != null) {
		throw new Exception('Display error: ${haxe.Json.stringify(envelope.error)}');
	}
	return envelope.result;
}
