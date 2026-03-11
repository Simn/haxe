(** Client-side socket receive loops for the two protocol variants.

    Used by [--connect] mode where the compiler process acts as a client,
    connecting to a running compilation server and decoding the response stream
    back to local stdout/stderr.

    Both loops start a background thread to forward local stdin to the server;
    they differ only in how they decode the server's response frames. *)

(** Spawn a background thread that forwards bytes from local stdin to [sock]
    until EOF, then half-closes the send side of the socket to signal EOF to
    the server.  Called by both [poll] and [poll_new]. *)
let start_stdin_forward_thread sock =
	let stdin_buf = Bytes.create 1024 in
	ignore (Thread.create (fun () ->
		(try
			let rec loop () =
				let n = Unix.read (Unix.descr_of_in_channel Stdlib.stdin) stdin_buf 0 1024 in
				if n = 0 then
					(try Unix.shutdown sock Unix.SHUTDOWN_SEND with _ -> ())
				else begin
					Protocol.ssend sock (Bytes.sub stdin_buf 0 n);
					loop ()
				end
			in
			loop ()
		with _ -> ())
	) ())

(** Protocol v1 (legacy) client receive loop.
    Buffers socket data and calls [print] for each complete newline-terminated
    line, without waiting for the connection to close. *)
let poll sock print =
	let response_buf = Buffer.create 0 in
	let flush_complete_lines () =
		let s = Buffer.contents response_buf in
		match String.rindex_opt s '\n' with
		| None -> ()
		| Some last_nl ->
			let complete  = String.sub s 0 (last_nl + 1) in
			let remaining = String.sub s (last_nl + 1) (String.length s - last_nl - 1) in
			let lines = ExtString.String.nsplit complete "\n" in
			let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
			List.iter print lines;
			Buffer.reset response_buf;
			if remaining <> "" then Buffer.add_string response_buf remaining
	in
	start_stdin_forward_thread sock;
	let sock_buf = Bytes.create 1024 in
	let sock_open = ref true in
	while !sock_open do
		let b = Unix.recv sock sock_buf 0 1024 [] in
		Buffer.add_subbytes response_buf sock_buf 0 b;
		if b <= 0 then
			sock_open := false
		else
			flush_complete_lines ()
	done;
	let s = Buffer.contents response_buf in
	if s <> "" then begin
		let lines = ExtString.String.nsplit s "\n" in
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines
	end

(** Protocol v2 (binary) client receive loop.
    Reads frames from the server until TAG_DONE is received.  Each frame
    carries a one-byte tag and a big-endian uint32 payload length.  Callbacks
    are invoked immediately on receipt of each complete frame, giving byte-level
    streaming with no newline-alignment requirement.
    Unknown tags are silently ignored for forward compatibility. *)
let poll_new sock ~on_print ~on_log ~on_result ~on_done =
	start_stdin_forward_thread sock;
	let read_exactly n =
		let buf = Bytes.create n in
		let rec loop pos =
			if pos = n then buf
			else
				let r = Unix.recv sock buf pos (n - pos) [] in
				if r = 0 then raise Exit
				else loop (pos + r)
		in
		loop 0
	in
	(try
		while true do
			let header = read_exactly 5 in
			let tag = Char.code (Bytes.get header 0) in
			let len =
				(Char.code (Bytes.get header 1) lsl 24) lor
				(Char.code (Bytes.get header 2) lsl 16) lor
				(Char.code (Bytes.get header 3) lsl  8) lor
				(Char.code (Bytes.get header 4))
			in
			let payload = if len > 0 then Bytes.unsafe_to_string (read_exactly len) else "" in
			if      tag = Protocol.tag_print   then on_print payload
			else if tag = Protocol.tag_log     then on_log payload
			else if tag = Protocol.tag_result  then on_result payload
			else if tag = Protocol.tag_done    then begin
				let has_error = len > 0 && Char.code (String.get payload 0) <> 0 in
				on_done has_error;
				raise Exit  (* orderly exit; caught by the with below *)
			end
		done
	with _ -> ())
