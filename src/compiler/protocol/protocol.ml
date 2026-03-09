open ParsedArg

(** Protocol version constants for the [--connect] socket communication channel.

    The client selects a version by including [-D haxe.protocol-version=N] in
    its arguments.  The server reads that define and encodes its response in the
    matching format.

    Version 1 — legacy newline-framed text:
    - [\x01<content>\n]: stdout chunk (newlines within content encoded as [\x01])
    - [\x02\n]: error flag sentinel
    - [<text>\n]: plain stderr line

    Version 2 — binary length-prefixed frames:
    - Frame format: [1 byte tag][4 bytes big-endian uint32 length][payload bytes]
    - Tag [0x01]: stdout chunk (raw bytes, no escaping needed)
    - Tag [0x02]: stderr chunk (raw bytes)
    - Tag [0x03]: error flag (empty payload)

    Stdin data is forwarded as raw bytes in both protocols, appended after the
    null-terminated argument string. *)

let version_legacy  = 1
let version_current = 2

(** Tag bytes for protocol v2 binary frames (server → client).

    [tag_stdout], [tag_stderr], and [tag_error] can be sent at any time during
    a request; they carry streaming output.  [tag_done] is sent exactly once,
    at the end of every request, to signal completion.  Clients wait for
    [tag_done] rather than a length prefix, which is what enables streaming. *)
let tag_stdout = 0x01
let tag_stderr = 0x02
let tag_error  = 0x03
let tag_done   = 0x04

(** Extract the requested protocol version from a pre-parsed argument list.
    Looks for [Define ("haxe.protocol-version", Some n)]; hyphens in the key are
    normalized to underscores to match [Common.convert_define].
    Returns [version_legacy] if no matching define is present. *)
let detect_version (args : parsed_arg list) =
	let target_key = "haxe.protocol_version" in
	let normalize k = String.concat "_" (ExtString.String.nsplit k "-") in
	List.fold_left (fun acc arg ->
		match arg with
		| Define (k, Some v) when normalize k = target_key ->
			(try int_of_string v with _ -> acc)
		| _ -> acc
	) version_legacy args

(** Serialize a single v2 frame: [tag:1][big-endian length:4][payload:N].
    Combines header and payload into one allocation to keep them in a
    single TCP segment. *)
let make_frame tag payload =
	let len = String.length payload in
	let buf = Bytes.create (5 + len) in
	Bytes.set buf 0 (Char.chr tag);
	Bytes.set buf 1 (Char.chr ((len lsr 24) land 0xFF));
	Bytes.set buf 2 (Char.chr ((len lsr 16) land 0xFF));
	Bytes.set buf 3 (Char.chr ((len lsr  8) land 0xFF));
	Bytes.set buf 4 (Char.chr ( len         land 0xFF));
	Bytes.blit_string payload 0 buf 5 len;
	Bytes.unsafe_to_string buf

(** Send all bytes in [str] to [sock], retrying on partial sends. *)
let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (Bytes.length str)
