open CompilationContext

(** Server-side IO plumbing: pipe-backed channels and background threads that
    forward compilation output and stdin through the socket communication channel.

    In server mode, real stdin/stdout/stderr cannot be used (they belong to the
    long-lived server process).  Instead we create Unix pipes: the compilation
    writes to one end, and background threads read from the other end and pass
    data to [comm.write_out] / [comm.write_err], which encode and send it to the
    client over the socket. *)

(** Read all available data from [channel] in 1024-byte chunks, calling [f] for
    each chunk.  Stops on EOF or a Unix error (e.g. pipe broken). *)
let rec read_content channel buf f =
	begin try
		let i = input channel buf 0 1024 in
		if i > 0 then begin
			f (Bytes.unsafe_to_string (Bytes.sub buf 0 i));
			read_content channel buf f
		end
	with Unix.Unix_error _ ->
		()
	end

(** Create a pipe whose write end is an [out_channel] and start a background
    thread that reads from the read end and passes each chunk to [write_fn].
    Returns [(out_channel, thread)].  The caller writes to [out_channel];
    [write_fn] receives the data asynchronously via the thread.

    Used to bridge OCaml [output_channel] writes (e.g. [Printf.printf] through
    [Format]) to the socket protocol encoder. *)
let make_output_pipe write_fn =
	let (r_fd, w_fd) = Unix.pipe ~cloexec:true () in
	let out_ch = Unix.out_channel_of_descr w_fd in
	let in_ch = Unix.in_channel_of_descr r_fd in
	let thread = Thread.create (fun () ->
		let buf = Bytes.create 1024 in
		(try while true do
			let n = input in_ch buf 0 1024 in
			if n = 0 then raise Exit;
			write_fn (Bytes.sub_string buf 0 n)
		done with
		| End_of_file | Exit -> ()
		| Unix.Unix_error _ -> ());
		close_in_noerr in_ch
	) () in
	(out_ch, thread)

(** Return the stdin [in_channel] for this compilation.
    If the client forwarded stdin data ([comm.stdin = Some ch]), use that channel.
    Otherwise create a pipe whose write end is immediately closed, so that any
    read on it returns EOF immediately. *)
let get_stdin_channel comm =
	match comm.stdin with
	| Some ch -> ch
	| None ->
		let (stdin_r_fd, stdin_w_fd) = Unix.pipe ~cloexec:true () in
		Unix.close stdin_w_fd;
		Unix.in_channel_of_descr stdin_r_fd

(** Pipe-based implementation of [getChar] for server mode.
    Reads one byte from [stdin_ch] and optionally echoes it to [stdout_ch].
    Returns [-1] on EOF, matching [Extc.getch]'s convention. *)
let getch_from_channel stdin_ch stdout_ch echo =
	let c = try
		int_of_char (input_char stdin_ch)
	with End_of_file ->
		-1
	in
	if echo && c >= 0 then begin
		output_char stdout_ch (char_of_int c);
		flush stdout_ch
	end;
	c

(** Build the {!Gctx.compilation_io} record for one compilation.

    In server mode ([comm.is_server = true]):
    - [stdout] and [stderr] are pipe-backed channels; background threads forward
      writes through [comm.write_out] / [comm.write_err] (the socket encoder).
    - [stdin] comes from the client's forwarded data, or an immediately-closed
      pipe when no stdin was provided.
    - [getch] reads from the stdin pipe instead of the real terminal.
    - [close] flushes both output channels and joins the background threads.

    In non-server mode the real process stdin/stdout/stderr are used directly. *)
let create_io comm =
	if comm.is_server then begin
		let (stdout_ch, stdout_thread) = make_output_pipe comm.write_out in
		let (stderr_ch, stderr_thread) = make_output_pipe comm.write_err in
		let stdin_ch = get_stdin_channel comm in
		let closed = ref false in
		{
			Gctx.print = comm.write_out;
			print_err = comm.write_err;
			print_result = comm.write_result;
			stdout = stdout_ch;
			stderr = stderr_ch;
			stdin = stdin_ch;
			getch = getch_from_channel stdin_ch stdout_ch;
			close = (fun () ->
				if not !closed then begin
					closed := true;
					flush stdout_ch; close_out_noerr stdout_ch; Thread.join stdout_thread;
					flush stderr_ch; close_out_noerr stderr_ch; Thread.join stderr_thread;
					close_in_noerr stdin_ch;
				end
			);
		}
	end else
		{
			Gctx.print = comm.write_out;
			print_err = comm.write_err;
			print_result = (fun s -> prerr_string s);
			stdout = Stdlib.stdout;
			stderr = Stdlib.stderr;
			stdin = Stdlib.stdin;
			getch = Extc.getch;
			close = (fun () -> ());
		}

(** Run a shell command in server mode, forwarding stdin from the client and
    capturing stdout/stderr through the socket protocol.
    Uses {!Process.run} so that the child's stdin is connected to the client's
    forwarded data; sending EOF when the client closes its end. *)
let run_command comm cmd =
	let proc = Process.run cmd None in
	let pout = Unix.in_channel_of_descr proc.Process.stdout_fd in
	let pin = Unix.out_channel_of_descr proc.Process.stdin_fd in
	let perr = Unix.in_channel_of_descr proc.Process.stderr_fd in
	let bout = Bytes.create 1024 in
	let berr = Bytes.create 1024 in
	(* Signal the stdin-forwarding thread to stop once the child exits.
	   The thread uses Unix.select with a short timeout to check this flag
	   periodically, avoiding a hang when the child exits but the client
	   hasn't closed its stdin yet (e.g. interactive use). *)
	let stop_stdin = ref false in
	let tin = match comm.stdin with
		| Some stdin_pipe ->
			let stdin_fd = Unix.descr_of_in_channel stdin_pipe in
			Some (Thread.create (fun () ->
				let buf = Bytes.create 1024 in
				(try while not !stop_stdin do
					let readable, _, _ = Unix.select [stdin_fd] [] [] 0.05 in
					if readable <> [] then begin
						let i = Unix.read stdin_fd buf 0 1024 in
						if i = 0 then raise Exit;
						output pin buf 0 i;
						flush pin
					end
				done with _ -> ());
				close_out_noerr pin
			) ())
		| None ->
			close_out_noerr pin;
			None
	in
	let tout = Thread.create (fun() -> read_content pout bout comm.write_out) () in
	let terr = Thread.create (fun() -> read_content perr berr comm.write_err) () in
	(* Join stdout/stderr threads first — they finish when the child closes its
	   output fds (typically on exit).  Then reap the process, stop stdin, join. *)
	Thread.join tout;
	Thread.join terr;
	close_in_noerr pout;
	close_in_noerr perr;
	let code = Process.exit proc in
	stop_stdin := true;
	(match tin with Some t -> Thread.join t | None -> ());
	code
