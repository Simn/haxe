open Globals
open Common
open CompilationCache
open Type
open DisplayProcessingGlobals
open Ipaddr
open Json
open CompilationContext
open ParsedArg
open MessageReporting
open HxbData
open TypeloadCacheHook

(** Communication channel for [--server-connect] mode.

    The incoming request is in the same length-prefixed format as before:
    [int32(len)][args separated by \\n][optionally \\x01 + stdin bytes].
    This matches what [HaxeServerProcessBase.prepareInput] in haxeserver sends.

    The outgoing encoding depends on the protocol version detected from the
    parsed args.  For v2, every write goes out immediately as a binary frame so
    the client can display output as it arrives.  A [tag_done] frame is sent at
    [close()] to signal end of request.  For v1 (legacy), all output is buffered
    and sent as a single length-prefixed blob at [close()].

    [wait_loop] calls [conn.set_version] after parsing each request's args. *)
let mk_streaming_communication chin chout =
	Unix.clear_nonblock (Unix.descr_of_in_channel chin);
	let ichin = IO.input_channel chin in
	let chout_fd = Unix.descr_of_out_channel chout in
	let ichout = IO.output_channel chout in
	let version = ref Protocol.version_legacy in
	let bout = Buffer.create 0 in
	let read () =
		let len = IO.read_i32 ichin in
		IO.really_nread_string ichin len
	in
	let write s =
		if !version >= Protocol.version_current then
			Protocol.ssend chout_fd (Bytes.unsafe_of_string s)
		else
			Buffer.add_string bout s
	in
	let close () =
		if !version >= Protocol.version_current then
			Protocol.ssend chout_fd (Bytes.unsafe_of_string (Protocol.make_frame Protocol.tag_done ""))
		else begin
			IO.write_i32 ichout (Buffer.length bout);
			IO.nwrite_string ichout (Buffer.contents bout);
			IO.flush ichout;
			Buffer.clear bout
		end
	in
	let set_version v =
		version := v;
		Buffer.clear bout
	in
	fun () ->
		(* Do NOT reset version here: the accept function is called again for the
		   next request while the worker domain is still processing the current one.
		   Resetting version here races with writes/close in the worker and causes
		   the wrong protocol path (e.g. buffered v1 blob instead of a v2 TAG_DONE
		   frame) to be taken.  Version is sticky once set via set_version; the
		   initial value from [ref version_legacy] handles the very first request. *)
		{ read; write; close; get_stdin = (fun () -> None); set_version }

module Connect = struct
	(* The connect function to connect to [host] at [port] and send arguments [args]. *)
	let do_connect ip port (args : parsed_arg list) =
		let (domain, host) = match ip with
			| V4 ip -> (Unix.PF_INET, V4.to_string ip)
			| V6 ip -> (Unix.PF_INET6, V6.to_string ip)
		in
		let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
		(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with
			| Unix.Unix_error(code,_,_) -> failwith("Couldn't connect on " ^ host ^ ":" ^ string_of_int port ^ " (" ^ (Unix.error_message code) ^ ")");
			| _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port)
		);
		let protocol_version = Protocol.detect_version args in
		let raw_args = ("--cwd " ^ Unix.getcwd()) :: Args.to_raw_args args in
		let s = (String.concat "" (List.map (fun a -> a ^ "\n") raw_args)) in
		Protocol.ssend sock (Bytes.of_string (s ^ "\000"));
		let has_error = ref false in
		if protocol_version >= Protocol.version_current then
			ClientConnect.poll_new sock
				~on_print:(fun s -> print_string s; flush stdout)
				~on_log:(fun s -> prerr_string s)
				~on_result:(fun s -> prerr_string s)
				~on_done:(fun err -> has_error := err)
		else begin
			let print line =
				match (if line = "" then '\x00' else line.[0]) with
				| '\x01' ->
					print_string (String.concat "\n" (List.tl (ExtString.String.nsplit line "\x01")));
					flush stdout
				| '\x02' ->
					has_error := true;
				| _ ->
					prerr_endline line;
			in
			ClientConnect.poll sock print
		end;
		if !has_error then 1 else 0
end

module SocketRequest = struct
	type t = {
		data : string;
		stdin : in_channel;
	}

	let setup_client_stdin_forward overflow sin =
		(* Set up stdin forwarding: create a pipe and a thread that reads
		   from the client socket and writes to the pipe. *)
		let (stdin_r_fd, stdin_w_fd) = Unix.pipe ~cloexec:true () in
		let _stdin_thread = Thread.create (fun () ->
			let write_all fd data pos len =
				let rec loop pos len =
					if len > 0 then begin
						let w = Unix.write fd data pos len in
						loop (pos + w) (len - w)
					end
				in
				loop pos len
			in
			let buf = Bytes.create 1024 in
			(try
				(* Write any overflow data read past the null terminator *)
				if Bytes.length overflow > 0 then
					write_all stdin_w_fd overflow 0 (Bytes.length overflow);
				(* Forward data from client socket to stdin pipe *)
				while true do
					let n = Unix.recv sin buf 0 1024 [] in
					if n = 0 then raise Exit;
					write_all stdin_w_fd buf 0 n
				done
			with _ -> ());
			(try Unix.close stdin_w_fd with _ -> ())
		) () in
		Unix.in_channel_of_descr stdin_r_fd

	(* Reads a null-terminated request from a non-blocking socket, tracking any
	   overflow data received past the null terminator (e.g. stdin data from the client). *)
	let read sin bufsize =
		let tmp = Bytes.create bufsize in
		let b = Buffer.create 0 in
		let overflow = ref Bytes.empty in
		let rec read_loop count =
			try
				let r = Unix.recv sin tmp 0 bufsize [] in
				if r = 0 then
					failwith "Incomplete request"
				else begin
					ServerMessage.socket_message (Printf.sprintf "Reading %d bytes\n" r);
					let rec find_null i = if i >= r then -1 else if Bytes.get tmp i = '\000' then i else find_null (i + 1) in
					let null_pos = find_null 0 in
					if null_pos >= 0 then begin
						Buffer.add_subbytes b tmp 0 null_pos;
						let remaining = r - null_pos - 1 in
						if remaining > 0 then
							overflow := Bytes.sub tmp (null_pos + 1) remaining;
						Buffer.contents b
					end else begin
						Buffer.add_subbytes b tmp 0 r;
						read_loop 0
					end
				end
			with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
				if count = 100 then
					failwith "Aborting inactive connection"
				else begin
					ServerMessage.socket_message "Waiting for data...";
					ignore(Unix.select [] [] [] 0.05);
					read_loop (count + 1);
				end
		in
		let data = read_loop 0 in
		(* Switch to blocking mode before spawning the stdin forwarding thread.
		   The socket was set to non-blocking for the request-parsing phase above
		   (to handle slow clients with retries), but the forwarding thread needs
		   blocking recv to avoid exiting prematurely on EWOULDBLOCK. *)
		Unix.clear_nonblock sin;
		let stdin = setup_client_stdin_forward !overflow sin in
		{ data; stdin }
end

let create_request_scope () =
	{
		stats = Stats.create ();
		timer_ctx = Timer.make_context (Timer.make ["other"]);
		cancellation_requested = false;
	}

let process sctx request_scope entry comm (args : parsed_arg list) =
	let t0 = Extc.time() in
	ServerMessage.arguments ["<" ^ string_of_int (List.length args) ^ " pre-parsed args>"];
	ServerCompilationContext.reset sctx;
	entry sctx request_scope comm args;
	ServerCompilationContext.run_delays sctx;
	ServerMessage.stats request_scope.stats (Extc.time() -. t0)

module RequestQueue = struct
	type request = {
		args : parsed_arg list;
		stdin : string option;
		comm : unit -> communication;
	}

	type t = {
		mutex : Mutex.t;
		semaphore : Semaphore.Counting.t;
		mutable requests : request list;
		mutable current_request : request_scope option;
		shutdown_flag : bool Atomic.t;
		cancel_token : bool Atomic.t;
	}

	let create () =
		{
			mutex = Mutex.create ();
			semaphore = Semaphore.Counting.make 0;
			requests = [];
			current_request = None;
			shutdown_flag = Atomic.make false;
			cancel_token = Atomic.make false;
		}

	let wake_up rq =
		Semaphore.Counting.release rq.semaphore

	let add rq args stdin comm =
		Mutex.lock rq.mutex;
		rq.requests <- { args; stdin; comm; } :: rq.requests;
		Mutex.unlock rq.mutex;
		wake_up rq

	let shutdown rq =
		Atomic.set rq.cancel_token true;
		Atomic.set rq.shutdown_flag true;
		wake_up rq
end

module WorkerDomain = struct
	open RequestQueue
	open ServerCompilationContext

	type t = {
		domain : unit Domain.t;
	}

	let shutdown rq =
		(* Drain remaining requests by closing their connections, then return
		   without recursing to exit the loop gracefully. *)
		Mutex.lock rq.mutex;
		let pending = rq.requests in
		rq.requests <- [];
		Mutex.unlock rq.mutex;
		List.iter (fun req ->
			let comm = req.comm() in
			(try comm.signal_error(); comm.write_err "Server shutdown\n"; with _ -> ());
			comm.close();
		) pending

	let run_request sctx request_scope entry {comm; stdin; args} =
		let comm = (comm()) in
		try
			process sctx request_scope entry comm args;
			comm
		with
		| Cancelled ->
			ServerMessage.uncaught_error "Compilation cancelled";
			(try comm.signal_error(); comm.write_err "Cancelled\n"; with _ -> ());
			comm;
		| e ->
			let estr = Printexc.to_string e in
			ServerMessage.uncaught_error estr;
			(try comm.signal_error(); comm.write_err (estr ^ "\n"); with _ -> ());
			if Helper.is_debug_run then print_endline (estr ^ "\n" ^ Printexc.get_backtrace());
			if e = Out_of_memory then begin
				comm.close();
				exit (-1);
			end;
			comm

	let create sctx entry rq =
		let domain = Domain.spawn (fun () ->
			let cs = sctx.cs in
			let rec loop () =
				Semaphore.Counting.acquire rq.semaphore;
				(* Check for shutdown before doing any work *)
				if Atomic.get rq.shutdown_flag then begin
					shutdown rq
				end else begin
					Mutex.lock rq.mutex;
					match rq.requests with
					| [] ->
						Mutex.unlock rq.mutex;
						if cs#has_task then begin
							cs#get_task#run;
							RequestQueue.wake_up rq;
						end;
						loop()
					| request :: l ->
						rq.requests <- l;
						Mutex.unlock rq.mutex;
						sctx.current_stdin <- request.stdin;
						Atomic.set rq.cancel_token false;
						let request_scope = create_request_scope() in
						rq.current_request <- Some request_scope;
						let comm = run_request sctx request_scope entry request in
						comm.close();
						sctx.current_stdin <- None;
						ServerCache.cleanup();
						if sctx.was_compilation then
							cs#add_task (new Tasks.server_exploration_task cs);
						RequestQueue.wake_up rq;
						loop()
				end
			in
			loop ()
		) in
		{
			domain;
		}
end

let setup_server_context verbose =
	if verbose then ServerMessage.enable_all ();
	Sys.catch_break false; (* Sys can never catch a break *)
	(* Create server context and set up hooks for parsing and typing *)
	let sctx = ServerCompilationContext.create verbose in
	ServerCache.enable_cache_mode sctx;
	sctx

(* The server main loop. Waits for the [accept] call to then process the sent compilation
   parameters through [process_params]. *)
let wait_loop entry verbose accept =
	let sctx = setup_server_context verbose in
	let rq = RequestQueue.create () in
	let worker = WorkerDomain.create sctx entry rq in
	(* Main loop: accept connections and enqueue requests for the worker.
	   The loop exits if the accept function raises an exception (e.g. socket closed). *)
	begin try
		while true do
			let conn = accept() in
			begin try
				let s = conn.read () in
				let stdin,hxml =
					try
						let idx = String.index s '\001' in
						let stdin = (String.sub s (idx + 1) ((String.length s) - idx - 1)) in
						Some stdin,(String.sub s 0 idx)
					with Not_found ->
						None,s
				in
				let data = Helper.parse_hxml_data hxml in
				let parsed_args = Args.parse_args data in
				let protocol_version = Protocol.detect_version parsed_args in
				conn.set_version protocol_version;
				let comm () = ServerCommunication.Communication.create_pipe sctx conn protocol_version in
				RequestQueue.add rq parsed_args stdin comm;
			with Unix.Unix_error _ ->
				ServerMessage.socket_message "Connection Aborted";
				conn.close()
			end;
		done
	with _ ->
		()
	end;
	RequestQueue.shutdown rq;
	Domain.join worker.domain;
	ServerCompilationContext.dispose sctx;
	0

(* Connect to given host/port and return accept function for communication *)
let init_wait_connect ip port =
	let host = match ip with
		| V4 ip -> V4.to_string ip
		| V6 ip -> V6.to_string ip
	in
	let host = Unix.inet_addr_of_string host in
	let chin, chout = Unix.open_connection (Unix.ADDR_INET (host,port)) in
	mk_streaming_communication chin chout

(* The accept-function to wait for a socket connection. *)
let init_wait_socket ip port =
	let (domain, host) = match ip with
		| V4 ip -> (Unix.PF_INET, V4.to_string ip)
		| V6 ip -> (Unix.PF_INET6, V6.to_string ip)
	in
	let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
	(try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	ServerMessage.socket_message ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	Unix.listen sock 10;
	let bufsize = 1024 in
	let accept() = (
		let sin, _ = Unix.accept sock in
		Unix.set_nonblock sin;
		ServerMessage.socket_message "Client connected";
		let stdin_pipe = ref None in
		let read () =
			let req = SocketRequest.read sin bufsize in
			stdin_pipe := Some (req.stdin);
			req.data
		in
		let get_stdin () = !stdin_pipe in
		let closed = ref false in
		let close() =
			if not !closed then begin
				closed := true;
				(* Shutdown before close to ensure FIN is sent to the client even if
				   the stdin-forwarding thread has a pending recv on the same fd.
				   Unix.close alone may not send FIN while another thread blocks on recv. *)
				(try Unix.shutdown sin Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
				(try Unix.close sin with Unix.Unix_error _ -> trace "Error while closing socket.");
			end
		in
		let write s =
			if not !closed then
				match Unix.getsockopt_error sin with
				| Some _ -> close()
				| None -> Protocol.ssend sin (Bytes.unsafe_of_string s);
		in
		{ read; write; close; get_stdin; set_version = (fun _ -> ()) }
	) in
	accept
