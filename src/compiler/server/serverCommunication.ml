open Globals
open Common
open ServerCompilationContext
open CompilationContext
open MessageReporting

let flush_context sctx ctx =
	let write = ctx.comm.write_err in
	match ctx.com.json_out with
	| Some api when not (is_diagnostics ctx.com) ->
		if has_error ctx then begin
			let errors = List.map (fun cm ->
				Json.JObject [
					"severity",JInt (MessageSeverity.to_int cm.cm_severity);
					"location",Genjson.generate_pos_as_location cm.cm_pos;
					"message",JString cm.cm_message;
				]
			) (List.rev ctx.messages) in
			api.send_error_raise errors;
		end
	| _ ->
		let add_diagnostics_messages () =
			List.iter (fun cm ->
				add_diagnostics_message ~depth:cm.cm_depth ctx.com cm.cm_message cm.cm_pos cm.cm_kind cm.cm_severity
			) (List.rev ctx.messages);
		in
		match ctx.com.report_mode with
			| RMDiagnostics _ ->
				add_diagnostics_messages ()
			| _ ->
				display_messages ctx (fun _ output ->
					write (output ^ "\n");
					ServerMessage.message output;
				);
				sctx.was_compilation <- ctx.com.display.dms_full_typing;
				if has_error ctx then begin
					ctx.com.timer_ctx.measure_times <- No;
					ctx.comm.signal_error ()
				end else
					if ctx.com.timer_ctx.measure_times = Yes then Timer.report_times ctx.com.timer_ctx (fun s -> write (s ^ "\n"));

module Communication = struct
	let create_stdio () =
		let rec self = {
			write_out = (fun s ->
				print_string s;
				flush stdout;
			);
			write_err = (fun s ->
				prerr_string s;
			);
			(* In non-server (stdio) mode display results go to stderr, same as write_err. *)
			write_result = (fun s ->
				prerr_string s;
			);
			signal_error = (fun () -> ());
			flush = (fun ctx ->
				display_messages ctx (fun sev output ->
					match sev with
						| MessageSeverity.Information -> print_endline output
						| Warning | Error | Hint -> prerr_endline output
				);

				if has_error ctx && !Helper.prompt then begin
					print_endline "Press enter to exit...";
					ignore(read_line());
				end;
				flush stdout;
			);
			close = (fun () -> ());
			is_server = false;
			stdin = None;
		} in
		self

	let create_pipe sctx (conn : server_connection) protocol_version =
		let write_frame tag s = conn.write (Protocol.make_frame tag s) in
		(* Track whether the current request encountered an error.  Read and reset
		   by [close]; set by [signal_error].  Mutable so it persists across the
		   single communication object that is shared across all requests. *)
		let has_error_ref = ref false in
		let rec self = {
			write_out = (
				if protocol_version >= Protocol.version_current then
					(* v2: raw bytes in a tagged frame — no newline encoding needed. *)
					fun s -> write_frame Protocol.tag_print s
				else
					(* v1 legacy: encode newlines as \x01 separators, wrap in \x01...\n. *)
					fun s -> conn.write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")
			);
			write_err = (
				if protocol_version >= Protocol.version_current then
					fun s -> write_frame Protocol.tag_log s
				else
					fun s -> conn.write s
			);
			write_result = (
				if protocol_version >= Protocol.version_current then
					fun s -> write_frame Protocol.tag_result s
				else
					(* v1: no separate channel; display output goes into the shared buffer. *)
					fun s -> conn.write s
			);
			signal_error = (
				if protocol_version >= Protocol.version_current then
					fun () -> has_error_ref := true
				else
					(* v1: write the legacy error-flag sentinel into the buffer. *)
					fun () -> conn.write "\x02\n"
			);
			flush = flush_context sctx;
			close = (fun () ->
				if protocol_version >= Protocol.version_current then begin
					(* v2: send TAG_DONE with a 1-byte status payload, then reset for the
					   next request.  Do NOT call conn.close() here — in v2 every write
					   is sent immediately so there is no buffered data to flush, and the
					   socket must stay open for subsequent requests. *)
					let status = if !has_error_ref then "\x01" else "\x00" in
					has_error_ref := false;
					conn.write (Protocol.make_frame Protocol.tag_done status)
				end else begin
					conn.close ()
				end
			);
			is_server = true;
			stdin = conn.get_stdin();
		}
		in
		self
end