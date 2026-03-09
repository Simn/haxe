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
					write "\x02\n"
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
			exit = (fun timer_ctx code ->
				if code = 0 then begin
					if timer_ctx.measure_times = Yes then Timer.report_times timer_ctx (fun s -> self.write_err (s ^ "\n"));
				end;
				exit code;
			);
			is_server = false;
			stdin = None;
		} in
		self

	let create_pipe sctx (conn : server_connection) protocol_version =
		let write_frame tag s = conn.write (Protocol.make_frame tag s) in
		let rec self = {
			write_out = (
				if protocol_version >= Protocol.version_current then
					(* v2: raw bytes in a tagged frame — no newline encoding needed. *)
					fun s -> write_frame Protocol.tag_stdout s
				else
					(* v1 legacy: encode newlines as \x01 separators, wrap in \x01...\n. *)
					fun s -> conn.write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")
			);
			write_err = (
				if protocol_version >= Protocol.version_current then
					(* The magic \x02\n value is the internal signal for "error flag";
					   everything else is a genuine stderr chunk. *)
					fun s ->
						if s = "\x02\n" then write_frame Protocol.tag_error ""
						else write_frame Protocol.tag_stderr s
				else
					fun s -> conn.write s
			);
			flush = flush_context sctx;
			close = (fun () ->
				conn.close()
			);
			exit = (fun _timer_ctx _i ->
				()
			);
			is_server = true;
			stdin = conn.get_stdin();
		}
		in
		self
end