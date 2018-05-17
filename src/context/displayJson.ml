open Globals
open Json.Reader
open JsonRpc
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson
open Type

type haxe_json_error =
	| MissingField of string * string
	| BadType of string * string

let raise_haxe_json_error id = function
	| MissingField(name,on) -> raise_custom id 1 (Printf.sprintf "Missing param %s on %s" name on)
	| BadType(desc,expected) -> raise_custom id 2 (Printf.sprintf "Unexpected value for %s, expected %s" desc expected)

let get_capabilities () =
	JObject [
		"definitionProvider",JBool true;
		"hoverProvider",JBool true;
		"completionProvider",JBool true;
		"packageProvider",JBool true;
		"signatureHelpProvider",JBool true;
	]

(* Generate the JSON of our times. *)
let json_of_times root =
	let rec loop node =
		if node.time > 0.0009 then begin
			let children = ExtList.List.filter_map loop node.children in
			let fl = [
				"name",jstring node.name;
				"path",jstring node.path;
				"info",jstring node.info;
				"time",jfloat node.time;
				"calls",jint node.num_calls;
				"percentTotal",jfloat (node.time *. 100. /. root.time);
				"percentParent",jfloat (if node == root then 0. else node.time *. 100. /. node.parent.time);
			] in
			let fl = match children with
				| [] -> fl
				| _ -> ("children",jarray children) :: fl
			in
			Some (jobject fl)
		end else
			None
	in
	loop root

let debug_context_sign = ref None

let parse_input com input report_times did_something =
	let send_string j = raise (DisplayOutput.Completion j) in
	let send_json json = send_string (string_of_json json) in
	let process () =
		let id,name,params = JsonRpc.parse_request input in
		let f_result json =
			let fl = [
				"result",json;
			] in
			let fl = if !report_times then begin
				close_times();
				let _,_,root = Timer.build_times_tree () in
				begin match json_of_times root with
				| None -> fl
				| Some jo -> ("timers",jo) :: fl
				end
			end else fl in
			let jo = jobject fl in
			send_json (JsonRpc.result id jo);
		in
		let f_error jl =
			send_json (JsonRpc.error id 0 ~data:(Some (JArray jl)) "Compiler error")
		in
		let params = match params with
			| Some (JObject fl) -> fl
			| Some json -> raise_invalid_params json
			| None -> []
		in
		(* JSON helper *)
		let get_field desc fl name =
			try List.assoc name fl with Not_found -> raise_haxe_json_error id (MissingField(name,desc))
		in
		let get_string desc j = match j with
			| JString s -> s
			| _ -> raise_haxe_json_error id (BadType(desc,"String"))
		in
		let get_int desc j = match j with
			| JInt i -> i
			| _ -> raise_haxe_json_error id (BadType(desc,"String"))
		in
		let get_bool desc j = match j with
			| JBool b -> b
			| _ -> raise_haxe_json_error id (BadType(desc,"Bool"))
		in
		let get_array desc j = match j with
			| JArray a -> a
			| _ -> raise_haxe_json_error id (BadType(desc,"Array"))
		in
		let get_object desc j = match j with
			| JObject o -> o
			| _ -> raise_haxe_json_error id (BadType(desc,"Object"))
		in
		let get_string_field desc name fl = get_string desc (get_field desc fl name) in
		let get_int_field desc name fl = get_int desc (get_field desc fl name) in
		let get_bool_field desc name fl = get_bool desc (get_field desc fl name) in
		let get_array_field desc name fl = get_array desc (get_field desc fl name) in
		(* let get_object_field desc name fl = get_object desc (get_field desc fl name) in *)
		let get_string_param name = get_string_field "param" name params in
		let get_int_param name = get_int_field "param" name params in
		let get_bool_param name = get_bool_field "param" name params in
		let get_array_param name = get_array_field "param" name params in
		(* let get_object_param name = get_object_field "param" name params in *)

		let enable_display mode =
			com.display <- create mode;
			Parser.display_mode := mode;
			Common.define_value com Define.Display "1";
			Parser.use_doc := true;
		in
		let read_display_file was_auto_triggered requires_offset is_completion =
			let file = get_string_param "file" in
			let pos = if requires_offset then get_int_param "offset" else (-1) in
			Parser.was_auto_triggered := was_auto_triggered;
			let pos = if pos <> (-1) && not is_completion then pos + 1 else pos in
			Parser.resume_display := {
				pfile = Path.unique_full_path file;
				pmin = pos;
				pmax = pos;
			}
		in
		let open CompilationServer in
		let get_sign () = match !debug_context_sign with
			| None -> Define.get_signature com.defines
			| Some sign -> sign
		in
		let f () = match name with
			| "initialize" ->
				f_result (JObject [
					"capabilities",get_capabilities()
				])
			| "textDocument/completion" ->
				read_display_file (get_bool_param "wasAutoTriggered") true true;
				enable_display DMDefault;
			| "textDocument/definition" ->
				Common.define com Define.NoCOpt;
				read_display_file false true false;
				enable_display DMDefinition;
			| "textDocument/hover" ->
				Common.define com Define.NoCOpt;
				read_display_file false true false;
				enable_display DMHover;
			| "textDocument/package" ->
				read_display_file false false false;
				enable_display DMPackage;
			| "textDocument/signatureHelp" ->
				read_display_file (get_bool_param "wasAutoTriggered") true false;
				enable_display DMSignature
			(* server *)
			| "server/contexts" ->
				let cs = CompilationServer.force() in
				let l = List.map (fun (sign,index) -> jobject [
					"index",jstring index;
					"signature",jstring (Digest.to_hex sign);
				]) (CompilationServer.get_signs cs) in
				f_result (jarray l)
			| "server/select" ->
				let cs = CompilationServer.force() in
				let i = get_int_param "index" in
				let (ctx,_) = try CompilationServer.get_sign_by_index cs (string_of_int i) with Not_found -> f_error [jstring "No such context"] in
				debug_context_sign := Some ctx;
				f_result (jstring (Printf.sprintf "Context %i selected" i))
			| "server/modules" ->
				let cs = CompilationServer.force() in
				let sign = get_sign () in
				let l = Hashtbl.fold (fun (_,sign') m acc ->
					if sign = sign' && m.m_extra.m_kind <> MFake then jstring (s_type_path m.m_path) :: acc else acc
				) cs.cache.c_modules [] in
				f_result (jarray l)
			| "server/module" ->
				let cs = CompilationServer.force() in
				let sign = get_sign() in
				let path = Path.parse_path (get_string_param "path") in
				let m = try CompilationServer.find_module cs (path,sign) with Not_found -> f_error [jstring "No such module"] in
				f_result (generate_module () m)
			| "server/configure" ->
				let l = List.map (fun j ->
					let fl = get_object "print param" j in
					let name = get_string_field "print param" "name" fl in
					let value = get_bool_field "print param" "value" fl in
					try
						ServerMessage.set_by_name name value;
						jstring (Printf.sprintf "Printing %s %s" name (if value then "enabled" else "disabled"))
					with Not_found ->
						f_error [jstring ("Invalid print parame name: " ^ name)]
				) (get_array_param "print") in
				f_result (jarray l)
			| _ -> raise_method_not_found id name
		in
		f();
		com.json_out <- Some(f_result,f_error);
	in
	JsonRpc.handle_jsonrpc_error process send_json;
	()
