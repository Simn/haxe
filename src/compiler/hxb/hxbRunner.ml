open Globals
open Type
open Common
open Typecore

let make_hxb_macro_api com state =
	let open MacroApi in
	{
		pos = null_pos;
		get_com = (fun () -> com);
		get_type = (fun s ->
			begin try
				(* TODO: this isn't very efficient *)
				let mt = List.find (fun mt -> s_type_path (t_infos mt).mt_path = s) state.HxbData.types in
				Some (type_of_module_type mt)
			with Not_found ->
				None
			end
		);
		get_module = (fun _ -> assert false);
		after_typing = (fun _ -> assert false);
		on_generate = (fun _ -> assert false);
		after_generate = (fun _ -> assert false);
		on_type_not_found = (fun _ -> assert false);
		parse_string = (fun _ -> assert false);
		type_expr = (fun _ -> assert false);
		resolve_type = (fun _ -> assert false);
		type_macro_expr = (fun _ -> assert false);
		store_typed_expr = (fun _ -> assert false);
		allow_package = (fun _ -> assert false);
		type_patch = (fun _ -> assert false);
		meta_patch = (fun _ -> assert false);
		set_js_generator = (fun _ -> assert false);
		get_local_type = (fun _ -> assert false);
		get_expected_type = (fun _ -> assert false);
		get_call_arguments = (fun _ -> assert false);
		get_local_method = (fun _ -> assert false);
		get_local_imports = (fun _ -> assert false);
		get_local_using = (fun _ -> assert false);
		get_local_vars = (fun _ -> assert false);
		get_build_fields = (fun _ -> assert false);
		get_pattern_locals = (fun _ -> assert false);
		define_type = (fun _ -> assert false);
		define_module = (fun _ -> assert false);
		module_dependency = (fun _ -> assert false);
		current_module = (fun _ -> assert false);
		on_reuse = (fun _ -> assert false);
		current_macro_module = (fun _ -> assert false);
		delayed_macro = (fun _ -> assert false);
		use_cache = (fun _ -> false);
		format_string = (fun _ -> assert false);
		cast_or_unify = (fun _ -> assert false);
		add_global_metadata = (fun _ -> assert false);
		add_module_check_policy = (fun _ -> assert false);
		decode_expr = (fun _ -> assert false);
		encode_expr = (fun _ -> assert false);
		encode_ctype = (fun _ -> assert false);
		decode_type = (fun _ -> assert false);
		flush_context = (fun _ -> assert false);
	}

let run com path =
	let hxb = Std.finally (Timer.timer ["hxb";"read"]) HxbReader.read path in
	List.iter (fun m ->
		begin match m.m_path with
		| ([],"String") ->
			List.iter (function
				| TClassDecl c -> com.basic.tarray <- (fun t -> TInst(c,[t]));
				| _ -> ()
			) m.m_types
		| ([],"StdTypes") ->
			List.iter (function
				| TAbstractDecl ({a_path = ([],"Null")} as a) ->
					com.basic.tnull <- (fun t -> TAbstract(a,[t]));
				| _ ->
					()
			) m.m_types;
		| _ ->
			()
		end
	) hxb.HxbData.modules;
	com.types <- hxb.HxbData.types;
	com.main_class <- hxb.HxbData.main_class;
	List.iter (fun s -> com.include_files <- s :: com.include_files) hxb.HxbData.include_files;
	com.resources <- hxb.HxbData.resources;
	if Common.defined com Define.Dump then Codegen.Dump.dump_types com;
	let mctx = MacroContext.Interp.create com (make_hxb_macro_api com hxb) false in
	MacroContext.Interp.add_types mctx com.types (fun t -> ());
	match hxb.HxbData.main with
		| None -> ()
		| Some e -> ignore(MacroContext.Interp.eval_expr mctx e)