open Ast
open Common
open DisplayTypes
open DisplayMode
open CompletionKind
open CompletionResultKind
open Type
open Typecore
open Globals
open Genjson

let reference_position = ref null_pos

module DisplayException = struct
	type kind =
		| Diagnostics of string
		| Statistics of string
		| ModuleSymbols of string
		| Metadata of string
		| DisplaySignatures of (tsignature * documentation) list * int * int
		| DisplayType of t * pos * string option
		| DisplayPosition of pos list
		| DisplayFields of CompletionKind.t list * CompletionResultKind.t * pos option (* insert pos *) * bool (* sorted? *)
		| DisplayPackage of string list

	exception DisplayException of kind

	let raise_diagnostics s = raise (DisplayException(Diagnostics s))
	let raise_statistics s = raise (DisplayException(Statistics s))
	let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
	let raise_metadata s = raise (DisplayException(Metadata s))
	let raise_signatures l isig iarg = raise (DisplayException(DisplaySignatures(l,isig,iarg)))
	let raise_type t p so = raise (DisplayException(DisplayType(t,p,so)))
	let raise_position pl = raise (DisplayException(DisplayPosition pl))
	let raise_fields ckl cr po b = raise (DisplayException(DisplayFields(ckl,cr,po,b)))
	let raise_package sl = raise (DisplayException(DisplayPackage sl))

	let to_json de = match de with
		| Diagnostics _
		| Statistics _
		| ModuleSymbols _
		| Metadata _ -> assert false
		| DisplaySignatures(sigs,isig,iarg) ->
			let ctx = Genjson.create_context () in
			let fsig ((tl,tr),doc) =
				let fl = generate_function_signature ctx tl tr in
				let fl = (match doc with None -> fl | Some s -> ("documentation",jstring s) :: fl) in
				jobject fl
			in
			jobject [
				"activeSignature",jint isig;
				"activeParameter",jint iarg;
				"signatures",jlist fsig sigs;
			]
		| DisplayType(t,p,doc) ->
			jobject [
				"documentation",jopt jstring doc;
				"range",generate_pos_as_range p;
				"type",generate_type (create_context ()) t;
			]
		| DisplayPosition pl ->
			jarray (List.map generate_pos_as_location pl)
		| DisplayFields(fields,kind,po,sorted) ->
			let ja = List.map (DisplayTypes.CompletionKind.to_json (Genjson.create_context ())) fields in
			let fl =
				("items",jarray ja) ::
				("kind",jint (Obj.magic kind)) ::
				("sorted",jbool sorted) ::
				(match po with None -> [] | Some p -> ["replaceRange",generate_pos_as_range p]) in
			jobject fl
		| DisplayPackage pack ->
			jarray (List.map jstring pack)
end

open DisplayException

let is_display_file file =
	file <> "?" && Path.unique_full_path file = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin < p_target.pmin && p.pmax >= p_target.pmax

let is_display_position p =
	encloses_position !Parser.resume_display p

module ExprPreprocessing = struct
	let find_before_pos com is_completion e =
		let display_pos = ref (!Parser.resume_display) in
		let is_annotated p = encloses_position !display_pos p in
		let annotate e dk =
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			(EDisplay(e,dk),pos e)
		in
		let annotate_marked e = annotate e DKMarked in
		let mk_null p = annotate_marked ((EConst(Ident "null")),p) in
		let loop_el el =
			let pr = !Parser.resume_display in
			let rec loop el = match el with
				| [] -> [mk_null pr]
				| e :: el ->
					if (pos e).pmin >= pr.pmax then (mk_null pr) :: e :: el
					else e :: loop el
			in
			(* print_endline (Printf.sprintf "%i-%i: PR" pr.pmin pr.pmax);
			List.iter (fun e ->
				print_endline (Printf.sprintf "%i-%i: %s" (pos e).pmin (pos e).pmax (Ast.s_expr e));
			) el; *)
			match el with
			| [] -> [mk_null pr]
			| e :: el ->
				if (pos e).pmin >= pr.pmax then (mk_null pr) :: e :: el
				else loop (e :: el)
		in
		let loop e =
			(* print_endline (Printf.sprintf "%i-%i: %s" (pos e).pmin (pos e).pmax (Ast.s_expr e)); *)
			match fst e with
			| EVars vl ->
				if List.exists (fun ((_,p),_,_) -> is_annotated p) vl then
					annotate_marked e
				else
					e
			| EBlock [] when is_annotated (pos e) ->
				annotate e DKStructure
			| EBlock el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EBlock el,(pos e)
			| ECall(e1,el) when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				ECall(e1,el),(pos e)
			| ENew((tp,pp),el) when is_annotated (pos e) && is_completion ->
				if is_annotated pp || pp.pmax >= !Parser.resume_display.pmax then
					annotate_marked e
				else begin
					let el = loop_el el in
					ENew((tp,pp),el),(pos e)
				end
			| EArrayDecl el when is_annotated (pos e) && is_completion ->
				let el = loop_el el in
				EArrayDecl el,(pos e)
			| EDisplay _ ->
				raise Exit
			| _ ->
				if is_annotated (pos e) then
					annotate_marked e
				else
					e
		in
		let rec map e =
			loop (Ast.map_expr map e)
		in
		try map e with Exit -> e

	let find_display_call e =
		let loop e = match fst e with
			| ECall _ | ENew _ when is_display_position (pos e) -> Parser.mk_display_expr e DKCall
			| _ -> e
		in
		let rec map e = loop (Ast.map_expr map e) in
		map e


	let process_expr com e = match com.display.dms_kind with
		| DMDefinition | DMUsage _ | DMHover -> find_before_pos com false e
		| DMDefault -> find_before_pos com true e
		| DMSignature -> find_display_call e
		| _ -> e
end

module DisplayEmitter = struct

	let requires_import ctx path =
		try
			let mt' = ctx.g.do_load_type_def ctx null_pos {tpackage = []; tname = snd path; tparams = []; tsub = None} in
			path <> (t_infos mt').mt_path
		with _ ->
			true

	let patch_type ctx t =
		let rec patch t = match t with
			| TInst(c,tl) when not (requires_import ctx c.cl_path) -> TInst({c with cl_path = ([],snd c.cl_path)},List.map patch tl)
			| TEnum(en,tl) when not (requires_import ctx en.e_path) -> TEnum({en with e_path = ([],snd en.e_path)},List.map patch tl)
			| TType(td,tl) when not (requires_import ctx td.t_path) -> TType({td with t_path = ([],snd td.t_path)},List.map patch tl)
			| TAbstract(a,tl) when not (requires_import ctx a.a_path) -> TAbstract({a with a_path = ([],snd a.a_path)},List.map patch tl)
			| _ -> Type.map patch t
		in
		patch t

	let display_module_type ctx mt p = match ctx.com.display.dms_kind with
		| DMDefinition -> raise_position [(t_infos mt).mt_name_pos];
		| DMUsage _ -> reference_position := (t_infos mt).mt_name_pos
		| DMHover -> raise_type (patch_type ctx (type_of_module_type mt)) p (t_infos mt).mt_doc
		| _ -> ()

	let rec display_type ctx t p =
		let dm = ctx.com.display in
		match dm.dms_kind with
		| DMHover ->
			raise_type (patch_type ctx t) p None
		| _ ->
			try display_module_type ctx (module_type_of_type t) p
			with Exit -> match follow t,follow !t_dynamic_def with
				| _,TDynamic _ -> () (* sanity check in case it's still t_dynamic *)
				| TDynamic _,_ -> display_type ctx !t_dynamic_def p
				| _ -> ()

	let check_display_type ctx t p =
		let add_type_hint () =
			let md = ctx.m.curmod.m_extra.m_display in
			md.m_type_hints <- (p,t) :: md.m_type_hints;
		in
		let maybe_display_type () =
			if ctx.is_display_file && is_display_position p then
				display_type ctx t p
		in
		match ctx.com.display.dms_kind with
		| DMStatistics -> add_type_hint()
		| DMUsage _ -> add_type_hint(); maybe_display_type()
		| _ -> maybe_display_type()

	let display_variable ctx v p = match ctx.com.display.dms_kind with
		| DMDefinition -> raise_position [v.v_pos]
		| DMUsage _ -> reference_position := v.v_pos
		| DMHover -> raise_type (patch_type ctx v.v_type) p None
		| _ -> ()

	let display_field ctx c cf p = match ctx.com.display.dms_kind with
		| DMDefinition -> raise_position [cf.cf_name_pos]
		| DMUsage _ -> reference_position := cf.cf_name_pos
		| DMHover ->
			let t = if Meta.has Meta.Impl cf.cf_meta then
				(prepare_using_field cf).cf_type
			else
				cf.cf_type
			in
			let t = match c,follow t with
				| Some c,TFun(tl,_) when cf.cf_name = "new" -> TFun(tl,TInst(c,List.map snd c.cl_params))
				| _ -> t
			in
			raise_type (patch_type ctx t) p cf.cf_doc
		| _ -> ()

	let maybe_display_field ctx c cf p =
		if is_display_position p then display_field ctx c cf p

	let display_enum_field ctx ef p = match ctx.com.display.dms_kind with
		| DMDefinition -> raise_position [ef.ef_name_pos]
		| DMUsage _ -> reference_position := ef.ef_name_pos
		| DMHover -> raise_type (patch_type ctx ef.ef_type) p ef.ef_doc
		| _ -> ()

	let display_meta com meta = match com.display.dms_kind with
		| DMHover ->
			begin match meta with
			| Meta.Custom _ | Meta.Dollar _ -> ()
			| _ -> match Meta.get_documentation meta with
				| None -> ()
				| Some (_,s) ->
					(* TODO: hack until we support proper output for hover display mode *)
					if com.json_out = None then
						raise_metadata ("<metadata>" ^ s ^ "</metadata>")
					else
						raise_type t_dynamic null_pos (Some s)
			end
		| DMDefault ->
			let all,_ = Meta.get_documentation_list() in
			let all = List.map (fun (s,doc) ->
				ITMetadata(s,Some doc)
			) all in
			raise_fields all CRMetadata None false
		| _ ->
			()

	let check_display_metadata ctx meta =
		List.iter (fun (meta,args,p) ->
			if is_display_position p then display_meta ctx.com meta;
			List.iter (fun e ->
				if is_display_position (pos e) then begin
					let e = ExprPreprocessing.process_expr ctx.com e in
					delay ctx PTypeField (fun _ -> ignore(type_expr ctx e Value));
				end
			) args
		) meta
end

open ImportStatus

let import_status_from_context ctx path =
	try
		let mt' = ctx.g.do_load_type_def ctx null_pos {tpackage = []; tname = snd path; tparams = []; tsub = None} in
		if path = (t_infos mt').mt_path then Imported
		else Shadowed
	with _ ->
		Unimported