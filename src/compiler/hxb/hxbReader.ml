open Globals
open Ast
open Type
open HxbData

type hxb_context = {
	ch : IO.input;
	mutable texpr_positions : texpr_position_mode;
	mutable full_read       : bool;
	(* tables *)
	mutable strings         : string array;
	mutable classes         : tclass array;
	mutable enums           : tenum array;
	mutable typedefs        : tdef array;
	mutable abstracts       : tabstract array;
	mutable class_fields    : tclass_field array;
	mutable enum_fields     : tenum_field array;
	mutable anon_fields     : tclass_field array;
	mutable anons           : tanon array;
	mutable modules         : module_def array;
	mutable vars            : tvar array;
	(* context *)
	mutable file            : string;
	mutable curpmin         : int;
	mutable curpmax         : int;
	(* state *)
	state                   : common_state;
}

let null_typedef = {
	t_path = ([],"");
	t_module = null_module;
	t_pos = null_pos;
	t_name_pos = null_pos;
	t_private = true;
	t_doc = None;
	t_meta = [];
	t_params = [];
	t_type = t_dynamic;
}

let null_enum = {
	e_path = ([],"");
	e_module = null_module;
	e_pos = null_pos;
	e_name_pos = null_pos;
	e_private = true;
	e_doc = None;
	e_meta = [];
	e_params = [];
	e_type = null_typedef;
	e_extern = true;
	e_constrs = PMap.empty;
	e_names = [];
}

let null_enum_field = {
	ef_name = "";
	ef_type = t_dynamic;
	ef_pos = null_pos;
	ef_name_pos = null_pos;
	ef_doc = None;
	ef_index = 0;
	ef_params = [];
	ef_meta = [];
}

let null_tvar = {
	v_id = -1;
	v_name = "";
	v_type = t_dynamic;
	v_capture = false;
	v_extra = None;
	v_meta = [];
	v_pos = null_pos;
}

let null_type_path = {
	tpackage = [];
	tname = "";
	tparams = [];
	tsub = None;
}

let null_func = {
	f_params = [];
	f_args = [];
	f_type = None;
	f_expr = None;
}

let null_placed_type_path = null_type_path,null_pos

let list_to_pmap a f =
	List.fold_left (fun pm x -> PMap.add (f x) x pm) PMap.empty a

(* Utility *)

module HxbRead = struct
	let read_array ctx f =
		let length = IO.read_i32 ctx.ch in
		Array.init length (fun _ -> f ctx)

	let read_list ctx f =
		let length = IO.read_i32 ctx.ch in
		ExtList.List.init length (fun _ -> f ctx)

	let read_list_8 ctx f =
		let length = IO.read_byte ctx.ch in
		ExtList.List.init length (fun _ -> f ctx)

	let read_hashtbl ctx fk fv =
		let length = IO.read_i32 ctx.ch in
		let h = Hashtbl.create length in
		for i = 0 to length - 1 do
			let k = fk ctx in
			let v = fv ctx in
			Hashtbl.add h k v
		done;
		h

	let read_lut ctx =
		IO.read_i32 ctx.ch

	let read_string ctx =
		ctx.strings.(read_lut ctx)

	let read_raw_string ctx =
		let i = IO.read_i32 ctx.ch in
		IO.nread_string ctx.ch i

	let read_dot_path ctx =
		let path = read_list_8 ctx read_string in
		let name = read_string ctx in
		(path,name)

	let read_bool ctx =
		IO.read_byte ctx.ch <> 0

	let read_option ctx f =
		if read_bool ctx then Some(f ctx)
		else None

	let read_position ctx =
		let file = read_string ctx in
		let min = IO.read_i32 ctx.ch in
		let max = IO.read_i32 ctx.ch in
		{
			pfile = file;
			pmin = min;
			pmax = max;
		}

	let read_position2 ctx =
		let min = IO.read_i32 ctx.ch in
		let max = IO.read_i32 ctx.ch in
		{
			pfile = ctx.file;
			pmin = min;
			pmax = max;
		}
end

module HxbTypeRef = struct
	open HxbRead
	let read_class_field_ref ctx = ctx.class_fields.(read_lut ctx)

	let read_enum_field_ref ctx = ctx.enum_fields.(read_lut ctx)

	let read_anon_field_ref ctx = ctx.anon_fields.(read_lut ctx)

	let read_class_ref ctx = ctx.classes.(read_lut ctx)

	let read_enum_ref ctx = ctx.enums.(read_lut ctx)

	let read_typedef_ref ctx = ctx.typedefs.(read_lut ctx)

	let read_abstract_ref ctx = ctx.abstracts.(read_lut ctx)

	let read_anon_ref ctx =
		ctx.anons.(read_lut ctx)

	let read_module_ref ctx =
		ctx.modules.(read_lut ctx)

	(* Type *)

	let rec read_typeref_params ctx =
		read_list_8 ctx read_typeref

	and read_typeref ctx =
		match IO.read_byte ctx.ch with
		| 0 -> t_dynamic
		| 1 -> TDynamic (read_typeref ctx)
		| 2 -> TInst(read_class_ref ctx,[])
		| 3 ->
			let c = read_class_ref ctx in
			TInst(c,read_typeref_params ctx)
		| 4 -> TEnum(read_enum_ref ctx,[])
		| 5 ->
			let en = read_enum_ref ctx in
			TEnum(en,read_typeref_params ctx)
		| 6 ->
			let c = read_class_ref ctx in
			TType(class_module_type c,[])
		| 7 ->
			let en = read_enum_ref ctx in
			TType(enum_module_type en.e_module en.e_path en.e_pos,[])
		| 8 ->
			let a = read_abstract_ref ctx in
			TType(abstract_module_type a [],[])
		| 9 -> TType(read_typedef_ref ctx,[])
		| 10 ->
			let td = read_typedef_ref ctx in
			TType(td,read_typeref_params ctx)
		| 11 -> TAbstract(read_abstract_ref ctx,[])
		| 12 ->
			let a = read_abstract_ref ctx in
			TAbstract(a,read_typeref_params ctx)
		| 13 -> TAnon(read_anon_ref ctx)
		| 14 -> TFun([],read_typeref ctx)
		| 15 ->
			let args = read_list_8 ctx read_function_arg in
			TFun(args,read_typeref ctx)
		| _ -> assert false

	and read_function_arg ctx =
		let name = read_string ctx in
		let opt = read_bool ctx in
		let t = read_typeref ctx in
		(name,opt,t)

	let read_class_ref_with_params ctx =
		let c = read_class_ref ctx in
		let tl = read_typeref_params ctx in
		(c,tl)
end

module HxbExpr = struct
	open HxbRead
	let rec read_binop ctx =
		match IO.read_byte ctx.ch with
			| 0 -> OpAdd
			| 1 -> OpMult
			| 2 -> OpDiv
			| 3 -> OpSub
			| 4 -> OpAssign
			| 5 -> OpEq
			| 6 -> OpNotEq
			| 7 -> OpGt
			| 8 -> OpGte
			| 9 -> OpLt
			| 10 -> OpLte
			| 11 -> OpAnd
			| 12 -> OpOr
			| 13 -> OpXor
			| 14 -> OpBoolAnd
			| 15 -> OpBoolOr
			| 16 -> OpShl
			| 17 -> OpShr
			| 18 -> OpUShr
			| 19 -> OpMod
			| 20 -> OpAssignOp(read_binop ctx)
			| 21 -> OpInterval
			| 22 -> OpArrow
			| 23 -> OpIn
			| _ -> assert false

	let read_unop ctx =
		match IO.read_byte ctx.ch with
		| 0 -> Increment
		| 1 -> Decrement
		| 2 -> Not
		| 3 -> Neg
		| 4 -> NegBits
		| _ -> assert false

	let read_quote_status ctx =
		match IO.read_byte ctx.ch with
		| 0 -> NoQuotes
		| 1 -> DoubleQuotes
		| _ -> assert false

	let read_object_field_key ctx =
		let name = read_string ctx in
		let p = read_position ctx in
		let quotes = read_quote_status ctx in
		(name,p,quotes)

	let rec read_type_path ctx =
		let pack = read_list_8 ctx read_string in
		let name = read_string ctx in
		let params = read_list_8 ctx read_type_param_or_const in
		let sub = read_option ctx read_string in
		{
			tpackage = pack;
			tname = name;
			tparams = params;
			tsub = sub;
		}

	and read_placed_type_path ctx =
		let tp = read_type_path ctx in
		let p = read_position ctx in
		(tp,p)

	and read_type_param_or_const ctx =
		match IO.read_byte ctx.ch with
		| 0 -> TPType(read_type_hint ctx)
		| 1 -> TPExpr(read_expr ctx)
		| _ -> assert false

	and read_complex_type ctx =
		match IO.read_byte ctx.ch with
		| 0 -> CTPath(read_type_path ctx)
		| 1 ->
			let thl = read_list_8 ctx read_type_hint in
			let th = read_type_hint ctx in
			CTFunction(thl,th)
		| 2 ->
			let cffl = read_list_8 ctx read_class_field in
			CTAnonymous cffl
		| 3 ->
			CTParent(read_type_hint ctx)
		| 4 ->
			let ptp = read_list_8 ctx read_placed_type_path in
			let cffl = read_list_8 ctx read_class_field in
			CTExtend(ptp,cffl)
		| 5 ->
			CTOptional(read_type_hint ctx)
		| 6 ->
			let pn = read_placed_name ctx in
			let th = read_type_hint ctx in
			CTNamed(pn,th)
		| _ ->
			assert false

	and read_type_hint ctx =
		let ct = read_complex_type ctx in
		let p = read_position ctx in
		(ct,p)

	and read_type_param ctx =
		let name = read_placed_name ctx in
		let params = read_list_8 ctx read_type_param in
		let constraints = read_list_8 ctx read_type_hint in
		let meta = read_list_8 ctx read_metadata in
		{
			tp_name = name;
			tp_params = params;
			tp_constraints = constraints;
			tp_meta = meta;
		}

	and read_func_arg ctx =
		let pn = read_placed_name ctx in
		let b = read_bool ctx in
		let meta = read_list_8 ctx read_metadata in
		let tho = read_option ctx read_type_hint in
		let eo = read_option ctx read_expr in
		(pn,b,meta,tho,eo)

	and read_func ctx =
		let params = read_list_8 ctx read_type_param in
		let args = read_list_8 ctx read_func_arg in
		let t = read_option ctx read_type_hint in
		let eo = read_option ctx read_expr in
		{
			f_params = params;
			f_args = args;
			f_type = t;
			f_expr = eo;
		}

	and read_placed_name ctx =
		let s = read_string ctx in
		let p = read_position ctx in
		(s,p)

	and read_metadata ctx =
		let name = read_string ctx in
		let args = read_list_8 ctx read_expr in
		let pos = read_position ctx in
		(Meta.from_string name,args,pos)

	and read_access ctx =
		match IO.read_byte ctx.ch with
		| 0 -> APublic
		| 1 -> APrivate
		| 2 -> AStatic
		| 3 -> AOverride
		| 4 -> ADynamic
		| 5 -> AInline
		| 6 -> AMacro
		| 7 -> AFinal
		| _ -> assert false

	and read_class_field_kind ctx =
		match IO.read_byte ctx.ch with
		| 0 ->
			let tho = read_option ctx read_type_hint in
			let eo = read_option ctx read_expr in
			FVar(tho,eo)
		| 1 ->
			FFun(read_func ctx)
		| 2 ->
			let pn1 = read_placed_name ctx in
			let pn2 = read_placed_name ctx in
			let tho = read_option ctx read_type_hint in
			let eo = read_option ctx read_expr in
			FProp(pn1,pn2,tho,eo)
		| _ ->
			assert false

	and read_class_field ctx =
		let name = read_placed_name ctx in
		let doc = read_option ctx read_string in
		let p = read_position ctx in
		let meta = read_list_8 ctx read_metadata in
		let access = read_list_8 ctx read_access in
		let kind = read_class_field_kind ctx in
		{
			cff_name = name;
			cff_doc = doc;
			cff_pos = p;
			cff_meta = meta;
			cff_access = access;
			cff_kind = kind;
		}

	and read_expr ctx =
		let p = read_position2 ctx in
		let e = match IO.read_byte ctx.ch with
		| 0 -> EConst(Int(read_string ctx))
		| 1 -> EConst(Float(read_string ctx))
		| 2 -> EConst(String(read_string ctx))
		| 3 -> EConst(Ident(read_string ctx))
		| 4 ->
			let s1 = read_string ctx in
			let s2 = read_string ctx in
			EConst(Regexp(s1,s2))
		| 5 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EArray(e1,e2)
		| 6 ->
			let op = read_binop ctx in
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EBinop(op,e1,e2)
		| 7 ->
			let e1 = read_expr ctx in
			let s = read_string ctx in
			EField(e1,s)
		| 8 -> EParenthesis(read_expr ctx)
		| 9 ->
			let read_object_field _ =
				let k = read_object_field_key ctx in
				let e = read_expr ctx in
				(k,e)
			in
			EObjectDecl(read_list_8 ctx read_object_field)
		| 10 ->
			EArrayDecl(read_list_8 ctx read_expr)
		| 11 ->
			let e1 = read_expr ctx in
			let el = read_list_8 ctx read_expr in
			ECall(e1,el)
		| 12 ->
			let ptp = read_placed_type_path ctx in
			let el = read_list_8 ctx read_expr in
			ENew(ptp,el)
		| 13 ->
			let op = read_unop ctx in
			let e1 = read_expr ctx in
			EUnop(op,Prefix,e1)
		| 14 ->
			let op = read_unop ctx in
			let e1 = read_expr ctx in
			EUnop(op,Postfix,e1)
		| 15 ->
			let read_var _ =
				let pn = read_placed_name ctx in
				let tho = read_option ctx read_type_hint in
				let eo = read_option ctx read_expr in
				(pn,tho,eo)
			in
			let vl = read_list_8 ctx read_var in
			EVars vl
		| 16 ->
			let so = read_option ctx read_string in
			let f = read_func ctx in
			EFunction(so,f)
		| 17 ->
			EBlock(read_list ctx read_expr)
		| 18 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EFor(e1,e2)
		| 19 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EIf(e1,e2,None)
		| 20 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			let e3 = read_expr ctx in
			EIf(e1,e2,Some e3)
		| 21 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EWhile(e1,e2,NormalWhile)
		| 22 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			EWhile(e1,e2,DoWhile)
		| 23 ->
			let e1 = read_expr ctx in
			let read_case _ =
				let el = read_list_8 ctx read_expr in
				let eg = read_option ctx read_expr in
				let e = read_option ctx read_expr in
				let p = read_position2 ctx in
				(el,eg,e,p)
			in
			let cases = read_list ctx read_case in
			let read_default _ =
				let eo = read_option ctx read_expr in
				let p = read_position2 ctx in
				(eo,p)
			in
			let def = read_option ctx read_default in
			ESwitch(e1,cases,def)
		| 24 ->
			let e1 = read_expr ctx in
			let read_catch _ =
				let pn = read_placed_name ctx in
				let th = read_type_hint ctx in
				let e = read_expr ctx in
				let p = read_position ctx in
				(pn,th,e,p)
			in
			let catches = read_list_8 ctx read_catch in
			ETry(e1,catches)
		| 25 -> EReturn None
		| 26 -> EReturn (Some (read_expr ctx))
		| 27 -> EBreak
		| 28 -> EContinue
		| 29 -> EUntyped (read_expr ctx)
		| 30 -> EThrow (read_expr ctx)
		| 31 -> ECast (read_expr ctx,None)
		| 32 ->
			let e1 = read_expr ctx in
			let th = read_type_hint ctx in
			ECast(e1,Some th)
		| 33 ->
			let e1 = read_expr ctx in
			let b = read_bool ctx in
			EDisplay(e1,b)
		| 34 ->
			let e1 = read_expr ctx in
			let e2 = read_expr ctx in
			let e3 = read_expr ctx in
			ETernary(e1,e2,e3)
		| 35 ->
			let e1 = read_expr ctx in
			let th = read_type_hint ctx in
			ECheckType(e1,th)
		| 36 ->
			let meta = read_metadata ctx in
			let e1 = read_expr ctx in
			EMeta(meta,e1)
		| _ -> assert false
		in
		(e,p)
end

module HxbTexpr = struct
	open HxbRead
	open HxbExpr
	open HxbTypeRef

	let read_class : (hxb_context -> bool -> tclass) ref = ref (fun _ -> assert false)

	let read_type_parameter ctx =
		let name = read_string ctx in
		let c = !read_class ctx true in
		(name,TInst(c,[]))

	let read_tconstant ctx =
		match IO.read_byte ctx.ch with
		| 0 -> TBool(false)
		| 1 -> TBool(true)
		| 2 -> TNull
		| 3 -> TThis
		| 4 -> TSuper
		| 5 -> TInt(IO.read_real_i32 ctx.ch)
		| 6 -> TFloat(read_string ctx)
		| 7 -> TString(read_string ctx)
		| _ -> assert false

	let read_tfield_access ctx =
		match IO.read_byte ctx.ch with
		| 0 ->
			let c = read_class_ref ctx in
			let tl = read_typeref_params ctx in
			let cf = read_class_field_ref ctx in
			FInstance(c,tl,cf)
		| 1 ->
			let c = read_class_ref ctx in
			let cf = read_class_field_ref ctx in
			FStatic(c,cf)
		| 2 -> FAnon(read_anon_field_ref ctx)
		| 3 -> FDynamic(read_string ctx)
		| 4 -> FClosure(None,read_class_field_ref ctx)
		| 5 ->
			let c = read_class_ref ctx in
			let tl = read_typeref_params ctx in
			let cf = read_class_field_ref ctx in
			FClosure(Some(c,tl),cf)
		| 6 ->
			let en = read_enum_ref ctx in
			let ef = read_enum_field_ref ctx in
			FEnum(en,ef)
		| _ -> assert false

	let rec read_tvar_def ctx =
		let read_tvar_extra _ =
			let params = read_list_8 ctx read_type_parameter in
			let e = read_option ctx read_texpr in
			(params,e)
		in
		let i = read_lut ctx in
		let id = IO.read_i32 ctx.ch in
		let name = read_string ctx in
		let t = read_typeref ctx in
		let capture = read_bool ctx in
		let extra = read_option ctx read_tvar_extra in
		let meta = read_list_8 ctx read_metadata in
		let p = read_position ctx in
		let v = {
			v_id = id;
			v_name = name;
			v_type = t;
			v_capture = capture;
			v_extra = extra;
			v_meta = meta;
			v_pos = p;
		} in
		ctx.vars.(i) <- v;
		v

	and read_object_field ctx =
		let k = read_object_field_key ctx in
		let e = read_texpr ctx in
		(k,e)

	and read_tfunction_arg ctx =
		let v = read_tvar_def ctx in
		let cto = read_option ctx read_tconstant in
		(v,cto)

	and read_tfunction ctx =
		let args = read_list_8 ctx read_tfunction_arg in
		let r = read_typeref ctx in
		let e = read_texpr ctx in
		{
			tf_args = args;
			tf_type = r;
			tf_expr = e;
		}

	and read_switch_case ctx =
		let el = read_list_8 ctx read_texpr in
		let e = read_texpr ctx in
		(el,e)

	and read_catch ctx =
		let v = read_tvar_def ctx in
		let e = read_texpr ctx in
		(v,e)

	and read_texpr ctx =
		let p = match ctx.texpr_positions with
			| RelativePositions ->
				ctx.curpmin <- ctx.curpmin + IO.read_i16 ctx.ch;
				ctx.curpmax <- ctx.curpmax + IO.read_i16 ctx.ch;
				{
					pfile = ctx.file;
					pmin = ctx.curpmin;
					pmax = ctx.curpmax;
				}
			| AbsolutePositions ->
				let pmin = IO.read_i32 ctx.ch in
				let pmax = IO.read_i32 ctx.ch in
				{
					pfile = ctx.file;
					pmin;
					pmax;
				}
			| NoPositions ->
				null_pos
		in
		let t = read_typeref ctx in
		let e = match IO.read_byte ctx.ch with
			| 0 -> TConst(read_tconstant ctx)
			| 1 -> TLocal (ctx.vars.(read_lut ctx))
			| 2 ->
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TArray(e1,e2)
			| 3 ->
				let op = read_binop ctx in
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TBinop(op,e1,e2)
			| 4 ->
				let e1 = read_texpr ctx in
				let fa = read_tfield_access ctx in
				TField(e1,fa)
			| 5 -> TTypeExpr (TClassDecl (read_class_ref ctx))
			| 6 -> TTypeExpr (TEnumDecl (read_enum_ref ctx))
			| 7 -> TTypeExpr (TTypeDecl (read_typedef_ref ctx))
			| 8 -> TTypeExpr (TAbstractDecl (read_abstract_ref ctx))
			| 9 -> TParenthesis(read_texpr ctx)
			| 10 -> TObjectDecl(read_list_8 ctx read_object_field)
			| 11 -> TArrayDecl(read_list_8 ctx read_texpr)
			| 12 ->
				let e1 = read_texpr ctx in
				let el = read_list_8 ctx read_texpr in
				TCall(e1,el)
			| 13 ->
				let c = read_class_ref ctx in
				let tl = read_typeref_params ctx in
				let el = read_list_8 ctx read_texpr in
				TNew(c,tl,el)
			| 14 ->
				let op = read_unop ctx in
				let e1 = read_texpr ctx in
				TUnop(op,Prefix,e1)
			| 15 ->
				let op = read_unop ctx in
				let e1 = read_texpr ctx in
				TUnop(op,Postfix,e1)
			| 16 -> TFunction(read_tfunction ctx)
			| 17 ->
				let v = read_tvar_def ctx in
				let eo = read_option ctx read_texpr in
				TVar(v,eo)
			| 18 -> TBlock(read_list ctx read_texpr)
			| 19 ->
				let v = read_tvar_def ctx in
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TFor(v,e1,e2)
			| 20 ->
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TIf(e1,e2,None)
			| 21 ->
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				let e3 = read_texpr ctx in
				TIf(e1,e2,Some e3)
			| 22 ->
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TWhile(e1,e2,NormalWhile)
			| 23 ->
				let e1 = read_texpr ctx in
				let e2 = read_texpr ctx in
				TWhile(e1,e2,DoWhile)
			| 24 ->
				let e1 = read_texpr ctx in
				let cases = read_list ctx read_switch_case in
				let eo = read_option ctx read_texpr in
				TSwitch(e1,cases,eo)
			| 25 ->
				let e1 = read_texpr ctx in
				let catches = read_list_8 ctx read_catch in
				TTry(e1,catches)
			| 26 -> TReturn None
			| 27 -> TReturn (Some (read_texpr ctx))
			| 28 -> TBreak
			| 29 -> TContinue
			| 30 -> TThrow (read_texpr ctx)
			| 31 -> TCast(read_texpr ctx,None)
			| 32 ->
				let e1 = read_texpr ctx in
				let c = read_class_ref ctx in
				TCast(e1,Some (TClassDecl c))
			| 33 ->
				let e1 = read_texpr ctx in
				let en = read_enum_ref ctx in
				TCast(e1,Some (TEnumDecl en))
			| 34 ->
				let e1 = read_texpr ctx in
				let td = read_typedef_ref ctx in
				TCast(e1,Some (TTypeDecl td))
			| 35 ->
				let e1 = read_texpr ctx in
				let a = read_abstract_ref ctx in
				TCast(e1,Some (TAbstractDecl a))
			| 36 ->
				let m = read_metadata ctx in
				let e1 = read_texpr ctx in
				TMeta(m,e1)
			| 37 ->
				let e1 = read_texpr ctx in
				let ef = read_enum_field_ref ctx in
				let i = IO.read_i32 ctx.ch in
				TEnumParameter(e1,ef,i)
			| 38 -> TEnumIndex(read_texpr ctx)
			| 39 -> TIdent(read_string ctx)
			| _ -> assert false
		in
		{
			eexpr = e;
			etype = t;
			epos = p;
		}

	let read_texpr_init ctx =
		let length = IO.read_i32 ctx.ch in
		ctx.vars <- Array.make length null_tvar;
		ctx.file <- read_string ctx;
		ctx.curpmin <- IO.read_i32 ctx.ch;
		ctx.curpmax <- IO.read_i32 ctx.ch;
		read_texpr ctx
end

module HxbField = struct
	open HxbRead
	open HxbExpr
	open HxbTexpr
	open HxbTypeRef

	let read_class_field_data ctx cf =
		cf.cf_type <- read_typeref ctx;
		cf.cf_public <- read_bool ctx;
		cf.cf_doc <- read_option ctx read_string;
		cf.cf_meta <- read_list_8 ctx read_metadata;
		cf.cf_params <- read_list_8 ctx read_type_parameter;
		cf.cf_kind <- match IO.read_byte ctx.ch with
			| 0 -> Method MethNormal
			| 1 -> Method MethInline
			| 2 -> Method MethDynamic
			| 3 -> Method MethMacro
			| 4 -> Var {v_read = AccNormal; v_write = AccNormal}
			| 5 -> Var {v_read = AccNormal; v_write = AccNo}
			| 6 -> Var {v_read = AccNormal; v_write = AccNever}
			| 7 -> Var {v_read = AccNormal; v_write = AccCtor}
			| 8 -> Var {v_read = AccNormal; v_write = AccResolve}
			| 9 -> Var {v_read = AccNormal; v_write = AccCall}
			| 10 -> Var {v_read = AccInline; v_write = AccNever}
			| 11 -> Var {v_read = AccCall; v_write = AccNever}
			| 12 ->
				let read () = match IO.read_byte ctx.ch with
					| 0 -> AccNormal
					| 1 -> AccNo
					| 2 -> AccNever
					| 3 -> AccCtor
					| 4 -> AccResolve
					| 5 -> AccCall
					| 6 -> AccInline
					| 7 ->
						let s = read_string ctx in
						let so = read_option ctx read_string in
						AccRequire(s,so)
					| _ -> assert false
				in
				let r = read() in
				let w = read() in
				Var {v_read = r; v_write = w}
			| _ ->
				assert false

	let rec read_class_field ctx =
		let cf = read_class_field_ref ctx in
		read_class_field_data ctx cf;
		cf.cf_overloads <- read_list ctx read_class_field;
		cf

	let read_enum_field ctx =
		let ef = read_enum_field_ref ctx in
		ef.ef_type <- read_typeref ctx;
		ef.ef_params <- read_list_8 ctx read_type_parameter;
		ef.ef_meta <- read_list_8 ctx read_metadata;
		ef
end

module HxbModuleType = struct
	open HxbRead
	open HxbTypeRef
	open HxbExpr
	open HxbTexpr
	open HxbField

	let read_class_kind ctx =
		match IO.read_byte ctx.ch with
		| 0 -> KNormal
		| 1 -> KTypeParameter (read_typeref_params ctx)
		| 2 -> KExpr (read_expr ctx)
		| 3 -> KGeneric
		| 4 ->
			let c = read_class_ref ctx in
			let tl = read_typeref_params ctx in
			KGenericInstance(c,tl)
		| 5 -> KMacroType
		| 6 -> KGenericBuild (read_list_8 ctx HxbExpr.read_class_field)
		| 7 -> KAbstractImpl(read_abstract_ref ctx)
		| _ -> assert false

	let read_class1 ctx is_tp =
		match IO.read_byte ctx.ch with
		| 0 ->
			let c = {
				null_class with
				cl_path = read_dot_path ctx
			} in
			c.cl_module <- read_module_ref ctx;
			c.cl_pos <- read_position ctx;
			c.cl_name_pos <- read_position ctx;
			c.cl_private <- read_bool ctx;
			c.cl_doc <- read_option ctx read_string;
			c.cl_meta <- read_list_8 ctx read_metadata;
			c.cl_extern <- read_bool ctx;
			c.cl_interface <- read_bool ctx;
			c
		| _ ->
			let path = read_dot_path ctx in
			failwith (s_type_path path)

	let read_class2 ctx c =
		c.cl_params <- read_list_8 ctx read_type_parameter;
		c.cl_kind <- read_class_kind ctx;
		c.cl_super <- read_option ctx read_class_ref_with_params;
		c.cl_implements <- read_list_8 ctx read_class_ref_with_params;
		c.cl_ordered_statics <- read_list ctx read_class_field;
		c.cl_ordered_fields <- read_list ctx read_class_field;
		c.cl_fields <- list_to_pmap c.cl_ordered_fields (fun cf -> cf.cf_name);
		c.cl_statics <- list_to_pmap c.cl_ordered_statics (fun cf -> cf.cf_name);
		c.cl_dynamic <- read_option ctx read_typeref;
		c.cl_array_access <- read_option ctx read_typeref;
		c.cl_constructor <- read_option ctx read_class_field;
		c.cl_overrides <- read_list ctx read_class_field_ref;
		c.cl_descendants <- read_list ctx read_class_ref

	let read_typedef1 ctx =
		match IO.read_byte ctx.ch with
		| 0 ->
			let path = read_dot_path ctx in
			let m = read_module_ref ctx in
			let p = read_position ctx in
			let pn = read_position ctx in
			let priv = read_bool ctx in
			let doc = read_option ctx read_string in
			let meta = read_list_8 ctx read_metadata in
			let td = {
				t_path = path;
				t_module = m;
				t_pos = p;
				t_name_pos = pn;
				t_private = priv;
				t_doc = doc;
				t_meta = meta;
				t_params = [];
				t_type = t_dynamic;
			} in
			td
		| _ ->
			let path = read_dot_path ctx in
			failwith (s_type_path path)

	let read_typedef2 ctx td =
		td.t_params <- read_list_8 ctx read_type_parameter;
		td.t_type <- read_typeref ctx

	let read_enum1 ctx =
		match IO.read_byte ctx.ch with
		| 0 ->
			let path = read_dot_path ctx in
			let m = read_module_ref ctx in
			let p = read_position ctx in
			let pn = read_position ctx in
			let priv = read_bool ctx in
			let doc = read_option ctx read_string in
			let meta = read_list_8 ctx read_metadata in
		let tdef = read_typedef1 ctx in
			let extern = read_bool ctx in
			let en = {
				e_path = path;
				e_module = m;
				e_pos = p;
				e_name_pos = pn;
				e_private = priv;
				e_doc = doc;
				e_meta = meta;
				e_params = [];
				e_type = tdef;
				e_extern = extern;
				e_constrs = PMap.empty;
				e_names = [];
			} in
			en
		| _ ->
			let path = read_dot_path ctx in
			failwith (s_type_path path)

	let read_enum2 ctx en =
		en.e_params <- read_list_8 ctx read_type_parameter;
		let ctors = read_list_8 ctx read_enum_field in
		let names = List.map (fun ef -> ef.ef_name) ctors in
		let ctors = list_to_pmap ctors (fun ef -> ef.ef_name) in
		en.e_constrs <- ctors;
		en.e_names <- names

	let read_abstract1 ctx =
		match IO.read_byte ctx.ch with
		| 0 ->
			let path = read_dot_path ctx in
			let m = read_module_ref ctx in
			let p = read_position ctx in
			let pn = read_position ctx in
			let priv = read_bool ctx in
			let doc = read_option ctx read_string in
			let meta = read_list_8 ctx read_metadata in
			let a = {
				a_path = path;
				a_module = m;
				a_pos = p;
				a_name_pos = pn;
				a_private = priv;
				a_doc = doc;
				a_meta = meta;
				a_params = [];
				a_ops = [];
				a_unops = [];
				a_impl = None;
				a_this = t_dynamic;
				a_from = [];
				a_from_field = [];
				a_to = [];
				a_to_field = [];
				a_array = [];
				a_resolve = None;
			} in
			a
		| _ ->
			let path = read_dot_path ctx in
			failwith (s_type_path path)

	let read_abstract2 ctx a =
		a.a_params <- read_list_8 ctx read_type_parameter;
		a.a_this <- read_typeref ctx;
		a.a_from <- read_list_8 ctx read_typeref;
		a.a_to <- read_list_8 ctx read_typeref;
		match IO.read_byte ctx.ch with
		| 0 ->
			()
		| 1 ->
			let read_cast_field _ =
				let t = read_typeref ctx in
				let name = read_class_field_ref ctx in
				(t,name)
			in
			a.a_impl <- Some (read_class_ref ctx);
			a.a_from_field <- read_list_8 ctx read_cast_field;
			a.a_to_field <- read_list_8 ctx read_cast_field;
			a.a_array <- read_list_8 ctx read_class_field_ref;
			a.a_resolve <- read_option ctx read_class_field_ref
		| _ ->
			assert false
end

module HxbModule = struct
	open HxbRead
	open HxbTypeRef

	let read_module_check_policy ctx =
		match IO.read_byte ctx.ch with
		| 0 -> NoCheckFileTimeModification
		| 1 -> CheckFileContentModification
		| 2 -> NoCheckDependencies
		| 3 -> NoCheckShadowing
		| _ -> assert false

	let read_module_kind ctx =
		match IO.read_byte ctx.ch with
		| 0 -> MCode
		| 1 -> MMacro
		| 2 -> MFake
		| 3 -> MExtern
		| 4 -> MImport
		| _ -> assert false

	let read_module1 ctx =
		let id = IO.read_i32 ctx.ch in
		let path = read_dot_path ctx in
		let file = read_string ctx in
		let sign = read_string ctx in
		let policy = read_list_8 ctx read_module_check_policy in
		let time = IO.read_double ctx.ch in
		let kind = read_module_kind ctx in
		let extra = module_extra file sign time kind policy in
		(* dirty *)
		extra.m_added <- IO.read_i32 ctx.ch;
		extra.m_mark <- IO.read_i32 ctx.ch;
		(* deps *)
		extra.m_processed <- IO.read_i32 ctx.ch;
		{
			m_id = id;
			m_path = path;
			m_types = [];
			m_extra = extra;
		}

	let read_module_type_ref ctx =
		match IO.read_byte ctx.ch with
		| 0 -> TClassDecl (read_class_ref ctx)
		| 1 -> TEnumDecl (read_enum_ref ctx)
		| 2 -> TTypeDecl (read_typedef_ref ctx)
		| 3 -> TAbstractDecl (read_abstract_ref ctx)
		| _ -> assert false

	let read_module2 ctx m =
		m.m_types <- read_list_8 ctx read_module_type_ref;
		(* binded_res *)
		(* macro calls *)
		m.m_extra.m_if_feature <- read_list_8 ctx (fun _ ->
			let s = read_string ctx in
			let c = read_class_ref ctx in
			let cf = read_class_field_ref ctx in
			let b = read_bool ctx in
			(s,(c,cf,b))
		);
		m.m_extra.m_features <- read_hashtbl ctx read_string read_bool
end

module HxbTables = struct
	open HxbRead

	let read_head_table ctx =
		let version = IO.read_byte ctx.ch in
		if version <> hxb_version then failwith (Printf.sprintf "Version mismatch: data has version %i, reader has version %i" version hxb_version);
		ctx.full_read <- (match IO.read_byte ctx.ch with
			| 0 -> false
			| 1 -> true
			| _ -> assert false);
		if not ctx.full_read then failwith "Only full read is currently supported";
		let major = IO.read_byte ctx.ch in
		let minor = IO.read_byte ctx.ch in
		let patch = IO.read_byte ctx.ch in
		ctx.texpr_positions <- (match IO.read_byte ctx.ch with
			| 0 -> NoPositions
			| 1 -> RelativePositions
			| 2 -> AbsolutePositions
			| _ -> assert false
		);
		ignore(major,minor,patch)

	let read_strings_table ctx =
		ctx.strings <- read_array ctx read_raw_string

	let read_file_table ctx =
		let open Lexer in
		let read_int_pair _ =
			let a = IO.read_i32 ctx.ch in
			let b = IO.read_i32 ctx.ch in
			(a,b)
		in
		let _ = read_list ctx (fun _ ->
			let lfile = read_string ctx in
			let lline = IO.read_i32 ctx.ch in
			let llines = read_list ctx read_int_pair in
			let lstrings = read_list ctx (fun _ -> IO.read_i32 ctx.ch) in
			let file = {
				lfile = lfile;
				lline = lline;
				lmaxline = lline;
				llines = llines;
				lalines = Array.of_list llines;
				lstrings = lstrings;
				llast = max_int;
				llastindex = 0;
			} in
			Hashtbl.replace Lexer.all_files lfile file
		) in
		()

	let read_module_table1 ctx =
		ctx.modules <- read_array ctx HxbModule.read_module1

	let read_class_table1 ctx =
		ctx.classes <- read_array ctx (fun _ -> HxbModuleType.read_class1 ctx false)

	let read_enum_table1 ctx =
		ctx.enums <- read_array ctx (fun _ -> HxbModuleType.read_enum1 ctx)

	let read_typedef_table1 ctx =
		ctx.typedefs <- read_array ctx (fun _ -> HxbModuleType.read_typedef1 ctx)

	let read_abstract_table1 ctx =
		ctx.abstracts <- read_array ctx (fun _ -> HxbModuleType.read_abstract1 ctx)

	let read_class_field_table ctx =
		ctx.class_fields <- read_array ctx (fun _ ->
			let name = read_string ctx in
			let p = read_position ctx in
			let pn = read_position ctx in
			{
				null_field with
				cf_name = name;
				cf_pos = p;
				cf_name_pos = pn;
			}
		)

	let read_enum_field_table ctx =
		ctx.enum_fields <- read_array ctx (fun _ ->
			let name = read_string ctx in
			let p = read_position ctx in
			let pn = read_position ctx in
			let doc = read_option ctx read_string in
			let i = IO.read_i32 ctx.ch in
			{
				null_enum_field with
				ef_name = name;
				ef_pos = p;
				ef_name_pos = pn;
				ef_doc = doc;
				ef_index = i
			}
		)

	let read_anon_field_table ctx =
		ctx.anon_fields <- read_array ctx (fun _ -> {
			null_field with
			cf_name = read_string ctx;
		})

	let read_anon_table1 ctx =
		let read_anon_status ctx =
			match IO.read_byte ctx.ch with
			| 0 -> Closed
			| 1 -> Opened
			| 2 -> Const
			| 3 -> Extend (HxbTypeRef.read_typeref_params ctx)
			| 4 -> Statics (HxbTypeRef.read_class_ref ctx)
			| 5 -> EnumStatics (HxbTypeRef.read_enum_ref ctx)
			| 6 -> AbstractStatics (HxbTypeRef.read_abstract_ref ctx)
			| _ -> assert false
		in
		ctx.anons <- read_array ctx (fun _ -> {
			a_status = ref (read_anon_status ctx);
			a_fields = PMap.empty;
		})

	let read_module_types_table ctx =
		ctx.state.types <- read_list ctx HxbModule.read_module_type_ref

	let read_module_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let m = ctx.modules.(i) in
			HxbModule.read_module2 ctx m;
		done

	let read_anon_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			match IO.read_byte ctx.ch with
			| 0 | 1 | 2 ->
				()
			| 3 ->
		let read_anon_field _ =
			let cf = HxbTypeRef.read_anon_field_ref ctx in
			HxbField.read_class_field_data ctx cf;
			cf
		in
			let an = ctx.anons.(i) in
			an.a_fields <- list_to_pmap (read_list_8 ctx read_anon_field) (fun cf -> cf.cf_name)
			| _ ->
				assert false
		done

	let read_class_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let c = ctx.classes.(i) in
			HxbModuleType.read_class2 ctx c
		done

	let read_enum_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let en = ctx.enums.(i) in
			HxbModuleType.read_enum2 ctx en
		done

	let read_typedef_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let td = ctx.typedefs.(i) in
			HxbModuleType.read_typedef2 ctx td
		done

	let read_abstract_table2 ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let a = ctx.abstracts.(i) in
			HxbModuleType.read_abstract2 ctx a
		done

	let read_expr_table ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let cf = ctx.class_fields.(i) in
			cf.cf_expr <- read_option ctx HxbTexpr.read_texpr_init;
		done

	let read_init_table ctx =
		let length = IO.read_i32 ctx.ch in
		for i = 0 to length - 1 do
			let c = HxbTypeRef.read_class_ref ctx in
			c.cl_init <- Some (HxbTexpr.read_texpr_init ctx)
		done

	let read_main_table ctx =
		ctx.state.main_class <- Some (HxbRead.read_dot_path ctx);
		ctx.state.main <- Some (HxbTexpr.read_texpr_init ctx)

	let read_include_file_table ctx =
		ctx.state.include_files <- read_list_8 ctx (fun _ ->
			let s1 = read_string ctx in
			let s2 = read_raw_string ctx in
			(s1,s2)
		)

	let read_resource_table ctx =
		ctx.state.resources <- read_hashtbl ctx read_string read_raw_string;
end

open HxbTables

let read file =
	HxbTexpr.read_class := (fun ctx is_tp ->
		let c = HxbModuleType.read_class1 ctx is_tp in
		HxbModuleType.read_class2 ctx c;
		c
	);
	let ch = IO.input_channel (open_in_bin file) in
	begin match IO.nread_string ch 3 with
		| "HXB" -> ()
		| _ -> failwith "Invalid HXB format";
	end;
	let ctx = {
		ch = ch;
		full_read = true;
		texpr_positions = RelativePositions;
		strings = Array.make 0 "";
		classes = Array.make 0 null_class;
		enums = Array.make 0 null_enum;
		typedefs = Array.make 0 null_typedef;
		abstracts = Array.make 0 null_abstract;
		class_fields = Array.make 0 null_field;
		enum_fields = Array.make 0 null_enum_field;
		anon_fields = Array.make 0 null_field;
		anons = Array.make 0 ({ a_status = ref Opened; a_fields = PMap.empty });
		modules = Array.make 0 null_module;
		vars = Array.make 0 null_tvar;
		file = "";
		curpmin = 0;
		curpmax = 0;
		state = {
			modules = [];
			types = [];
			main = None;
			main_class = None;
			include_files = [];
			resources = Hashtbl.create 0;
		}
	} in
	let total = ref 3 in
	let rec loop () =
		let s = IO.nread_string ch 4 in
		let l = IO.read_i32 ch in
		total := !total + l;
		(* print_string (Printf.sprintf "Reading %s (%i bytes)... " s l); *)
		begin match s with
			| "HEAD" -> read_head_table ctx
			| "STRS" -> read_strings_table ctx
			| "FILE" -> read_file_table ctx
			| "MOD1" -> read_module_table1 ctx
			| "CLS1" -> read_class_table1 ctx
			| "TDF1" -> read_typedef_table1 ctx
			| "ENM1" -> read_enum_table1 ctx
			| "ABS1" -> read_abstract_table1 ctx
			| "ANO1" -> read_anon_table1 ctx
			| "TORD" -> read_module_types_table ctx;
			| "CLSF" -> read_class_field_table ctx
			| "ENMF" -> read_enum_field_table ctx
			| "ANOF" -> read_anon_field_table ctx
			| "MOD2" -> read_module_table2 ctx
			| "CLS2" -> read_class_table2 ctx
			| "TDF2" -> read_typedef_table2 ctx
			| "ENM2" -> read_enum_table2 ctx
			| "ABS2" -> read_abstract_table2 ctx
			| "ANO2" -> read_anon_table2 ctx
			| "EXPR" -> read_expr_table ctx
			| "INIT" -> read_init_table ctx
			| "MAIN" -> read_main_table ctx
			| "INCF" -> read_include_file_table ctx
			| "RSRC" -> read_resource_table ctx
			| "END " -> raise Exit
			| _ -> ignore(IO.nread ch l)
		end;
		(* print_endline ("done!"); *)
		loop()
	in
	try
		loop()
	with Exit ->
		(* print_endline ("done!"); *)
		(* print_endline (Printf.sprintf "Total: %i bytes" !total); *)
		ctx.state.modules <- Array.to_list ctx.modules;
		ctx.state