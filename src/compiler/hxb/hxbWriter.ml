open Globals
open Ast
open Type
open Common
open HxbData
open HxbWriterPool

type context = {
	mutable ch      : bytes IO.output;
	pool            : Pool.t;
	files           : (string,bool) Hashtbl.t;
	mutable curpmin : int;
	mutable curpmax : int;
	texpr_positions : texpr_position_mode;
}

let create_table ctx =
	let ch = IO.output_bytes () in
	let old = ctx.ch in
	ctx.ch <- ch;
	(fun () ->
		ctx.ch <- old;
		IO.close_out ch;
	)

module HxbWrite = struct
	let write_list ctx f l =
		IO.write_i32 ctx.ch (List.length l);
		List.iter (f ctx) l

	let write_list_8 ctx f l =
		IO.write_byte ctx.ch (List.length l);
		List.iter (f ctx) l

	let write_hashtbl ctx fk fv h =
		let l = Hashtbl.fold (fun k v acc -> (k,v) :: acc) h [] in
		write_list ctx (fun _ (k,v) ->
			fk k;
			fv v;
		) l

	let write_lut ctx i =
		IO.write_i32 ctx.ch i

	let write_string ctx s =
		write_lut ctx (Pool.add_string ctx.pool s)

	let write_raw_string ctx s =
		IO.write_i32 ctx.ch (String.length s);
		IO.nwrite_string ctx.ch s

	let write_dot_path ctx path =
		let i,(k,il) = Pool.add_path ctx.pool path in
		IO.write_byte ctx.ch k;
		let rec loop il = match il with
			| [] -> ()
			| i :: il ->
				loop il;
				write_lut ctx i
		in
		loop il;
		write_lut ctx i

	let write_bool ctx b =
		IO.write_byte ctx.ch (if b then 1 else 0)

	let write_option ctx f = function
		| None -> IO.write_byte ctx.ch 0
		| Some x ->
			IO.write_byte ctx.ch 1;
			f ctx x

	let write_file_name ctx s =
		Hashtbl.replace ctx.files s true;
		write_string ctx s

	let write_position ctx p =
		write_file_name ctx p.pfile;
		IO.write_i32 ctx.ch p.pmin;
		IO.write_i32 ctx.ch p.pmax

	let write_position2 ctx p =
		IO.write_i32 ctx.ch p.pmin;
		IO.write_i32 ctx.ch p.pmax
end

module HxbTypeRef = struct
	open HxbWrite

	let write_class_field_ref c ctx cf =
		write_lut ctx (Pool.add_class_field ctx.pool c cf)

	let write_enum_field_ref en ctx ef =
		write_lut ctx (Pool.add_enum_field ctx.pool en ef)

	let write_anon_field_ref ctx cf =
		write_lut ctx (Pool.add_anon_field ctx.pool cf)

	let write_class_ref ctx c =
		write_lut ctx (Pool.add_class ctx.pool c)

	let write_enum_ref ctx en =
		write_lut ctx (Pool.add_enum ctx.pool en)

	let write_typedef_ref ctx td =
		write_lut ctx (Pool.add_typedef ctx.pool td)

	let write_abstract_ref ctx a =
		write_lut ctx (Pool.add_abstract ctx.pool a)

	let rec write_typeref_params ctx tl =
		write_list_8 ctx write_typeref tl

	and write_typeref ctx t = match t with
		| TDynamic t1 ->
			if t1 == t_dynamic then
				IO.write_byte ctx.ch 0
			else begin
				IO.write_byte ctx.ch 1;
				write_typeref ctx t1
			end
		| TInst(c,[]) ->
			IO.write_byte ctx.ch 2;
			write_class_ref ctx c;
		| TInst (c,tl) ->
			IO.write_byte ctx.ch 3;
			write_class_ref ctx c;
			write_typeref_params ctx tl;
		| TEnum (en,[]) ->
			IO.write_byte ctx.ch 4;
			write_enum_ref ctx en;
		| TEnum (en,tl) ->
			IO.write_byte ctx.ch 5;
			write_enum_ref ctx en;
			write_typeref_params ctx tl;
		| TType (td,[]) ->
			IO.write_byte ctx.ch 6;
			write_typedef_ref ctx td;
		| TType (td,tl) ->
			IO.write_byte ctx.ch 7;
			write_typedef_ref ctx td;
			write_typeref_params ctx tl;
		| TAbstract (a,[]) ->
			IO.write_byte ctx.ch 8;
			write_abstract_ref ctx a;
		| TAbstract (a,tl) ->
			IO.write_byte ctx.ch 9;
			write_abstract_ref ctx a;
			write_typeref_params ctx tl;
		| TAnon an ->
			IO.write_byte ctx.ch 10;
			write_lut ctx (Pool.add_anon ctx.pool an);
		| TFun ([],tr) ->
			IO.write_byte ctx.ch 11;
			write_typeref ctx tr;
		| TFun (tl,tr) ->
			IO.write_byte ctx.ch 12;
			let write_fun_arg _ (n,o,t) =
				write_string ctx n;
				write_bool ctx o;
				write_typeref ctx t
			in
			write_list_8 ctx write_fun_arg tl;
			write_typeref ctx tr
		| TMono r ->
			begin match !r with
				| Some t -> write_typeref ctx t
				| None -> IO.write_byte ctx.ch 0
			end
		| TLazy r ->
			begin match !r with
				| LAvailable t ->
					write_typeref ctx t
				| _ ->
					failwith "unresolved TLazy"
			end

	let write_class_ref_with_params ctx c tl =
		write_class_ref ctx c;
		write_typeref_params ctx tl
end

module HxbExpr = struct
	open HxbWrite
	open HxbTypeRef

	let rec write_binop ctx = function
		| OpAdd -> IO.write_byte ctx.ch 0
		| OpMult -> IO.write_byte ctx.ch 1
		| OpDiv -> IO.write_byte ctx.ch 2
		| OpSub -> IO.write_byte ctx.ch 3
		| OpAssign -> IO.write_byte ctx.ch 4
		| OpEq -> IO.write_byte ctx.ch 5
		| OpNotEq -> IO.write_byte ctx.ch 6
		| OpGt -> IO.write_byte ctx.ch 7
		| OpGte -> IO.write_byte ctx.ch 8
		| OpLt -> IO.write_byte ctx.ch 9
		| OpLte -> IO.write_byte ctx.ch 10
		| OpAnd -> IO.write_byte ctx.ch 11
		| OpOr -> IO.write_byte ctx.ch 12
		| OpXor -> IO.write_byte ctx.ch 13
		| OpBoolAnd -> IO.write_byte ctx.ch 14
		| OpBoolOr -> IO.write_byte ctx.ch 15
		| OpShl -> IO.write_byte ctx.ch 16
		| OpShr -> IO.write_byte ctx.ch 17
		| OpUShr -> IO.write_byte ctx.ch 18
		| OpMod -> IO.write_byte ctx.ch 19
		| OpAssignOp op ->
			IO.write_byte ctx.ch 20;
			write_binop ctx op
		| OpInterval -> IO.write_byte ctx.ch 21
		| OpArrow -> IO.write_byte ctx.ch 22
		| OpIn -> IO.write_byte ctx.ch 23

	let write_unop ctx op =
		let i = match op with
			| Increment -> 0
			| Decrement -> 1
			| Not -> 2
			| Neg -> 3
			| NegBits -> 4
		in
		IO.write_byte ctx.ch i

	let write_object_field_key ctx (n,p,qs) =
		write_string ctx n;
		write_position ctx p;
		begin match qs with
			| NoQuotes -> IO.write_byte ctx.ch 0
			| DoubleQuotes -> IO.write_byte ctx.ch 1
		end

	let rec write_type_path ctx tp =
		write_list_8 ctx write_string tp.tpackage;
		write_string ctx tp.tname;
		write_list_8 ctx write_type_param_or_const tp.tparams;
		write_option ctx write_string tp.tsub

	and write_placed_type_path ctx (tp,p) =
		write_type_path ctx tp;
		write_position ctx p

	and write_type_param_or_const ctx = function
		| TPType th ->
			IO.write_byte ctx.ch 0;
			write_type_hint ctx th
		| TPExpr e ->
			IO.write_byte ctx.ch 1;
			write_expr ctx e

	and write_complex_type ctx = function
		| CTPath tp ->
			IO.write_byte ctx.ch 0;
			write_type_path ctx tp
		| CTFunction(thl,th) ->
			IO.write_byte ctx.ch 1;
			write_list_8 ctx write_type_hint thl;
			write_type_hint ctx th
		| CTAnonymous cffl ->
			IO.write_byte ctx.ch 2;
			write_list_8 ctx write_class_field cffl;
		| CTParent th ->
			IO.write_byte ctx.ch 3;
			write_type_hint ctx th
		| CTExtend(ptp,cffl) ->
			IO.write_byte ctx.ch 4;
			write_list_8 ctx write_placed_type_path ptp;
			write_list_8 ctx write_class_field cffl;
		| CTOptional th ->
			IO.write_byte ctx.ch 5;
			write_type_hint ctx th
		| CTNamed(pn,th) ->
			IO.write_byte ctx.ch 6;
			write_placed_name ctx pn;
			write_type_hint ctx th

	and write_type_hint ctx (ct,p) =
		write_complex_type ctx ct;
		write_position ctx p

	and write_type_param ctx tp =
		write_placed_name ctx tp.tp_name;
		write_list_8 ctx write_type_param tp.tp_params;
		write_list_8 ctx write_type_hint tp.tp_constraints;
		write_list_8 ctx write_metadata tp.tp_meta

	and write_func_arg ctx (pn,b,meta,tho,eo) =
		write_placed_name ctx pn;
		write_bool ctx b;
		write_list_8 ctx write_metadata meta;
		write_option ctx write_type_hint tho;
		write_option ctx write_expr eo

	and write_func ctx f =
		write_list_8 ctx write_type_param f.f_params;
		write_list_8 ctx write_func_arg f.f_args;
		write_option ctx write_type_hint f.f_type;
		write_option ctx write_expr f.f_expr

	and write_placed_name ctx (s,p) =
		write_string ctx s;
		write_position ctx p

	and write_metadata ctx (m,el,p) =
		write_string ctx (Meta.to_string m);
		write_list_8 ctx write_expr el;
		write_position ctx p

	and write_access ctx ac =
		let i = match ac with
			| APublic -> 0
			| APrivate -> 1
			| AStatic -> 2
			| AOverride -> 3
			| ADynamic -> 4
			| AInline -> 5
			| AMacro -> 6
			| AFinal -> 7
		in
		IO.write_byte ctx.ch i

	and write_class_field_kind ctx = function
		| FVar(tho,eo) ->
			IO.write_byte ctx.ch 0;
			write_option ctx write_type_hint tho;
			write_option ctx write_expr eo;
		| FFun f ->
			IO.write_byte ctx.ch 1;
			write_func ctx f
		| FProp(pn1,pn2,tho,eo) ->
			IO.write_byte ctx.ch 2;
			write_placed_name ctx pn1;
			write_placed_name ctx pn2;
			write_option ctx write_type_hint tho;
			write_option ctx write_expr eo

	and write_class_field ctx cff =
		write_placed_name ctx cff.cff_name;
		write_option ctx write_string cff.cff_doc;
		write_position ctx cff.cff_pos;
		write_list_8 ctx write_metadata cff.cff_meta;
		write_list_8 ctx write_access cff.cff_access;
		write_class_field_kind ctx cff.cff_kind

	and write_expr ctx (e,p) =
		write_position2 ctx p;
		match e with
		| EConst (Int s) ->
			IO.write_byte ctx.ch 0;
			write_string ctx s;
		| EConst (Float s) ->
			IO.write_byte ctx.ch 1;
			write_string ctx s;
		| EConst (String s) ->
			IO.write_byte ctx.ch 2;
			write_string ctx s;
		| EConst (Ident s) ->
			IO.write_byte ctx.ch 3;
			write_string ctx s;
		| EConst (Regexp(s1,s2)) ->
			IO.write_byte ctx.ch 4;
			write_string ctx s1;
			write_string ctx s2;
		| EArray(e1,e2) ->
			IO.write_byte ctx.ch 5;
			write_expr ctx e1;
			write_expr ctx e2;
		| EBinop(op,e1,e2) ->
			IO.write_byte ctx.ch 6;
			write_binop ctx op;
			write_expr ctx e1;
			write_expr ctx e2;
		| EField(e1,s) ->
			IO.write_byte ctx.ch 7;
			write_expr ctx e1;
			write_string ctx s;
		| EParenthesis e1 ->
			IO.write_byte ctx.ch 8;
			write_expr ctx e1
		| EObjectDecl fl ->
			IO.write_byte ctx.ch 9;
			let write_field _ (k,e1) =
				write_object_field_key ctx k;
				write_expr ctx e1
			in
			write_list_8 ctx write_field fl;
		| EArrayDecl el ->
			IO.write_byte ctx.ch 10;
			write_list_8 ctx write_expr el;
		| ECall(e1,el) ->
			IO.write_byte ctx.ch 11;
			write_expr ctx e1;
			write_list_8 ctx write_expr el
		| ENew(ptp,el) ->
			IO.write_byte ctx.ch 12;
			write_placed_type_path ctx ptp;
			write_list_8 ctx write_expr el;
		| EUnop(op,Prefix,e1) ->
			IO.write_byte ctx.ch 13;
			write_unop ctx op;
			write_expr ctx e1;
		| EUnop(op,Postfix,e1) ->
			IO.write_byte ctx.ch 14;
			write_unop ctx op;
			write_expr ctx e1;
		| EVars vl ->
			IO.write_byte ctx.ch 15;
			let write_var _ (pn,tho,eo) =
				write_placed_name ctx pn;
				write_option ctx write_type_hint tho;
				write_option ctx write_expr eo
			in
			write_list_8 ctx write_var vl
		| EFunction(so,f) ->
			IO.write_byte ctx.ch 16;
			write_option ctx write_string so;
			write_func ctx f;
		| EBlock el ->
			IO.write_byte ctx.ch 17;
			write_list ctx write_expr el
		| EFor(e1,e2) ->
			IO.write_byte ctx.ch 18;
			write_expr ctx e1;
			write_expr ctx e2;
		| EIf(e1,e2,None) ->
			IO.write_byte ctx.ch 19;
			write_expr ctx e1;
			write_expr ctx e2;
		| EIf(e1,e2,Some e3) ->
			IO.write_byte ctx.ch 20;
			write_expr ctx e1;
			write_expr ctx e2;
			write_expr ctx e3;
		| EWhile(e1,e2,NormalWhile) ->
			IO.write_byte ctx.ch 21;
			write_expr ctx e1;
			write_expr ctx e2;
		| EWhile(e1,e2,DoWhile) ->
			IO.write_byte ctx.ch 22;
			write_expr ctx e1;
			write_expr ctx e2;
		| ESwitch(e1,cases,def) ->
			IO.write_byte ctx.ch 23;
			write_expr ctx e1;
			let write_case _ (el,eg,eo,p) =
				write_list_8 ctx write_expr el;
				write_option ctx write_expr eg;
				write_option ctx write_expr eo;
				write_position2 ctx p;
			in
			write_list ctx write_case cases;
			let write_default _ (eo,p) =
				write_option ctx write_expr eo;
				write_position2 ctx p
			in
			write_option ctx write_default def;
		| ETry(e1,catches) ->
			IO.write_byte ctx.ch 24;
			write_expr ctx e1;
			let write_catch _ (pn,th,e,p) =
				write_placed_name ctx pn;
				write_type_hint ctx th;
				write_expr ctx e;
				write_position ctx p;
			in
			write_list_8 ctx write_catch catches;
		| EReturn None ->
			IO.write_byte ctx.ch 25;
		| EReturn (Some e1) ->
			IO.write_byte ctx.ch 26;
			write_expr ctx e1;
		| EBreak ->
			IO.write_byte ctx.ch 27;
		| EContinue ->
			IO.write_byte ctx.ch 28;
		| EUntyped e1 ->
			IO.write_byte ctx.ch 29;
			write_expr ctx e1;
		| EThrow e1 ->
			IO.write_byte ctx.ch 30;
			write_expr ctx e1;
		| ECast(e1,None) ->
			IO.write_byte ctx.ch 31;
			write_expr ctx e1;
		| ECast(e1,Some th) ->
			IO.write_byte ctx.ch 32;
			write_expr ctx e1;
			write_type_hint ctx th;
		| EDisplay(e1,b) ->
			IO.write_byte ctx.ch 33;
			write_expr ctx e1;
			write_bool ctx b;
		| EDisplayNew _ ->
			assert false
		| ETernary(e1,e2,e3) ->
			IO.write_byte ctx.ch 34;
			write_expr ctx e1;
			write_expr ctx e2;
			write_expr ctx e3;
		| ECheckType(e1,th) ->
			IO.write_byte ctx.ch 35;
			write_expr ctx e1;
			write_type_hint ctx th;
		| EMeta(m,e1) ->
			IO.write_byte ctx.ch 36;
			write_metadata ctx m;
			write_expr ctx e1
end

module HxbTexpr = struct
	open HxbWrite
	open HxbExpr
	open HxbTypeRef

	let write_class : (context -> tclass -> unit) ref = ref (fun _ _ -> assert false)

	let write_type_parameter ctx (s,t) =
		write_string ctx s;
		match follow t with
		| TInst(c,[]) ->
			(!write_class) ctx c;
		| _ ->
			assert false

	let write_tconstant ctx = function
		| TBool false ->
			IO.write_byte ctx.ch 0;
		| TBool true ->
			IO.write_byte ctx.ch 1;
		| TNull ->
			IO.write_byte ctx.ch 2;
		| TThis ->
			IO.write_byte ctx.ch 3;
		| TSuper ->
			IO.write_byte ctx.ch 4
		| TInt i32 ->
			IO.write_byte ctx.ch 5;
			IO.write_real_i32 ctx.ch i32
		| TFloat f ->
			IO.write_byte ctx.ch 6;
			write_string ctx f;
		| TString s ->
			IO.write_byte ctx.ch 7;
			write_string ctx s

	let write_tvar_ref ctx v =
		write_lut ctx (Pool.add_var ctx.pool v)

	let write_tfield_access ctx = function
		| FInstance(c,tl,cf) ->
			IO.write_byte ctx.ch 0;
			write_class_ref ctx c;
			write_typeref_params ctx tl;
			write_class_field_ref c ctx cf
		| FStatic(c,cf) ->
			IO.write_byte ctx.ch 1;
			write_class_ref ctx c;
			write_class_field_ref c ctx cf
		| FAnon cf ->
			IO.write_byte ctx.ch 2;
			write_anon_field_ref ctx cf
		| FDynamic s ->
			IO.write_byte ctx.ch 3;
			write_string ctx s;
		| FClosure(None,cf) ->
			IO.write_byte ctx.ch 4;
			write_anon_field_ref ctx cf
		| FClosure(Some(c,tl),cf) ->
			IO.write_byte ctx.ch 5;
			write_class_ref_with_params ctx c tl;
			write_class_field_ref c ctx cf;
		| FEnum(en,ef) ->
			IO.write_byte ctx.ch 6;
			write_enum_ref ctx en;
			write_enum_field_ref en ctx ef

	let rec write_tvar_def ctx v =
		write_lut ctx (Pool.add_var ctx.pool v);
		IO.write_i32 ctx.ch v.v_id;
		write_string ctx v.v_name;
		write_typeref ctx v.v_type;
		write_bool ctx v.v_capture;
		write_option ctx (fun _ (tp,eo) ->
			write_list_8 ctx write_type_parameter tp;
			write_option ctx write_texpr eo;
		) v.v_extra;
		write_list_8 ctx write_metadata v.v_meta;
		write_position ctx v.v_pos

	and write_texpr ctx e =
		begin match ctx.texpr_positions with
			| RelativePositions ->
				let dmin = e.epos.pmin - ctx.curpmin in
				let dmax = e.epos.pmax - ctx.curpmax in
				ctx.curpmin <- e.epos.pmin;
				ctx.curpmax <- e.epos.pmax;
				IO.write_i16 ctx.ch dmin;
				IO.write_i16 ctx.ch dmax;
			| AbsolutePositions ->
				IO.write_i32 ctx.ch e.epos.pmin;
				IO.write_i32 ctx.ch e.epos.pmax
			| NoPositions ->
				()
		end;
		write_typeref ctx e.etype;
		match e.eexpr with
		| TConst ct ->
			IO.write_byte ctx.ch 0;
			write_tconstant ctx ct;
		| TLocal v ->
			IO.write_byte ctx.ch 1;
			write_tvar_ref ctx v;
		| TArray(e1,e2) ->
			IO.write_byte ctx.ch 2;
			write_texpr ctx e1;
			write_texpr ctx e2;
		| TBinop(op,e1,e2) ->
			IO.write_byte ctx.ch 3;
			write_binop ctx op;
			write_texpr ctx e1;
			write_texpr ctx e2;
		| TField(e1,fa) ->
			IO.write_byte ctx.ch 4;
			write_texpr ctx e1;
			write_tfield_access ctx fa;
		| TTypeExpr mt ->
			begin match mt with
			| TClassDecl c ->
				IO.write_byte ctx.ch 5;
				write_class_ref ctx c;
			| TEnumDecl en ->
				IO.write_byte ctx.ch 6;
				write_enum_ref ctx en;
			| TTypeDecl td ->
				IO.write_byte ctx.ch 7;
				write_typedef_ref ctx td
			| TAbstractDecl a ->
				IO.write_byte ctx.ch 8;
				write_abstract_ref ctx a
			end
		| TParenthesis e1 ->
			IO.write_byte ctx.ch 9;
			write_texpr ctx e1;
		| TObjectDecl fl ->
			IO.write_byte ctx.ch 10;
			let write_field _ (k,e1) =
				write_object_field_key ctx k;
				write_texpr ctx e1
			in
			write_list_8 ctx write_field fl;
		| TArrayDecl el ->
			IO.write_byte ctx.ch 11;
			write_list_8 ctx write_texpr el;
		| TCall(e1,el) ->
			IO.write_byte ctx.ch 12;
			write_texpr ctx e1;
			write_list_8 ctx write_texpr el;
		| TNew(c,tl,el) ->
			IO.write_byte ctx.ch 13;
			write_class_ref_with_params ctx c tl;
			write_list_8 ctx write_texpr el;
		| TUnop(op,flag,e1) ->
			IO.write_byte ctx.ch (match flag with Prefix -> 14 | Postfix -> 15);
			write_unop ctx op;
			write_texpr ctx e1;
		| TFunction tf ->
			IO.write_byte ctx.ch 16;
			write_list_8 ctx (fun _ (v,cto) ->
				write_tvar_def ctx v;
				write_option ctx write_tconstant cto;
			) tf.tf_args;
			write_typeref ctx tf.tf_type;
			write_texpr ctx tf.tf_expr
		| TVar(v,eo) ->
			IO.write_byte ctx.ch 17;
			write_tvar_def ctx v;
			write_option ctx write_texpr eo;
		| TBlock el ->
			IO.write_byte ctx.ch 18;
			write_list ctx write_texpr el;
		| TFor(v,e1,e2) ->
			IO.write_byte ctx.ch 19;
			write_tvar_def ctx v;
			write_texpr ctx e1;
			write_texpr ctx e2;
		| TIf(e1,e2,None) ->
			IO.write_byte ctx.ch 20;
			write_texpr ctx e1;
			write_texpr ctx e2;
		| TIf(e1,e2,Some e3) ->
			IO.write_byte ctx.ch 21;
			write_texpr ctx e1;
			write_texpr ctx e2;
			write_texpr ctx e3;
		| TWhile(e1,e2,flag) ->
			IO.write_byte ctx.ch (match flag with NormalWhile -> 22 | DoWhile -> 23);
			write_texpr ctx e1;
			write_texpr ctx e2;
		| TSwitch(e1,cases,edef) ->
			IO.write_byte ctx.ch 24;
			write_texpr ctx e1;
			write_list ctx (fun _ (el,e) ->
				write_list_8 ctx write_texpr el;
				write_texpr ctx e;
			) cases;
			write_option ctx write_texpr edef;
		| TTry(e1,catches) ->
			IO.write_byte ctx.ch 25;
			write_texpr ctx e1;
			write_list_8 ctx (fun _ (v,e) ->
				write_tvar_def ctx v;
				write_texpr ctx e;
			) catches;
		| TReturn None ->
			IO.write_byte ctx.ch 26
		| TReturn (Some e1) ->
			IO.write_byte ctx.ch 27;
			write_texpr ctx e1;
		| TBreak ->
			IO.write_byte ctx.ch 28;
		| TContinue ->
			IO.write_byte ctx.ch 29;
		| TThrow e1 ->
			IO.write_byte ctx.ch 30;
			write_texpr ctx e1;
		| TCast(e1,None) ->
			IO.write_byte ctx.ch 31;
			write_texpr ctx e1;
		| TCast(e1,Some (TClassDecl c)) ->
			IO.write_byte ctx.ch 32;
			write_texpr ctx e1;
			write_class_ref ctx c;
		| TCast(e1,Some (TEnumDecl en)) ->
			IO.write_byte ctx.ch 33;
			write_texpr ctx e1;
			write_enum_ref ctx en;
		| TCast(e1,Some (TTypeDecl td)) ->
			IO.write_byte ctx.ch 34;
			write_texpr ctx e1;
			write_typedef_ref ctx td;
		| TCast(e1,Some (TAbstractDecl a)) ->
			IO.write_byte ctx.ch 35;
			write_texpr ctx e1;
			write_abstract_ref ctx a;
		| TMeta(m1,e1) ->
			IO.write_byte ctx.ch 36;
			write_metadata ctx m1;
			write_texpr ctx e1;
		| TEnumParameter(e1,ef,i) ->
			IO.write_byte ctx.ch 37;
			write_texpr ctx e1;
			let rec loop t = match follow t with
				| TEnum(en,_) -> en
				| TFun(_,tr) -> loop tr
				| _ -> assert false
			in
			(* TODO: Have to find the enum type here... pretty stupid, can we avoid it? *)
			let en = loop ef.ef_type in
			write_enum_field_ref en ctx ef;
			IO.write_i32 ctx.ch i;
		| TEnumIndex e1 ->
			IO.write_byte ctx.ch 38;
			write_texpr ctx e1;
		| TIdent s ->
			IO.write_byte ctx.ch 39;
			write_string ctx s

	let write_texpr ctx e =
		Pool.init_expr_context ctx.pool;
		ctx.curpmin <- e.epos.pmin;
		ctx.curpmax <- e.epos.pmax;
		let close = create_table ctx in
		write_texpr ctx e;
		let expr = close() in
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.vars);
		write_position ctx e.epos;
		IO.nwrite ctx.ch expr
end

module HxbField = struct
	open HxbWrite
	open HxbExpr
	open HxbTexpr
	open HxbTypeRef

	let write_class_field_data ctx cf =
		write_typeref ctx cf.cf_type;
		write_bool ctx cf.cf_public;
		write_option ctx write_string cf.cf_doc;
		write_list_8 ctx write_metadata cf.cf_meta;
		write_list_8 ctx write_type_parameter cf.cf_params;
		match cf.cf_kind with
		| Method meth ->
			let i = match meth with
			| MethNormal -> 0
			| MethInline -> 1
			| MethDynamic -> 2
			| MethMacro -> 3
			in
			IO.write_byte ctx.ch i
		| Var {v_read = r; v_write = w} ->
			match r,w with
			| AccNormal,AccNormal -> IO.write_byte ctx.ch 4
			| AccNormal,AccNo -> IO.write_byte ctx.ch 5
			| AccNormal,AccNever -> IO.write_byte ctx.ch 6
			| AccNormal,AccCtor -> IO.write_byte ctx.ch 7
			| AccNormal,AccResolve -> IO.write_byte ctx.ch 8
			| AccNormal,AccCall -> IO.write_byte ctx.ch 9
			| AccInline,AccNever -> IO.write_byte ctx.ch 10
			| AccCall,AccNever -> IO.write_byte ctx.ch 11
			| _ ->
				IO.write_byte ctx.ch 12;
				let write = function
					| AccNormal -> IO.write_byte ctx.ch 0
					| AccNo -> IO.write_byte ctx.ch 1
					| AccNever -> IO.write_byte ctx.ch 2
					| AccCtor -> IO.write_byte ctx.ch 3
					| AccResolve -> IO.write_byte ctx.ch 4
					| AccCall -> IO.write_byte ctx.ch 5
					| AccInline -> IO.write_byte ctx.ch 6
					| AccRequire(s,so) ->
						IO.write_byte ctx.ch 7;
						write_string ctx s;
						write_option ctx write_string so;
				in
				write r;
				write w

	let rec write_class_field ctx c cf : unit =
		write_class_field_ref c ctx cf;
		write_class_field_data ctx cf;
		(* TODO: cf_expr_unoptimized *)
		write_list ctx (fun _ cf -> write_class_field ctx c cf) cf.cf_overloads

	let write_enum_field ctx en ef =
		write_enum_field_ref en ctx ef;
		write_typeref ctx ef.ef_type;
		write_list_8 ctx write_type_parameter ef.ef_params;
		write_list_8 ctx write_metadata ef.ef_meta
end

module HxbModuleType = struct
	open HxbWrite
	open HxbTypeRef
	open HxbExpr
	open HxbTexpr
	open HxbField

	let write_class_kind ctx = function
		| KNormal ->
			IO.write_byte ctx.ch 0
		| KTypeParameter tl ->
			IO.write_byte ctx.ch 1;
			write_list_8 ctx write_typeref tl
		| KExpr e ->
			IO.write_byte ctx.ch 2;
			write_expr ctx e;
		| KGeneric ->
			IO.write_byte ctx.ch 3;
		| KGenericInstance(c,tl) ->
			IO.write_byte ctx.ch 4;
			write_class_ref ctx c;
			write_typeref_params ctx tl;
		| KMacroType ->
			IO.write_byte ctx.ch 5
		| KGenericBuild cffl ->
			IO.write_byte ctx.ch 6;
			write_list_8 ctx HxbExpr.write_class_field cffl;
		| KAbstractImpl a ->
			IO.write_byte ctx.ch 7;
			write_abstract_ref ctx a


	let write_module_infos ctx info =
		write_dot_path ctx info.mt_path;
		write_lut ctx (Pool.get_module ctx.pool info.mt_module);
		write_position ctx info.mt_pos;
		write_position ctx info.mt_name_pos;
		write_bool ctx info.mt_private;
		write_option ctx write_string info.mt_doc;
		write_list_8 ctx write_metadata info.mt_meta

	let write_class1 ctx c =
		write_module_infos ctx (Obj.magic c);
		write_bool ctx c.cl_extern;
		write_bool ctx c.cl_interface

	let write_class2 ctx c =
		write_list_8 ctx write_type_parameter c.cl_params;
		write_class_kind ctx c.cl_kind;
		write_option ctx (fun ctx (c,tl) ->
			write_class_ref ctx c;
			write_typeref_params ctx tl
		) c.cl_super;
		write_list_8 ctx (fun ctx (c,tl) ->
			write_class_ref ctx c;
			write_typeref_params ctx tl
		) c.cl_implements;
		write_list ctx (fun _ -> write_class_field ctx c) c.cl_ordered_statics;
		write_list ctx (fun _ -> write_class_field ctx c) c.cl_ordered_fields;
		write_option ctx write_typeref c.cl_dynamic;
		write_option ctx write_typeref c.cl_array_access;
		write_option ctx (fun _ -> write_class_field ctx c) c.cl_constructor;
		write_list ctx (write_class_field_ref c) c.cl_overrides;
		write_list ctx write_class_ref c.cl_descendants

	let write_typedef1 ctx td =
		write_module_infos ctx (Obj.magic td)

	let write_typedef2 ctx td =
		write_list_8 ctx write_type_parameter td.t_params;
		write_typeref ctx td.t_type

	let write_enum1 ctx en =
		write_module_infos ctx (Obj.magic en);
		write_typedef1 ctx en.e_type;
		write_bool ctx en.e_extern

	let write_enum2 ctx en =
		write_list_8 ctx write_type_parameter en.e_params;
		IO.write_byte ctx.ch (List.length en.e_names);
		List.iter (fun s ->
			let ef = PMap.find s en.e_constrs in
			HxbField.write_enum_field ctx en ef
		) en.e_names

	let write_abstract1 ctx a =
		write_module_infos ctx (Obj.magic a)

	let write_abstract2 ctx a =
		write_list_8 ctx write_type_parameter a.a_params;
		(* a_ops *)
		(* a_unops *)
		write_typeref ctx a.a_this;
		write_list_8 ctx write_typeref a.a_from;
		write_list_8 ctx write_typeref a.a_to;
		begin match a.a_impl with
			| Some c ->
				IO.write_byte ctx.ch 1;
				write_class_ref ctx c;
				let write_cast_field _ (t,cf) =
					write_typeref ctx t;
					write_class_field_ref c ctx cf
				in
				write_list_8 ctx write_cast_field a.a_from_field;
				write_list_8 ctx write_cast_field a.a_to_field;
				write_list_8 ctx (write_class_field_ref c) a.a_array;
				write_option ctx (write_class_field_ref c) a.a_resolve;
			| None ->
				IO.write_byte ctx.ch 0;
		end
end

module HxbAnon = struct
	open HxbTypeRef
	open HxbField

	let write_anon_status ctx = function
		| Closed -> IO.write_byte ctx.ch 0
		| Opened -> IO.write_byte ctx.ch 1
		| Const -> IO.write_byte ctx.ch 2
		| Extend tl ->
			IO.write_byte ctx.ch 3;
			write_typeref_params ctx tl
		| Statics c ->
			IO.write_byte ctx.ch 4;
			write_class_ref ctx c;
		| EnumStatics en ->
			IO.write_byte ctx.ch 5;
			write_enum_ref ctx en;
		| AbstractStatics a ->
			IO.write_byte ctx.ch 6;
			write_abstract_ref ctx a

	let write_anon ctx an =
		write_anon_status ctx !(an.a_status)

	let write_anon_structure ctx an =
		let i,l = PMap.fold (fun cf (i,acc) -> (i + 1,cf :: acc)) an.a_fields (0,[]) in
		IO.write_byte ctx.ch i;
		List.iter (fun cf ->
			write_anon_field_ref ctx cf;
			write_class_field_data ctx cf;
		) l
end

module HxbModule = struct
	open HxbWrite
	open HxbTypeRef

	let write_module_check_policy ctx mcp =
		let i = match mcp with
			| NoCheckFileTimeModification -> 0
			| CheckFileContentModification -> 1
			| NoCheckDependencies -> 2
			| NoCheckShadowing -> 3
		in
		IO.write_byte ctx.ch i

	let write_module_kind ctx mk =
		let i = match mk with
			| MCode -> 0
			| MMacro -> 1
			| MFake -> 2
			| MExtern -> 3
			| MImport -> 4
		in
		IO.write_byte ctx.ch i

	let write_module_type_ref ctx = function
		| TClassDecl c ->
			IO.write_byte ctx.ch 0;
			write_class_ref ctx c;
		| TEnumDecl en ->
			IO.write_byte ctx.ch 1;
			write_enum_ref ctx en;
		| TTypeDecl td ->
			IO.write_byte ctx.ch 2;
			write_typedef_ref ctx td;
		| TAbstractDecl a ->
			IO.write_byte ctx.ch 3;
			write_abstract_ref ctx a

	let write_module1 ctx m =
		ignore(Pool.add_module ctx.pool m);
		IO.write_i32 ctx.ch m.m_id;
		write_dot_path ctx m.m_path;
		let ex = m.m_extra in
		write_string ctx ex.m_file;
		write_string ctx ex.m_sign;
		write_list_8 ctx write_module_check_policy ex.m_check_policy;
		IO.write_double ctx.ch ex.m_time;
		write_module_kind ctx ex.m_kind;
		(* dirty *)
		IO.write_i32 ctx.ch ex.m_added;
		IO.write_i32 ctx.ch ex.m_mark;
		(* deps *)
		IO.write_i32 ctx.ch ex.m_processed

	let write_module2 ctx m =
		write_list_8 ctx write_module_type_ref m.m_types;
		let ex = m.m_extra in
		(* binded_res *)
		(* macro calls *)
		write_list_8 ctx (fun _ (s,(c,cf,b)) ->
			write_string ctx s;
			write_class_ref ctx c;
			write_class_field_ref c ctx cf;
			write_bool ctx b
		) ex.m_if_feature;
		write_hashtbl ctx (write_string ctx) (write_bool ctx) ex.m_features
end

module HxbFile = struct
	open HxbWrite
	open Lexer

	let write ctx =
		let l = Hashtbl.fold (fun k _ acc ->
			let file = Lexer.find_file k in
			file :: acc
		) ctx.files [] in
		let write_int_pair _ (a,b) =
			IO.write_i32 ctx.ch a;
			IO.write_i32 ctx.ch b;
		in
		HxbWrite.write_list ctx (fun _ file ->
			write_string ctx file.lfile;
			IO.write_i32 ctx.ch file.lline;
			write_list ctx write_int_pair file.llines;
			write_list ctx (fun _ -> IO.write_i32 ctx.ch) file.lstrings;
		) l
end

module HxbHeader = struct
	let write ctx ctx =
		IO.write_byte ctx.ch hxb_version;
		IO.write_byte ctx.ch Globals.version_major;
		IO.write_byte ctx.ch Globals.version_minor;
		IO.write_byte ctx.ch Globals.version_revision;
		IO.write_byte ctx.ch (match ctx.texpr_positions with
			| NoPositions -> 0
			| RelativePositions -> 1
			| AbsolutePositions -> 2)
end

let explore_anon_types ctx =
	Lut.iter (fun an ->
		let tdefs = Hashtbl.create 0 in
		let rec loop t = match t with
			| TInst(_,tl) | TEnum(_,tl) | TAbstract(_,tl) ->
				List.iter loop tl
			| TType(td,tl) ->
				if not (Hashtbl.mem tdefs td.t_path) then begin
					Hashtbl.add tdefs td.t_path true;
				end;
				List.iter loop tl
			| TAnon an ->
				ignore(Pool.add_anon ctx.pool an);
				anon_fields an;
			| TFun(tl,tr) ->
				List.iter (fun (_,_,t) -> loop t) tl;
				loop tr
			| TDynamic t1 ->
				if t1 != t_dynamic then loop t1
			| TMono r ->
				begin match !r with
					| None -> ()
					| Some t -> loop t
				end
			| TLazy r ->
				begin match !r with
					| LAvailable t ->
						loop t
					| _ ->
						failwith "unresolved TLazy"
				end
		and anon_fields an =
			PMap.iter (fun _ cf ->
				ignore(Pool.add_anon_field ctx.pool cf);
				loop cf.cf_type;
			) an.a_fields
		in
		anon_fields an
	) ctx.pool.Pool.anons

type write_kind =
	| Full of common_state
	| SingleModule of module_def

let write def ch_file wk =
	HxbTexpr.write_class := (fun ctx c ->
		HxbModuleType.write_class1 ctx c;
		HxbModuleType.write_class2 ctx c;
	);
	let ctx = {
		ch = IO.output_bytes();
		pool = Pool.create();
		curpmin = 0;
		curpmax = 0;
		files = Hashtbl.create 0;
		texpr_positions = (match Define.raw_defined_value_safe def "hxb-texpr-pos" with
			| "relative" -> RelativePositions
			| "none" -> NoPositions
			| _ -> AbsolutePositions);
	} in
	IO.nwrite_string ch_file "HXB";

	let byte_lengths = Hashtbl.create 0 in
	let close_table name bytes ch =
		IO.nwrite_string ch name;
		let length = Bytes.length bytes in
		Hashtbl.add byte_lengths name length;
		IO.write_i32 ch length;
		IO.nwrite ch bytes
	in
	let write_table name f =
		let close = create_table ctx in
		f ctx;
		close_table name (close())
	in
	let write_lut' name fl fi lut f =
		let close = create_table ctx in
		IO.write_i32 ctx.ch (fl lut);
		fi f lut;
		close_table name (close())
	in
	let write_lut name lut f =
		write_lut' name Lut.length Lut.iter lut f
	in
	let write_eq_lut name lut f =
		write_lut' name EqLut.length EqLut.iter lut f
	in

	let head = write_table "HEAD" (HxbHeader.write ctx) in

	(* The order of everything from here on out is very sensitive. In particular, the order
	   in which we handle tables here is completely different from the actual output order
	   further below. This is because we first want to reach a stage where we know all the
	   parts involved in this compilation, before writing out the information.
	*)

	(* 1. Handle module declarations and definitions. *)

	let write_mod1 modules ctx = HxbWrite.write_list ctx HxbModule.write_module1 modules in
	let write_mod2 modules ctx = HxbWrite.write_list ctx HxbModule.write_module2 modules in

	let modules,types,state = match wk with
		| Full state ->
			state.modules,state.types,Some state
		| SingleModule md ->
			[md],md.m_types,None
	in

	let mod1 = write_table "MOD1" (write_mod1 modules) in
	let mod2 = write_table "MOD2" (write_mod2 modules) in

	let tord = write_table "TORD" (fun _ ->
		HxbWrite.write_list ctx HxbModule.write_module_type_ref types
	) in

	(* 2. Handle module type definitions. Don't write out declarations yet because there
	      might be more being discovered, i.e. references that are not part of this
	      "compilation unit". *)
	let enm2 = write_lut "ENM2" ctx.pool.Pool.enums (HxbModuleType.write_enum2 ctx) in
	let tdf2 = write_lut "TDF2" ctx.pool.Pool.typedefs (HxbModuleType.write_typedef2 ctx) in
	let abs2 = write_lut "ABS2" ctx.pool.Pool.abstracts (HxbModuleType.write_abstract2 ctx) in
	let cls2 = write_lut "CLS2" ctx.pool.Pool.classes (HxbModuleType.write_class2 ctx) in

	(* 3. Handle expressions of all class fields we know. This might fill various other
	      pools. *)
	let expr = write_lut "EXPR" ctx.pool.Pool.class_fields (fun cf ->
		HxbWrite.write_option ctx HxbTexpr.write_texpr cf.cf_expr
	) in

	(* 4. Handle __init__ and main() expressions. Same as above. *)
	let init = write_table "INIT" (fun ctx ->
		let inits = ref [] in
		Lut.iter (fun c -> match c.cl_init with
			| None -> ()
			| Some e -> inits := (c,e) :: !inits
		) ctx.pool.Pool.classes;
		HxbWrite.write_list ctx (fun _ (c,e) ->
			HxbTypeRef.write_class_ref ctx c;
			HxbTexpr.write_texpr ctx e;
		) !inits;
	) in
	let main = match state with
		| Some { main = Some e; main_class = Some tp } ->
			Some (write_table "MAIN" (fun ctx ->
				HxbWrite.write_dot_path ctx tp;
				HxbTexpr.write_texpr ctx e
			))
		| _ ->
			None
	in

	(* 5. At this point we have to make sure that we know ALL anon types. This makes it
	      necessary to follow their field types recursively. *)
	explore_anon_types ctx;

	(* 6. Handle fields. All necessary information should be available now. *)
	Lut.close ctx.pool.Pool.class_fields;
	let clsf = write_lut "CLSF" ctx.pool.Pool.class_fields (fun cf ->
		HxbWrite.write_string ctx cf.cf_name;
		HxbWrite.write_position ctx cf.cf_pos;
		HxbWrite.write_position ctx cf.cf_name_pos;
	) in

	Lut.close ctx.pool.Pool.enum_fields;
	let enmf = write_lut "ENMF" ctx.pool.Pool.enum_fields (fun ef ->
		HxbWrite.write_string ctx ef.ef_name;
		HxbWrite.write_position ctx ef.ef_pos;
		HxbWrite.write_position ctx ef.ef_name_pos;
		HxbWrite.write_option ctx HxbWrite.write_string ef.ef_doc;
		IO.write_i32 ctx.ch ef.ef_index;
	) in

	EqLut.close ctx.pool.Pool.anon_fields;
	let anof = write_eq_lut "ANOF" ctx.pool.Pool.anon_fields (fun cf ->
		HxbWrite.write_string ctx cf.cf_name
	) in

	(* 7. Handle anon definitions. At this point, we have seen all anons that could
	      possibly show up. *)
	Lut.close ctx.pool.Pool.anons;
	let ano2 = write_lut "ANO2" ctx.pool.Pool.anons (fun an ->
		HxbAnon.write_anon_structure ctx an
	) in

	let ano1 = write_lut "ANO1" ctx.pool.Pool.anons (fun an ->
		HxbAnon.write_anon ctx an
	) in

	(* 8. Handle module type declarations. We can now assume that we know all
	      module types that are involved in this compilation. *)
	Lut.close ctx.pool.Pool.classes;
	let cls1 = write_lut "CLS1" ctx.pool.Pool.classes (fun c ->
		HxbModuleType.write_class1 ctx c
	) in

	Lut.close ctx.pool.Pool.enums;
	let enm1 = write_lut "ENM1" ctx.pool.Pool.enums (fun en ->
		HxbModuleType.write_enum1 ctx en
	) in

	Lut.close ctx.pool.Pool.typedefs;
	let tdf1 = write_lut "TDF1" ctx.pool.Pool.typedefs (fun td ->
		HxbModuleType.write_typedef1 ctx td
	) in

	Lut.close ctx.pool.Pool.abstracts;
	let abs1 = write_lut "ABS1" ctx.pool.Pool.abstracts (fun a ->
		HxbModuleType.write_abstract1 ctx a
	) in

	(* 9. Handle resources and such *)
	let incf = match state with
		| Some { include_files = files } when files <> [] ->
			let table = write_table "INCF" (fun _ ->
				HxbWrite.write_list_8 ctx (fun _ (s1,s2) ->
					HxbWrite.write_string ctx s1;
					HxbWrite.write_raw_string ctx s2;
				) files
			) in
			Some table
		| _ ->
			None
	in

	let rsrc = match state with
		| Some state when Hashtbl.length state.resources > 0 ->
			let table = write_table "RSRC" (fun _ ->
				HxbWrite.write_hashtbl ctx (HxbWrite.write_string ctx) (HxbWrite.write_raw_string ctx) state.resources
			) in
			Some table
		| _ ->
			None
	in

	(* 10. Handle all referenced file names. *)
	let file = write_table "FILE" HxbFile.write in

	(* 11. Handle strings. *)
	Lut.close ctx.pool.Pool.strings;
	let strs = write_lut "STRS" ctx.pool.Pool.strings (fun s ->
		HxbWrite.write_raw_string ctx s;
	) in

	(* 12. Write out the information in a straightforward order. *)
	let tables = [
		head;
		strs; (* strings first *)
		file; (* files next, reference strings *)
		mod1; (* module declarations reference files and strings *)
		cls1; (* classes reference modules *)
		tdf1; (* typedefs - same *)
		enm1; (* enums reference modules AND TYPEDEFS *)
		abs1; (* abstracts reference modules (the class relation is part of the definition) *)
		ano1; (* anons reference all of the above through their status (except typedefs maybe?) *)
		tord; (* type order refers to module types *)
		clsf; (* class fields reference all of the above through their types (no expressions yet) *)
		enmf; (* same *)
		anof; (* same *)
		mod2; (* module definitions reference module types *)
		cls2; (* class definitions reference all of the above *)
		tdf2; (* typedefs - same *)
		enm2; (* enums - same *)
		abs2; (* abstracts - same *)
		ano2; (* anons - same *)
		expr; (* expressions reference all of the above *)
		init; (* init expressions - same *)
	] in

	let append tables a = match a with
		| Some a -> tables @ [a]
		| None -> tables
	in

	let tables = append tables main in
	let tables = append tables incf in
	let tables = append tables rsrc in

	List.iter (fun f -> f ch_file) tables;

	let stats = write_table "STAT" (fun _ ->
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.strings);
		IO.write_i32 ctx.ch (Hashtbl.length ctx.files);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.modules);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.classes);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.enums);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.typedefs);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.abstracts);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.anons);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.class_fields);
		IO.write_i32 ctx.ch (Lut.length ctx.pool.Pool.enum_fields);
		IO.write_i32 ctx.ch (EqLut.length ctx.pool.Pool.anon_fields);

		let l = Hashtbl.fold (fun name size acc ->
			(name,size) :: acc
		) byte_lengths [] in
		HxbWrite.write_list_8 ctx (fun _ (s,i) ->
			IO.nwrite_string ctx.ch s;
			IO.write_i32 ctx.ch i
		) l;
	) in

	stats ch_file;

	IO.nwrite_string ch_file "END ";
	IO.write_i32 ch_file 0;
	IO.close_out ch_file

let write com file =
	if file_extension file = "hxb" then begin
		let ch_file = IO.output_channel (open_out_bin file) in
		let state = {
			modules = com.Common.modules;
			types   = com.Common.types;
			include_files = com.Common.include_files;
			main = com.Common.main;
			main_class = com.Common.main_class;
			resources = com.Common.resources;
		} in
		write com.defines ch_file (Full state)
	end else begin
		let file = Path.add_trailing_slash file in
		List.iter (fun m ->
			let ch = IO.output_channel (Path.create_file true ".hxb" [] (file :: fst m.m_path @ [snd m.m_path])) in
			write com.defines ch (SingleModule m)
		) (List.filter (fun m -> not (ExtString.String.ends_with m.m_extra.m_file "import.hx")) com.modules)
	end