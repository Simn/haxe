(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(* Typing of functions and their arguments. *)

open Globals
open Ast
open Type
open Typecore
open Common
open Error

let type_function_arg ctx t e opt p =
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),null_pos) | _ -> e) in
		ctx.t.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),null_pos) -> ctx.t.tnull t | _ -> t in
		t, e

let type_var_field ctx t e stat p =
	if stat then ctx.curfun <- FunStatic else ctx.curfun <- FunMember;
	let e = type_expr ctx e (WithType t) in
	let e = AbstractCast.cast_or_unify ctx t e p in
	match t with
	| TType ({ t_path = ([],"UInt") },[]) | TAbstract ({ a_path = ([],"UInt") },[]) when stat -> { e with etype = t }
	| _ -> e

let type_function_params ctx fd fname p =
	let params = ref [] in
	params := Typeload.type_type_params ctx ([],fname) (fun() -> !params) p fd.f_params;
	!params

let type_function_arg_value ctx t c =
	match c with
		| None -> None
		| Some e ->
			let p = pos e in
			let e = ctx.g.do_optimize ctx (type_expr ctx e (WithType t)) in
			unify ctx e.etype t p;
			let rec loop e = match e.eexpr with
				| TConst c -> Some c
				| TCast(e,None) -> loop e
				| _ ->
					display_error ctx "Parameter default value should be constant" p;
					None
			in
			loop e

let save_function_state ctx =
	let old_ret = ctx.ret in
	let old_fun = ctx.curfun in
	let old_opened = ctx.opened in
	let locals = ctx.locals in
	(fun () ->
		ctx.locals <- locals;
		ctx.ret <- old_ret;
		ctx.curfun <- old_fun;
		ctx.opened <- old_opened;
	)

let type_function ctx args ret fmode f p =
	let fargs = List.map2 (fun (n,c,t) ((_,pn),_,m,_,_) ->
		if n.[0] = '$' then error "Function argument names starting with a dollar are not allowed" p;
		let c = type_function_arg_value ctx t c in
		let v,c = add_local ctx n t pn, c in
		v.v_meta <- m;
		if n = "this" then v.v_meta <- (Meta.This,[],null_pos) :: v.v_meta;
		v,c
	) args f.f_args in
	ctx.curfun <- fmode;
	ctx.ret <- ret;
	ctx.opened <- [];
	let e = match f.f_expr with
		| None -> error "Function body required" p
		| Some e -> e
	in
	let e = type_expr ctx e NoValue in
	let e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), ({eexpr = TBlock el} as e1)) -> e1
		| _ -> e
	in
	let has_return e =
		let rec loop e =
			match e.eexpr with
			| TReturn (Some _) -> raise Exit
			| TFunction _ -> ()
			| _ -> Type.iter loop e
		in
		try loop e; false with Exit -> true
	in
	begin match follow ret with
		| TAbstract({a_path=[],"Void"},_) -> ()
		(* We have to check for the presence of return expressions here because
		   in the case of Dynamic ctx.ret is still a monomorph. If we indeed
		   don't have a return expression we can link the monomorph to Void. We
		   can _not_ use type_iseq to avoid the Void check above because that
		   would turn Dynamic returns to Void returns. *)
		| TMono t when not (has_return e) -> ignore(link t ret ctx.t.tvoid)
		| _ -> (try TypeloadCheck.return_flow ctx e with Exit -> ())
	end;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let has_super_constr() =
		match ctx.curclass.cl_super with
		| None ->
			None
		| Some (csup,tl) ->
			try
				let _,cf = get_constructor (fun f->f.cf_type) csup in
				Some (Meta.has Meta.CompilerGenerated cf.cf_meta,TInst(csup,tl))
			with Not_found ->
				None
	in
	let e = if fmode <> FunConstructor then
		e
	else begin
		delay ctx PForce (fun () -> TypeloadCheck.check_final_vars ctx e);
		match has_super_constr() with
		| Some (was_forced,t_super) ->
			(try
				loop e;
				if was_forced then
					let e_super = mk (TConst TSuper) t_super e.epos in
					let e_super_call = mk (TCall(e_super,[])) ctx.t.tvoid e.epos in
					concat e_super_call e
				else begin
					display_error ctx "Missing super constructor call" p;
					e
				end
			with
				Exit -> e);
		| None ->
			e
	end in
	let e = match ctx.curfun, ctx.vthis with
		| (FunMember|FunConstructor), Some v ->
			let ev = mk (TVar (v,Some (mk (TConst TThis) ctx.tthis p))) ctx.t.tvoid p in
			(match e.eexpr with
			| TBlock l -> { e with eexpr = TBlock (ev::l) }
			| _ -> mk (TBlock [ev;e]) e.etype p)
		| _ -> e
	in
	List.iter (fun r -> r := Closed) ctx.opened;
	e , fargs

let type_function ctx args ret fmode f p =
	let save = save_function_state ctx in
	Std.finally save (type_function ctx args ret fmode f) p

let add_constructor ctx c force_constructor p =
	match c.cl_constructor, c.cl_super with
	| None, Some ({ cl_constructor = Some cfsup } as csup,cparams) when not c.cl_extern ->
		let cf = {
			cfsup with
			cf_pos = p;
			cf_meta = List.filter (fun (m,_,_) -> m = Meta.CompilerGenerated) cfsup.cf_meta;
			cf_doc = None;
			cf_expr = None;
		} in
		let r = exc_protect ctx (fun r ->
			let t = mk_mono() in
			r := lazy_processing (fun() -> t);
			let ctx = { ctx with
				curfield = cf;
				pass = PTypeField;
			} in
			ignore (follow cfsup.cf_type); (* make sure it's typed *)
			(if ctx.com.config.pf_overload then List.iter (fun cf -> ignore (follow cf.cf_type)) cf.cf_overloads);
			let map_arg (v,def) =
				(*
					let's optimize a bit the output by not always copying the default value
					into the inherited constructor when it's not necessary for the platform
				*)
				match ctx.com.platform, def with
				| _, Some _ when not ctx.com.config.pf_static -> v, (Some TNull)
				| Flash, Some (TString _) -> v, (Some TNull)
				| Cpp, Some (TString _) -> v, def
				| Cpp, Some _ -> { v with v_type = ctx.t.tnull v.v_type }, (Some TNull)
				| _ -> v, def
			in
			let args = (match cfsup.cf_expr with
				| Some { eexpr = TFunction f } ->
					List.map map_arg f.tf_args
				| _ ->
					let values = get_value_meta cfsup.cf_meta in
					match follow cfsup.cf_type with
					| TFun (args,_) ->
						List.map (fun (n,o,t) ->
							let def = try type_function_arg_value ctx t (Some (PMap.find n values)) with Not_found -> if o then Some TNull else None in
							map_arg (alloc_var n (if o then ctx.t.tnull t else t) p,def) (* TODO: var pos *)
						) args
					| _ -> assert false
			) in
			let p = c.cl_pos in
			let vars = List.map (fun (v,def) -> alloc_var v.v_name (apply_params csup.cl_params cparams v.v_type) v.v_pos, def) args in
			let super_call = mk (TCall (mk (TConst TSuper) (TInst (csup,cparams)) p,List.map (fun (v,_) -> mk (TLocal v) v.v_type p) vars)) ctx.t.tvoid p in
			let constr = mk (TFunction {
				tf_args = vars;
				tf_type = ctx.t.tvoid;
				tf_expr = super_call;
			}) (TFun (List.map (fun (v,c) -> v.v_name, c <> None, v.v_type) vars,ctx.t.tvoid)) p in
			cf.cf_expr <- Some constr;
			cf.cf_type <- t;
			unify ctx t constr.etype p;
			t
		) "add_constructor" in
		cf.cf_type <- TLazy r;
		c.cl_constructor <- Some cf;
	| None,_ when force_constructor ->
		let constr = mk (TFunction {
			tf_args = [];
			tf_type = ctx.t.tvoid;
			tf_expr = mk (TBlock []) ctx.t.tvoid p;
		}) (tfun [] ctx.t.tvoid) p in
		let cf = mk_field "new" constr.etype p null_pos in
		cf.cf_expr <- Some constr;
		cf.cf_type <- constr.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf;
	| _ ->
		(* nothing to do *)
		()
;;
Typeload.type_function_params_rec := type_function_params