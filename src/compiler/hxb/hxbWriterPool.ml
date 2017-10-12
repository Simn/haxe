open Globals
open Ast
open Path
open Type

module Lut = struct
	type ('a,'b) t = {
		tbl : 'b DynArray.t;
		lut : ('a,int) Hashtbl.t;
		mutable closed : bool;
	}

	let create () = {
		tbl = DynArray.create();
		lut = Hashtbl.create 0;
		closed = false;
	}

	let get lut k = Hashtbl.find lut.lut k

	let add lut k v =
		try
			Hashtbl.find lut.lut k
		with Not_found ->
			assert (not lut.closed);
			let i = DynArray.length lut.tbl in
			Hashtbl.add lut.lut k i;
			DynArray.add lut.tbl v;
			i

	let length lut =
		DynArray.length lut.tbl

	let iter f lut =
		DynArray.iter f lut.tbl

	let close lut =
		lut.closed <- true
end

module EqLut = struct
	type 'a t = {
		tbl : 'a DynArray.t;
		mutable closed : bool;
	}

	let create () = {
		tbl = DynArray.create();
		closed = false;
	}

	let get f fp x =
		(* TODO: this is retarded *)
		let r = ref (-1) in
		try
			DynArray.iteri (fun i x' ->
				if f x x' then begin
					r := i;
					raise Exit
				end
			) fp.tbl;
			raise Not_found
		with Exit ->
			!r

	let add f fp x =
		try
			get f fp x
		with Not_found ->
			assert (not fp.closed);
			let i = DynArray.length fp.tbl in
			DynArray.add fp.tbl x;
			i

	let length lut =
		DynArray.length lut.tbl

	let iter f lut =
		DynArray.iter f lut.tbl

	let close lut =
		lut.closed <- true
end

module Pool = struct
	type anon_key =
		| KClass of path
		| KEnum of path
		| KAbstract of path
		| KAnon of string

	type t = {
		strings      : (string,string) Lut.t;
		modules      : (int,module_def) Lut.t;
		classes      : (path,tclass) Lut.t;
		enums        : (path,tenum) Lut.t;
		typedefs     : (path,tdef) Lut.t;
		abstracts    : (path,tabstract) Lut.t;
		anons        : (anon_key,tanon) Lut.t;
		class_fields : (path * string * string,tclass_field) Lut.t;
		enum_fields  : (path * string,tenum_field) Lut.t;
		anon_fields  : tclass_field EqLut.t;
		mutable vars : (int,tvar) Lut.t;
	}

	let create () = {
		strings      = Lut.create();
		modules      = Lut.create();
		classes      = Lut.create();
		enums        = Lut.create();
		typedefs     = Lut.create();
		abstracts    = Lut.create();
		anons        = Lut.create();
		class_fields = Lut.create();
		enum_fields  = Lut.create();
		anon_fields  = EqLut.create();
		vars         = Lut.create();
	}

	(* String *)

	let add_string pool s =
		Lut.add pool.strings s s

	let add_string_list pool sl =
		let rec loop i acc sl = match sl with
			| [] -> i,acc
			| s :: sl ->
				let k = add_string pool s in
				loop (i + 1) (k :: acc) sl
		in
		loop 0 [] sl

	let add_path pool (sl,s) =
		let i = add_string pool s in
		let il = add_string_list pool sl in
		i,il

	(* Module *)

	let add_module pool m =
		Lut.add pool.modules m.m_id m

	let get_module pool m =
		try Lut.get pool.modules m.m_id with Not_found -> failwith ("Unbound module: " ^ (s_type_path m.m_path))

	(* Module type *)

	let add_class tp c =
		Lut.add tp.classes c.cl_path c

	let add_enum tp en =
		Lut.add tp.enums en.e_path en

	let add_typedef tp td =
		Lut.add tp.typedefs td.t_path td

	let add_abstract tp a =
		Lut.add tp.abstracts a.a_path a

	let add_anon_field pool cf =
		EqLut.add (==) pool.anon_fields cf

	let add_anon tp an =
		let key = match !(an.a_status) with
			| Statics c -> KClass c.cl_path
			| EnumStatics en -> KEnum en.e_path
			| AbstractStatics a -> KAbstract a.a_path
			| _ ->
				let fields = PMap.foldi (fun s cf acc ->
					let k = Printf.sprintf "%s:%s" s (s_type (print_context()) cf.cf_type) in
					k :: acc
				) an.a_fields [] in
				let l = List.sort Pervasives.compare fields in
				KAnon (String.concat "-" l)
		in
		Lut.add tp.anons key an

	(* Field *)

	let add_class_field pool c cf =
		Lut.add pool.class_fields (c.cl_path,cf.cf_name,s_type (print_context()) cf.cf_type) cf

	let add_enum_field pool en ef =
		Lut.add pool.enum_fields (en.e_path,ef.ef_name) ef

	(* Expr *)

	let init_expr_context pool =
		pool.vars <- Lut.create()

	let add_var pool v =
		Lut.add pool.vars v.v_id v
end