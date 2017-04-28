(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

open Globals
open EvalContext
open EvalValue
open EvalPrinting
open EvalHash

exception Break
exception Continue
exception Return of value

let unexpected_value : 'a . value -> string -> 'a = fun v s ->
	let str = Printf.sprintf "Unexpected value %s, expected %s" (value_string v) s in
	exc_string str

let invalid_call_arg_number i i2 =
	exc_string (Printf.sprintf "Invalid number of call arguments: Expected %i, got %i" i i2)

let format_pos p =
	let error_printer file line = Printf.sprintf "%s:%d:" file line in
	Lexer.get_error_pos error_printer p

let error_exc v stack p =
	let pl = List.map (fun env -> env.leave_pos) stack in
	let pl = List.filter (fun p -> p <> null_pos) pl in
	match pl with
	| [] ->
		Error.error (Printf.sprintf "%s: Uncaught exception %s" (format_pos p) (value_string v)) null_pos
	| _ ->
		let sstack = String.concat "\n" (List.map (fun p -> Printf.sprintf "\t%s" (format_pos p)) pl) in
		Error.error (Printf.sprintf "%s: Uncaught exception %s\nCalled from:\n%s" (format_pos p) (value_string v) sstack) null_pos

let catch_exceptions ctx f p =
	let prev = !get_ctx_ref in
	select ctx;
	let r = try
		let v = f() in
		get_ctx_ref := prev;
		Some v
	with
	| RunTimeException(v,stack,p) ->
		get_ctx_ref := prev;
		error_exc v (match stack with [] -> [] | _ :: l -> l) p
	| MacroApi.Abort ->
		None
	| exc ->
		get_ctx_ref := prev;
		raise exc
	in
	r