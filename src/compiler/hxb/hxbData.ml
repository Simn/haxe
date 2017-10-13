open Type

type texpr_position_mode =
	| NoPositions
	| RelativePositions
	| AbsolutePositions

type common_state = {
	mutable modules       : module_def list;
	mutable types         : module_type list;
	mutable main          : texpr option;
	mutable main_class    : Type.path option;
	mutable include_files : (string * string) list;
	mutable resources     : (string,string) Hashtbl.t;
}

type write_kind =
	| Full of common_state
	| SingleModule of module_def

let hxb_version = 1