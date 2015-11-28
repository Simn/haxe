(* NEVER insert any new variants in the middle. This messes up error enumeration.
   Always add them at the end! *)
type haxe_error =
	| ClassNameMustStartWithUppercaseCharacter
	| PackageNameMustStartWithALowerCaseCharacter

let to_string = function
	| ClassNameMustStartWithUppercaseCharacter -> "Class name must start with uppercase character"
	| PackageNameMustStartWithALowerCaseCharacter -> "Package name must start with a lower case character"

let error_message e =
	Printf.sprintf "HXE%04i: %s" (Obj.magic e) (to_string e)