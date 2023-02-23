type environment = (Ast.id, value, Base.String.comparator_witness) Base.Map.t

and rec_flag =
  | Recursive
  | NonRecursive

and value =
  | VInt of int (** 5 *)
  | VString of string (** "apple" *)
  | VBool of bool (** true *)
  | VChar of char (** 'a' *)
  | VUnit (** () *)
  | VList of value list (** [1; 2; 3] *)
  | VTuple of value list (** ("abc", 123, false) *)
  | VFun of Ast.id list * Ast.expr * environment * rec_flag (** fun x -> x * x *)

type error =
  [ `UnboundValue of string (** Unbound value *)
  | `Unreachable
    (** Unreachable code. If this error is thrown then something went seriously wrong *)
  | `UnsupportedOperation (** Used unsupported operation *)
  | `DivisionByZero (** n / 0*)
  | `NotAFunction (** Unreachable when type inference is used *)
  | `TypeMismatch (** Unreachable when type inference is used *)
  | `MisusedWildcard (** Wildcard is in the right-hand expression *)
  | `PatternMatchingFailed (** The case is not matched *)
  | `NonExhaustivePatternMatching (** Pattern-matching is not exhaustive *)
  ]

let rec pp_value fmt =
  let open Format in
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_value fmt value)
      fmt
  in
  function
  | VInt value -> fprintf fmt "%d" value
  | VChar value -> fprintf fmt "%C" value
  | VBool value -> fprintf fmt "%B" value
  | VString value -> fprintf fmt "%S" value
  | VUnit -> fprintf fmt "()"
  | VList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt ", ") list
  | VTuple tuple -> fprintf fmt "(%a)" (fun fmt -> pp_list fmt ", ") tuple
  | VFun _ -> fprintf fmt "<fun>"
;;

let print_value = Format.printf "%a\n" pp_value

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `UnboundValue name -> fprintf fmt "Runtime error: unbound value %s." name
  | `Unreachable ->
    fprintf
      fmt
      "This code is supposed to be unreachable. If you got this error, something must \
       have gone seriously wrong."
  | `UnsupportedOperation -> fprintf fmt "Runtime error: unsupported operation."
  | `DivisionByZero -> fprintf fmt "Runtime error: division by zero."
  | `NotAFunction ->
    fprintf fmt "Runtime error: this is not a function, it cannot be applied."
  | `TypeMismatch -> fprintf fmt "Runtime error: mismatching types."
  | `MisusedWildcard ->
    fprintf
      fmt
      "Runtime error: wildcard must not appear on the right-hand side of an expression."
  | `PatternMatchingFailed -> fprintf fmt "Runtime error: pattern-matching failed."
  | `NonExhaustivePatternMatching ->
    fprintf fmt "Runtime error: this pattern-matching is not exhaustive."
;;

let print_error = Format.printf "%a" pp_error
