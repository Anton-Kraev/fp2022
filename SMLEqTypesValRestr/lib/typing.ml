type type_variable_number = int
type identifier = string

type ground_type =
  | Char
  | String
  | Int
  | Bool
  | Unit
[@@deriving show { with_path = false }]

type typ =
  | TVar of type_variable_number (** 'a *)
  | TEqualityVar of type_variable_number (** ''a *)
  | TArr of typ * typ (** string -> int *)
  | TTuple of typ list (** int * int *)
  | TList of typ (** 'a list *)
  | TGround of ground_type (** int *)

(* Ground types *)
let char_typ = TGround Char
let string_typ = TGround String
let int_typ = TGround Int
let unit_typ = TGround Unit
let bool_typ = TGround Bool

(* Smart constructors for types *)
let var_t n = TVar n
let var_eq_t n = TEqualityVar n
let arrow_t left_type right_type = TArr (left_type, right_type)
let tuple_t type_list = TTuple type_list
let list_t typ = TList typ

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ
