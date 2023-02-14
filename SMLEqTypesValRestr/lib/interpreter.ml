open Ast

module type MONAD_ERROR = sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

type environment = (id, value, Base.String.comparator_witness) Base.Map.t

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
  | VFun of id list * expr * environment * rec_flag (** fun x -> x * x *)

type error =
  [ `UnboundValue of string (** Unbound value *)
  | `Unreachable
    (** Unreachable code. If this error is thrown then something went seriously wrong *)
  | `UnsupportedOperation (** Used unsupported operation *)
  | `Division_by_zero (** n / 0*)
  | `NotAFunction (** Unreachable when type inference is used *)
  | `TypeMismatch (** Unreachable when type inference is used *)
  | `MisusedWildcard (** Wildcard is in the right-hand expression *)
  | `PatternMatchingFailed (** The case is not matched *)
  | `NonExhaustivePatternMatching (** Pattern-matching is not exhaustive *)
  ]

module Environment (M : MONAD_ERROR) = struct
  open M

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (Printf.sprintf "%S not found" key)
  ;;

  let update environment key value = Base.Map.update environment key ~f:(fun _ -> value)
  let empty = Base.Map.empty (module Base.String)
end

module Interpret (M : MONAD_ERROR) : sig
  val run : expr list -> value M.t
end = struct
  open M
  open Environment (M)

  let run e = return @@ VString "TODO"

  let rec eval (expression : expr) (environment : environment) =
    let rec foldr f ini = function
      | [] -> return ini
      | head :: tail ->
        let* head = eval head environment in
        let* tail = foldr f ini tail in
        return @@ f head tail
    in
    match expression with
    | ELiteral literal ->
      (match literal with
       | LInt x -> return @@ VInt x
       | LString x -> return @@ VString x
       | LBool x -> return @@ VBool x
       | LChar x -> return @@ VChar x
       | LUnit -> return VUnit)
    | EBinaryOp (operation, left_operand, right_operand) ->
      let* left_operand = eval left_operand environment in
      let* right_operand = eval right_operand environment in
      (match operation, left_operand, right_operand with
       (* Arithmetic operations *)
       | Add, VInt x, VInt y -> return @@ VInt (x + y)
       | Sub, VInt x, VInt y -> return @@ VInt (x - y)
       | Mult, VInt x, VInt y -> return @@ VInt (x * y)
       | Div, VInt x, VInt y ->
         if y = 0 then fail "Division by zero" else return @@ VInt (x / y)
       | (Add | Sub | Mult | Div), _, _ -> fail "Type mismatch"
       (* And ( && ) *)
       | And, VBool x, VBool y -> return @@ VBool (x && y)
       | Or, VBool x, VBool y -> return @@ VBool (x || y)
       | (And | Or), _, _ -> fail "Type mismatch"
       (* Equality *)
       | op, arg1, arg2 ->
         let comparison_operation : 'a. 'a -> 'a -> bool =
           match op with
           | Eq -> Base.Poly.( = )
           | NotEq -> Base.Poly.( <> )
           | Greater -> Base.Poly.( > )
           | GreaterOrEq -> Base.Poly.( >= )
           | Less -> Base.Poly.( < )
           | _ -> Base.Poly.( <= )
         in
         (match arg1, arg2 with
          | VInt x, VInt y -> return @@ VBool (comparison_operation x y)
          | VString x, VString y -> return @@ VBool (comparison_operation x y)
          | VBool x, VBool y -> return @@ VBool (comparison_operation x y)
          | VChar x, VChar y -> return @@ VBool (comparison_operation x y)
          | VList x, VList y -> return @@ VBool (comparison_operation x y)
          | _, _ -> fail "Unsupported operation"))
    | EIdentifier name ->
      if name = "_"
      then fail "Misused wildcard"
      else
        let* v = find environment name in
        (match v with
         | VFun (id_list, function_body, environment, Recursive) ->
           return @@ VFun (id_list, function_body, update environment name v, Recursive)
         | _ -> return v)
    | EApplication (function_expr, argument_expr) ->
      let* eval_argument = eval argument_expr environment in
      let* eval_function = eval function_expr environment in
      let* id_list, function_body, local_environment, recursive =
        match eval_function with
        | VFun (id_list, function_body, environment, recursive) ->
          return (id_list, function_body, environment, recursive)
        | _ -> fail "Not a function"
      in
      let* id, id_list =
        match id_list with
        | head :: tail -> return (head, tail)
        | _ -> fail "Not a function"
      in
      let environment =
        if id <> "_" then update local_environment id eval_argument else local_environment
      in
      if id_list = []
      then eval function_body environment
      else return @@ VFun (id_list, function_body, environment, recursive)
    | EArrowFun (arguments_list, function_body) ->
      (match arguments_list with
       | [] -> eval function_body environment
       | _ -> return @@ VFun (arguments_list, function_body, environment, NonRecursive))
    | EFunDec (_, arguments_list, function_body) ->
      (match arguments_list with
       | [] -> eval function_body environment
       | _ -> return @@ VFun (arguments_list, function_body, environment, Recursive))
    | EValueDec (_, function_body) -> eval function_body environment
    | EIfThenElse (condition, true_branch, false_branch) ->
      let* eval_conditional = eval condition environment in
      (match eval_conditional with
       | VBool true -> eval true_branch environment
       | VBool false -> eval false_branch environment
       | _ -> fail "Type mismatch")
    | EUnaryOp (operator, operand) ->
      let* operand = eval operand environment in
      (match operator, operand with
       | Neg, VInt x -> return @@ VInt (-x)
       | Not, VBool x -> return @@ VBool (not x)
       | _ -> fail "Type mismatch")
    | EList list ->
      (match list with
       | [] -> return @@ VList []
       | _ ->
         let rec eval_list list =
           match Base.List.hd_exn list, Base.List.tl_exn list with
           | head, [] ->
             let* head = eval head environment in
             return @@ VList [ head ]
           | head, tail ->
             let* head = eval head environment in
             let* tail = eval_list tail in
             (match tail with
              | VList tail -> return @@ VList (head :: tail)
              | _ -> fail "Unreachable")
         in
         eval_list list)
    | EConsList (operand, list) ->
      let* operand = eval operand environment in
      let* list = eval list environment in
      (match operand, list with
       | x, VList list -> return @@ VList (x :: list)
       | _ -> fail "Type mismatch")
    | ETuple list ->
      let* list = foldr (fun x xs -> x :: xs) [] list in
      return @@ VTuple list
    | ELetIn (bindings_list, expression) ->
      let rec eval_bindings environment = function
        | h :: t ->
          let* result = eval h environment in
          (match h with
           | EValueDec (name, _) -> eval_bindings (update environment name result) t
           | _ -> fail "Unreachable")
        | _ -> eval expression environment
      in
      eval_bindings environment bindings_list
    | ECaseOf (matched_expression, case_list) ->
      let* environment = return environment in
      let rec compare_patterns matched_expression case action environment =
        let rec helper environment = function
          | matched_head :: matched_tail, head :: tail ->
            let result, environment, head_success =
              compare_patterns
                matched_head
                head
                (EArrowFun ([ "_" ], ELiteral LUnit))
                environment
            in
            let monadic_execution, new_environment, tail_success =
              helper environment (matched_tail, tail)
            in
            let ( *> ) l r = l >>= fun _ -> r in
            result *> monadic_execution, new_environment, head_success && tail_success
          | [], [] -> eval action environment, environment, true
          | _ -> fail "PatternMatchingFailed", environment, false
        in
        match matched_expression, case with
        | VInt value, ELiteral (LInt x) when value = x ->
          eval action environment, environment, true
        | VChar value, ELiteral (LChar x) when value = x ->
          eval action environment, environment, true
        | VBool value, ELiteral (LBool x) when value = x ->
          eval action environment, environment, true
        | VString value, ELiteral (LString x) when value = x ->
          eval action environment, environment, true
        | VUnit, ELiteral LUnit -> eval action environment, environment, true
        | VInt value, EUnaryOp (Neg, ELiteral (LInt x)) when value = -x ->
          eval action environment, environment, true
        | value, EIdentifier id ->
          let new_environment =
            if id <> "_" then update environment id value else environment
          in
          eval action new_environment, new_environment, true
        | VList matched_list, EList list -> helper environment (matched_list, list)
        | VTuple matched_tuple, ETuple tuple -> helper environment (matched_tuple, tuple)
        | VList matched_list, EConsList (head, tail) ->
          (match matched_list with
           | matched_head :: matched_tail ->
             let result, environment, head_success =
               compare_patterns
                 matched_head
                 head
                 (EArrowFun ([ "_" ], ELiteral LUnit))
                 environment
             in
             let monadic_execution, new_environment, tail_success =
               compare_patterns (VList matched_tail) tail action environment
             in
             let ( *> ) l r = l >>= fun _ -> r in
             result *> monadic_execution, new_environment, head_success && tail_success
           | [] -> fail "PatternMatchingFailed", environment, false)
        | _ -> fail "PatternMatchingFailed", environment, false
      in
      let* eval_matched_expression = eval matched_expression environment in
      let rec helper = function
        | case :: tail ->
          let result, _, success =
            compare_patterns eval_matched_expression (fst case) (snd case) environment
          in
          if success then result else helper tail
        | [] -> fail "Non exhaustive pattern matching"
      in
      helper case_list
  ;;
end
