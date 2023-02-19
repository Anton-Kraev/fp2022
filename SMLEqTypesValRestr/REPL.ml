open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast
open SMLEqTypesValRestr_lib.Inferencer
open SMLEqTypesValRestr_lib.Typing
open SMLEqTypesValRestr_lib.Interpreter

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | String -> fprintf fmt "string"
     | Char -> fprintf fmt "char"
     | Bool -> fprintf fmt "bool"
     | Unit -> fprintf fmt "unit")
  | TTuple value_list ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ -> pp_type fmt typ))
      value_list
  | TList typ -> fprintf fmt (arrow_format typ ^^ " list") pp_type typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Char.chr (var + 97))
  | TEqualityVar var -> fprintf fmt "%s" @@ "''" ^ Char.escaped (Char.chr (var + 97))
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

let pp_error fmt (err : SMLEqTypesValRestr_lib.Inferencer.error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "No such variable: %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is ";
    pp_type fmt t1;
    fprintf fmt " but expected type was ";
    pp_type fmt t2
  | `Unreachable -> fprintf fmt "Not reachable."
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;

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

let pp_error fmt =
  let open Format in
  function
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

let () =
  parse "fun f x = case x of y => y"
  |> function
  | Ok ast -> show_expr ast |> print_endline
  | Error msg -> failwith msg
;;

let () =
  parse "fn x y => x = y"
  |> function
  | Ok ast -> show_expr ast |> print_endline
  | Error msg -> failwith msg
;;

let () =
  parse "fun f x = case x of y => y"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok t -> print_typ t
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;

let () =
  parse "fn x y z => x = y orelse case z of (h, t) => h"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok t -> print_typ t
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;

let () =
  parse "val x = fn x y => let val id = x\n                val idid = y in idid = id end"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok t -> print_typ t
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;

module Res : MONAD_ERROR = struct
  type 'a t = ('a, error) result

  let ( >>= ) e1 e2 =
    match e1 with
    | Ok x -> e2 x
    | Error s -> Error s
  ;;

  let return = Result.ok
  let fail = Result.error
  let ( let* ) = ( >>= )
  let ( >>| ) f g = f >>= fun x -> return (g x)
end

open Interpret (Res)
open Environment (Res)

let () =
  parse "fun f x = case x of y => y"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok _ ->
      eval ast empty
      |> (function
      | Ok result -> print_value result
      | Error error -> print_error error)
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;

let () =
  parse "let val t = (fn x y z => x = y orelse z) in (t 2 2 false) end"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok _ ->
      eval ast empty
      |> (function
      | Ok result -> print_value result
      | Error error -> print_error error)
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;

let () =
  parse "val x = fn x y => let val id = x\n                val idid = y in idid = id end"
  |> function
  | Ok ast ->
    Result.map snd (R.run (infer TypeEnv.empty ast))
    |> (function
    | Ok _ ->
      eval ast empty
      |> (function
      | Ok result -> print_value result
      | Error error -> print_error error)
    | Error err -> print_type_error err)
  | Error msg -> failwith msg
;;
