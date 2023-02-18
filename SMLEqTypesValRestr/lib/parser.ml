(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* dispatch for parsers *)
type dispatch =
  { unary_op_p : dispatch -> Ast.expr Angstrom.t
  ; binary_op_p : dispatch -> Ast.expr Angstrom.t
  ; tuple_p : dispatch -> Ast.expr Angstrom.t
  ; list_p : dispatch -> Ast.expr Angstrom.t
  ; cons_list_p : dispatch -> Ast.expr Angstrom.t
  ; case_of_p : dispatch -> Ast.expr Angstrom.t
  ; let_in_p : dispatch -> Ast.expr Angstrom.t
  ; application_p : dispatch -> Ast.expr Angstrom.t
  ; fun_dec_p : dispatch -> Ast.expr Angstrom.t
  ; val_dec_p : dispatch -> Ast.expr Angstrom.t
  ; val_rec_dec_p : dispatch -> Ast.expr Angstrom.t
  ; arrow_fun_p : dispatch -> Ast.expr Angstrom.t
  ; if_then_else_p : dispatch -> Ast.expr Angstrom.t
  ; expr_p : dispatch -> Ast.expr Angstrom.t
  }

(* helper functions *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let is_first_char = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_varname_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

(* parsers *)
let spaces = take_while is_space
let parens p = spaces *> char '(' *> spaces *> p <* spaces <* char ')'

let varname =
  peek_char_fail
  >>= fun first ->
  if is_first_char first
  then take_while is_varname_char
  else fail "Parsing error: bad first symbol of id."
;;

let id_of_expr = function
  | EIdentifier x -> return x
  | _ -> failwith "Unreachable"
;;

let literal_p =
  fix
  @@ fun self ->
  spaces
  *>
  let int_literal_p = integer >>| fun x -> LInt x in
  let char_literal_p = char '\'' *> any_char <* char '\'' >>| fun x -> LChar x in
  let string_literal_p =
    char '"' *> take_while (fun x -> x != '"') <* char '"' >>| fun x -> LString x
  in
  let bool_literal_p =
    string "true" <|> string "false" >>| bool_of_string >>| fun x -> LBool x
  in
  let unit_literal_p = string "()" >>| fun _ -> LUnit in
  let parse_literal =
    choice
      [ int_literal_p; char_literal_p; string_literal_p; bool_literal_p; unit_literal_p ]
  in
  parens self <|> lift e_literal parse_literal
;;

let identifier_p =
  fix
  @@ fun _ ->
  spaces
  *>
  let keywords =
    [ "let"
    ; "val"
    ; "case"
    ; "of"
    ; "if"
    ; "then"
    ; "else"
    ; "in"
    ; "fun"
    ; "fn"
    ; "end"
    ; "true"
    ; "false"
    ; "not"
    ; "orelse"
    ; "andalso"
    ]
  in
  let parse_identifier =
    varname
    >>= fun name ->
    if List.exists (fun x -> x = name) keywords
    then fail "Parsing error: keyword used."
    else return @@ e_identifier name
  in
  parse_identifier
;;

let unary_op_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content_neg =
    choice
      [ parens self
      ; literal_p
      ; identifier_p
      ; d.binary_op_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.if_then_else_p d
      ]
  in
  let parse_content_not =
    choice
      [ parens self
      ; parens @@ d.binary_op_p d
      ; parens @@ d.case_of_p d
      ; parens @@ d.let_in_p d
      ; parens @@ d.application_p d
      ; parens @@ d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> lift2 e_unary_op (char '~' >>| uneg) parse_content_neg
  <|> lift2 e_unary_op (string "not" >>| unot) parse_content_not
;;

let binary_op_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let multiplicative = spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ]
  and additive = spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ]
  and relational =
    spaces
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blse
         ; char '>' >>| bgt
         ; char '<' >>| bls
         ]
  and equality = spaces *> choice [ string "=" >>| beq; string "<>" >>| bneq ]
  and logical_and = spaces *> (string "andalso" >>| band)
  and logical_or = spaces *> (string "orelse" >>| bor) in
  let parse_content =
    choice
      [ parens self
      ; d.unary_op_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let rec parse_bin_op expr_parser op_parsers =
    let chainl1 expr_p op_p =
      let rec go acc =
        lift2 (fun f x -> e_binary_op f acc x) op_p expr_p >>= go <|> return acc
      in
      expr_p >>= fun init -> go init
    in
    match op_parsers with
    | [ op ] -> chainl1 expr_parser op
    | h :: t -> chainl1 (parse_bin_op expr_parser t) h
    | _ -> fail "Unreachable"
  in
  parse_bin_op
    parse_content
    [ logical_or; logical_and; equality; relational; additive; multiplicative ]
;;

let tuple_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let brackets parser = spaces *> char '(' *> parser <* char ')' in
  let separator = spaces *> char ',' *> spaces <|> spaces in
  let parse_content =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self <|> lift e_tuple @@ brackets @@ many (parse_content <* separator)
;;

let list_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let brackets parser = spaces *> char '[' *> parser <* char ']' in
  let separator = spaces *> char ',' *> spaces <|> spaces in
  let parse_content =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self <|> lift e_list @@ brackets @@ many (parse_content <* separator)
;;

let cons_list_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let separator = spaces *> string "::" *> spaces
  and parse_content =
    choice
      [ parens self
      ; d.unary_op_p d
      ; parens @@ d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; parens @@ d.case_of_p d
      ; d.let_in_p d
      ; parens @@ d.application_p d
      ; parens @@ d.arrow_fun_p d
      ; parens @@ d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self <|> lift2 e_cons_list (parse_content <* separator) (self <|> parse_content)
;;

let case_of_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content_left =
    choice
      [ d.unary_op_p d
      ; d.cons_list_p d
      ; d.tuple_p d
      ; d.list_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let parse_content_right =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.cons_list_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> string "case"
      *> lift2
           e_case_of
           parse_content_right
           (let parse_case =
              lift2
                (fun case action -> case, action)
                parse_content_left
                (spaces *> string "=>" *> parse_content_right)
            and separator = spaces *> string "|" in
            spaces
            *> string "of"
            *> spaces
            *> (string "|" <|> spaces)
            *> sep_by1 separator parse_case)
;;

let let_in_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> string "let"
      *> take_while1 is_space
      *> lift2
           e_let_in
           (sep_by1 (take_while1 is_space) (d.val_dec_p d <|> d.val_rec_dec_p d))
           (spaces *> string "in" *> parse_content <* spaces <* string "end")
;;

let application_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let function_parser =
    choice
      [ parens @@ d.case_of_p d
      ; parens @@ d.let_in_p d
      ; parens @@ d.arrow_fun_p d
      ; parens @@ d.if_then_else_p d
      ; identifier_p
      ]
  in
  let operand_parser =
    choice
      [ parens self
      ; parens @@ d.unary_op_p d
      ; parens @@ d.binary_op_p d
      ; parens @@ d.tuple_p d
      ; d.list_p d
      ; parens @@ d.cons_list_p d
      ; parens @@ d.case_of_p d
      ; parens @@ d.let_in_p d
      ; parens @@ d.arrow_fun_p d
      ; parens @@ d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let apply_lift acc = lift (e_application acc) operand_parser in
  let rec go acc = apply_lift acc >>= go <|> return acc in
  parens self <|> function_parser >>= fun init -> apply_lift init >>= fun init -> go init
;;

let fun_dec_p d =
  fix
  @@ fun _ ->
  spaces
  *> string "fun"
  *> spaces
  *>
  let parse_content =
    choice
      [ d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  lift3
    e_fun_dec
    (identifier_p
    >>= id_of_expr
    >>= fun name ->
    if name = "_" then fail "Parsing error: wildcard not expected." else return name)
    (many (identifier_p >>= id_of_expr))
    (spaces *> string "=" *> parse_content)
;;

let val_dec_p d =
  fix
  @@ fun _ ->
  spaces
  *> string "val"
  *> spaces
  *>
  let parse_content =
    choice
      [ d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  lift2
    e_val_dec
    (identifier_p
    >>= id_of_expr
    >>= fun name ->
    if name = "_" then fail "Parsing error: wildcard not expected." else return name)
    (spaces *> string "=" *> parse_content)
;;

let val_rec_dec_p d =
  fix
  @@ fun _ ->
  spaces
  *> string "val rec"
  *> spaces
  *>
  let parse_content =
    choice
      [ d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  lift2
    e_val_rec_dec
    (identifier_p
    >>= id_of_expr
    >>= fun name ->
    if name = "_" then fail "Parsing error: wildcard not expected." else return name)
    (spaces *> string "=" *> parse_content)
;;

let arrow_fun_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> string "fn"
      *> lift2
           e_arrow_fun
           (many1 (identifier_p >>= id_of_expr) <* spaces <* string "=>" <* spaces)
           (parse_content <* spaces)
;;

let if_then_else_p d =
  fix
  @@ fun self ->
  spaces
  *>
  let parse_content =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.tuple_p d
      ; d.list_p d
      ; d.cons_list_p d
      ; d.case_of_p d
      ; d.let_in_p d
      ; d.application_p d
      ; d.arrow_fun_p d
      ; literal_p
      ; identifier_p
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           e_if_then_else
           parse_content
           (spaces *> string "then" *> parse_content)
           (spaces *> string "else" *> parse_content)
;;

(* Parser of general expression *)
let expr_p d =
  choice
    [ d.unary_op_p d
    ; d.binary_op_p d
    ; d.tuple_p d
    ; d.list_p d
    ; d.cons_list_p d
    ; d.case_of_p d
    ; d.let_in_p d
    ; d.application_p d
    ; d.fun_dec_p d
    ; d.val_dec_p d
    ; d.val_rec_dec_p d
    ; d.arrow_fun_p d
    ; d.if_then_else_p d
    ; literal_p
    ; identifier_p
    ]
;;

(* Default expression dispatch *)
let default_d =
  { unary_op_p
  ; binary_op_p
  ; tuple_p
  ; list_p
  ; cons_list_p
  ; case_of_p
  ; let_in_p
  ; application_p
  ; fun_dec_p
  ; val_dec_p
  ; val_rec_dec_p
  ; arrow_fun_p
  ; if_then_else_p
  ; expr_p
  }
;;

(* main parser *)
let parse str = parse_string ~consume:Prefix (expr_p default_d) str
let parse_optimistically str = Result.get_ok @@ parse str

(* some parser tests *)
let%test _ = parse_optimistically "1" = ELiteral (LInt 1)
let%test _ = parse_optimistically "x" = EIdentifier "x"
let%test _ = parse_optimistically "~1" = EUnaryOp (Neg, ELiteral (LInt 1))

let%test _ =
  parse_optimistically "(1 + 2)*3=4"
  = EBinaryOp
      ( Eq
      , EBinaryOp
          (Mult, EBinaryOp (Add, ELiteral (LInt 1), ELiteral (LInt 2)), ELiteral (LInt 3))
      , ELiteral (LInt 4) )
;;

let%test _ =
  parse_optimistically "(1, true, 'd', \"str\")"
  = ETuple
      [ ELiteral (LInt 1)
      ; ELiteral (LBool true)
      ; ELiteral (LChar 'd')
      ; ELiteral (LString "str")
      ]
;;

let%test _ =
  parse_optimistically "[[77, 88], [99, 66]]"
  = EList
      [ EList [ ELiteral (LInt 77); ELiteral (LInt 88) ]
      ; EList [ ELiteral (LInt 99); ELiteral (LInt 66) ]
      ]
;;

let%test _ =
  parse_optimistically "1 :: [2, 3]"
  = EConsList (ELiteral (LInt 1), EList [ ELiteral (LInt 2); ELiteral (LInt 3) ])
;;

let%test _ =
  parse_optimistically "case x of 1 => true | _ => false"
  = ECaseOf
      ( EIdentifier "x"
      , [ ELiteral (LInt 1), ELiteral (LBool true)
        ; EIdentifier "_", ELiteral (LBool false)
        ] )
;;

let%test _ =
  parse_optimistically "let val x = 1 val y = 2 in x + y end"
  = ELetIn
      ( [ EValDec ("x", ELiteral (LInt 1)); EValDec ("y", ELiteral (LInt 2)) ]
      , EBinaryOp (Add, EIdentifier "x", EIdentifier "y") )
;;

let%test _ =
  parse_optimistically "f (x y)"
  = EApplication (EIdentifier "f", EApplication (EIdentifier "x", EIdentifier "y"))
;;

let%test _ =
  parse_optimistically "fun f x y = x orelse y"
  = EFunDec ("f", [ "x"; "y" ], EBinaryOp (Or, EIdentifier "x", EIdentifier "y"))
;;

let%test _ =
  parse_optimistically "val x = not x" = EValDec ("x", EUnaryOp (Not, EIdentifier "x"))
;;

let%test _ =
  parse_optimistically
    "val rec factorial = fn n => if n <= 1 then 1 else n * factorial (n - 1)"
  = EValRecDec
      ( "factorial"
      , EArrowFun
          ( [ "n" ]
          , EIfThenElse
              ( EBinaryOp (LessOrEq, EIdentifier "n", ELiteral (LInt 1))
              , ELiteral (LInt 1)
              , EBinaryOp
                  ( Mult
                  , EIdentifier "n"
                  , EApplication
                      ( EIdentifier "factorial"
                      , EBinaryOp (Sub, EIdentifier "n", ELiteral (LInt 1)) ) ) ) ) )
;;

let%test _ =
  parse_optimistically "fn x y => x <= y"
  = EArrowFun ([ "x"; "y" ], EBinaryOp (LessOrEq, EIdentifier "x", EIdentifier "y"))
;;

let%test _ =
  parse_optimistically "if true then 1 else 0"
  = EIfThenElse (ELiteral (LBool true), ELiteral (LInt 1), ELiteral (LInt 0))
;;
