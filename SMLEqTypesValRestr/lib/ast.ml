(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type literal =
  | LChar of char
  | LString of string
  | LInt of int
  | LBool of bool
  | LUnit
[@@deriving show { with_path = false }]

type binary_op =
  | Add (* + *)
  | Sub (* - *)
  | Mult (* * *)
  | Div (* / *)
  | Eq (* = *)
  | NotEq (* <> *)
  | Less (* < *)
  | LessOrEq (* <= *)
  | Greater (* > *)
  | GreaterOrEq (* >= *)
  | And (* andalso *)
  | Or (* orelse *)
[@@deriving show { with_path = false }]

type unary_op =
  | Neg (* ~ *)
  | Not (* not *)
[@@deriving show { with_path = false }]

type expr =
  | ELiteral of literal (* 55 *)
  | EIdentifier of id (* varname *)
  | EUnaryOp of unary_op * expr (* ~10 *)
  | EBinaryOp of binary_op * expr * expr (* 7 + 8 *)
  | ETuple of expr list (* ("first", 2, '3') *)
  | EList of expr list (* [99.3, 83.32] *)
  | EConsList of expr * expr (* 4 :: [5, 6] *)
  | ECaseOf of expr * (expr * expr) list
    (* case x 
                                             of 0 => 'zero' 
                                              | _ => 'not zero' *)
  | ELetIn of expr list * expr
    (* let
                                    val a = 3 
                                    val b = 10
                                  in
                                    a + b
                                  end *)
  | EApplication of expr * expr (* f x *)
  | EFunDec of id * id list * expr
    (* fun f x y = x + y
                                      fun factorial n = n * factorial (n - 1) *)
  | EValueDec of id * expr (* val x = 88 *)
  | EArrowFun of id list * expr (* fn x => x + 1 *)
  | EIfThenElse of expr * expr * expr (* if true then 1 else 0 *)
[@@deriving show { with_path = false }]
