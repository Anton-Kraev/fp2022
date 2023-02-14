open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast

let () = parse "let val x = 2 val y = 3 in x + y end" |> print_endline
let f x y = x = y
let id x = x
let r = f id id
