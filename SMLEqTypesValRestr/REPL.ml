open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast

let () = parse "val x = 1 + 2 + x" |> print_endline
