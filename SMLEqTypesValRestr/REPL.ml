open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast

let () = parse "if true then 1 else 0" |> print_endline
