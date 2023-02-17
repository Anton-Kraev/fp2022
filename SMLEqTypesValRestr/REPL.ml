open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast

let () = parse "1" |> pp_expr Format.std_formatter
