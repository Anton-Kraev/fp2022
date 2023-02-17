open SMLEqTypesValRestr_lib.Parser
open SMLEqTypesValRestr_lib.Ast

let () =
  parse "(1"
  |> function
  | Ok ast -> pp_expr Format.std_formatter ast
  | Error msg -> failwith msg
;;
