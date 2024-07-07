open Miniverse
open Vc
open Evc

let cast = CVal (CHnf
    (CTuple [CVar "x";CVar "y";CVar "x";CVar "y";CVar "x";CVar "x";CVar "y";]))

let ast = TupleExpr [Exists (["x1";"x2"], Var "y");Lambda ("x", Var "y");Var "x";Var "x";Var "y";]

let _ = Exists (["x";"y";"z"], Int 1)

let print_cast cast =
  Fmt.pr 
    "%a@."
    Vc_fmt.print_cprogram (CPOne cast)

let print_ast ast =
  Fmt.pr 
    "%a@."
    Evc_fmt.print_program (POne ast)

let () =
  print_cast cast;
  print_ast ast
