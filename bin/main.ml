open Miniverse
open Vc
open Evc

let print_cast cast =
  Fmt.pr 
    "%a@."
    Vc_fmt.print_cprogram (CPOne cast)

let print_ast ast =
  Fmt.pr 
    "%a@."
    Evc_fmt.print_program (POne ast)

let () =
  print_ast Examples.tiny;
  print_cast Examples.cast;
  print_ast Examples.ast1;
  print_cast Examples.cast1;
