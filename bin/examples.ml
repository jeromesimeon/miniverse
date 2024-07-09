open Miniverse
open Vc
open Evc

let cast = CExists ("xxxxxx", CVal (CHnf
    (CTuple [CVar "xxx";CVar "yyy";CVar "xxx";CVar "yyy";CVar "xxx";CVar "xxx";CVar "zzz";CVar "xxx";CVar "yyy";CVar "xxx";CVar "yyy";CVar "xxx";CVar "xxx";CVar "zzz";CVar "xxx";CVar "yyy";CVar "xxx";CVar "yyy";CVar "xxx";CVar "xxx";CVar "zzz";CVar "xxx";CVar "yyy";CVar "xxx";CVar "yyy";CVar "xxx";CVar "xxx";CVar "zzz";])))

let ast1 = TupleExpr [Exists (["x1";"x2"], Var "y");Lambda ("x", Var "y");Var "x";Var "x";Var "y";]
let cast1 = Desugar.desugar ast1

let tiny = Exists (["x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z"], Exists (["x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z";"x";"y";"z"], Int 1))
