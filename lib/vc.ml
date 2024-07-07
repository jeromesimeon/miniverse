(* core VC *)

type cexpr =
  | CVal of cvalue
  | CEqSeq of ceq * cexpr
  | CExist of Common.var * cexpr
  | CFail
  | CChoice of cexpr * cexpr
  | CApp of cvalue * cvalue
  | COne of cexpr
  | CAll of cexpr
and ceq =
  | CExpr of cexpr
  | CEq of cvalue * cexpr
and cvalue =
  | CVar of string
  | CHnf of chnf
and chnf =
  | CInt of int
  | COp of Common.op
  | CTuple of cvalue list
  | CLambda of string * cexpr

type cprogram =
  | CPOne of cexpr

