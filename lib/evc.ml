(* extended VC *)

type expr =
  (* From VC *)
  | Exists of string list * expr (* Extended *)
  | Fail
  | Choice of expr * expr
  | AppExpr of expr * expr (* Extended *)
  | One of expr
  | All of expr

  (* Extensions *)
  | EqSeq of expr * expr * expr (* Extended, further than the paper suggests: e1 = e2; e3 *)
  | Plus of expr * expr (* New *)
  | GreaterThan of expr * expr (* New *)
  | Bind of string * expr * expr (* New *)
  | TupleExpr of expr list (* Extended *)
  | EqExpr of expr * expr (* New *)
  | LambdaTuple of string list * expr (* Extended(?) i.e., is a singleton tuple also a single value or not *)
  | IfExpr of string list * expr * expr * expr (* New *)

  (* Folded *)
  | Var of string
  | Int of int
  | Op of Common.op
  | Lambda of string * expr

type program =
  | POne of expr
