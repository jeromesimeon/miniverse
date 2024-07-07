open Evc

let print_exists pp xl f e =
  Fmt.pf
    f
    {|@[<h1>∃@;%a@;<0 -1>.%a@]|}
    Fmt.(list ~sep:sp string) xl pp e
let print_tuple pp f el =
    Fmt.pf
    f
    {|@[<hv2>⟨@,%a@;<0 -2>⟩@]|} 
    Fmt.(list ~sep:comma pp) el

let rec print_expr f (e:expr) =
  begin match e with
    | Exists (xl, e) ->
      print_exists print_expr xl f e
    | Fail ->
      Fmt.pf f "fail"
    | Choice (e1, e2) ->
      Fmt.pf f "%a │ %a" print_expr e1 print_expr e2
    | AppExpr (e1, e2) ->
      Fmt.pf f "%a %a" print_expr e1 print_expr e2
    | One e ->
      Fmt.pf f "one{ %a }" print_expr e
    | All e ->
      Fmt.pf f "all{ %a }" print_expr e
    | EqSeq (e1, e2, e3) ->
      Fmt.pf f "%a = %a;@ %a" print_expr e1 print_expr e2 print_expr e3
    | Plus (e1, e2) ->
      Fmt.pf f "%a + %a" print_expr e1 print_expr e2
    | GreaterThan (e1, e2) ->
      Fmt.pf f "%a > %a" print_expr e1 print_expr e2
    | Bind (x, e1, e2) ->
      Fmt.pf
        f
        {|%s := %a@; %a|} x print_expr e1 print_expr e2
    | TupleExpr el ->
      print_tuple print_expr f el
    | EqExpr (e1, e2) ->
      Fmt.pf f "%a;@ %a" print_expr e1 print_expr e2
    | LambdaTuple (xl,e) ->
      Fmt.pf f 
        {|λ%a@[<hv2>⟨@,%a@;<0 -2>⟩@]|} (print_tuple Fmt.string) xl print_expr e
    | IfExpr (xl, e1, e2, e3) ->
      Fmt.pf f
        {|if (%a) then %a else %a|} (print_exists print_expr xl) e1 print_expr e2 print_expr e3

    | Var x -> Fmt.pf f "%s" x
    | Int i -> Fmt.pf f "%n" i
    | Op op -> Common_print.print_op f op
    | Lambda (fname,e) ->
      Fmt.pf f 
        {|λ%s@[<hv2>⟨@,%a@;<0 -2>⟩@]|} fname print_expr e

  end

let print_program f p =
  begin match p with
    | POne e ->
      print_expr f e
  end
