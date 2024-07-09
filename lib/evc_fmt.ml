open Evc

let rec print_expr f (e:expr) =
  begin match e with
    | Exists (xl, e) ->
      Common_fmt.print_exists print_expr xl f e
    | Fail ->
      Fmt.pf f "fail"
    | Choice (e1, e2) ->
      Fmt.pf f "@[<hov>%a │@ %a@]" print_expr e1 print_expr e2
    | AppExpr (e1, e2) ->
      Fmt.pf f "%a %a" print_expr e1 print_expr e2
    | One e ->
      Common_fmt.print_one print_expr f e
    | All e ->
      Common_fmt.print_all print_expr f e
    | EqSeq (e1, e2, e3) ->
      Fmt.pf f "%a = %a;@ %a" print_expr e1 print_expr e2 print_expr e3
    | Plus (e1, e2) ->
      Fmt.pf f "@[<hov>%a +@ %a@]" print_expr e1 print_expr e2
    | GreaterThan (e1, e2) ->
      Fmt.pf f "@[<hov>%a >@ %a@]" print_expr e1 print_expr e2
    | Bind (x, e1, e2) ->
      Fmt.pf
        f
        {|@[<hov>%s := %a@;@ %a@]|} x print_expr e1 print_expr e2
    | TupleExpr el ->
      Common_fmt.print_tuple print_expr f el
    | EqExpr (e1, e2) ->
      Fmt.pf f "@[%a;@ %a@]" print_expr e1 print_expr e2
    | LambdaTuple (xl,e) ->
      Fmt.pf f 
        {|λ%a@[<hv2>⟨@,%a@;⟩@]|} (Common_fmt.print_tuple Fmt.string) xl print_expr e
    | IfExpr (xl, e1, e2, e3) ->
      Fmt.pf f
        {|@[<hov 0 0>if (%a)@ then %a@ else %a@]|} (Common_fmt.print_exists print_expr xl) e1 print_expr e2 print_expr e3

    | Var x -> Fmt.pf f "%s" x
    | Int i -> Fmt.pf f "%n" i
    | Op op -> Common_fmt.print_op f op
    | Lambda (fname,e) ->
      Fmt.pf f 
        {|λ%s@[<hv2>⟨@,%a@;<0 -2>⟩@]|} fname print_expr e

  end

let print_program f p =
  begin match p with
    | POne e ->
      print_expr f e
  end

let print_ast ast =
  Fmt.pr 
    "%a@."
    print_program (POne ast)

