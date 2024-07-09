open Vc

let rec print_cexpr f (e:cexpr) =
  begin match e with
    | CVal v -> print_cvalue f v
    | CEqSeq (eq, e) ->
      Fmt.pf f "@[<hov 0>%a;@ %a@]" print_ceq eq print_cexpr e
    | CExists (x, e) ->
      Fmt.pf f "@[<hov 1>∃%s.@;%a@]" x print_cexpr e
    | CFail ->
      Fmt.pf f "fail"
    | CChoice (e1, e2) ->
      Fmt.pf f "@[<hov>%a │@ %a@]" print_cexpr e1 print_cexpr e2
    | CApp (v1, v2) ->
      Fmt.pf f "@[<hov>%a@ %a@]" print_cvalue v1 print_cvalue v2
    | COne e ->
      Common_fmt.print_one print_cexpr f e
    | CAll e ->
      Common_fmt.print_all print_cexpr f e
  end

and print_cvalue f (v:cvalue) =
  begin match v with
    | CVar x ->
      Fmt.pf f "%s" x
    | CHnf chnf ->
      print_chnf f chnf
  end

and print_ceq f eq =
  begin match eq with
    | CExpr e ->
      print_cexpr f e
    | CEq (v, e) ->
      Fmt.pf f "@[<hov 0>%a =@ %a@]" print_cvalue v print_cexpr e
  end

and print_chnf f chnf  =
  begin match chnf with
    | CInt i ->
      Fmt.pf f "%n" i
    | COp op ->
      Common_fmt.print_op f op
    | CTuple cvl ->
      Common_fmt.print_tuple print_cvalue f cvl
    | CLambda (fname,e) ->
      Fmt.pf
        f
        {|λ%s@[<hv2>⟨@,%a@,@]⟩|} fname print_cexpr e
 end

let print_cprogram f p =
  begin match p with
    | CPOne e ->
      print_cexpr f e
  end
