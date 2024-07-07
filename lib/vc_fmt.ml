open Vc

let rec print_cexpr f (e:cexpr) =
  begin match e with
    | CVal v -> print_cvalue f v
    | CEqSeq (eq, e) ->
      Fmt.pf f "%a;%a" print_ceq eq print_cexpr e
    | CExist (x, e) ->
      Fmt.pf f "∃%s.%a" x print_cexpr e
    | CFail ->
      Fmt.pf f "fail"
    | CChoice (e1, e2) ->
      Fmt.pf f "%a │ %a" print_cexpr e1 print_cexpr e2
    | CApp (v1, v2) ->
      Fmt.pf f "%a %a" print_cvalue v1 print_cvalue v2
    | COne e ->
      Fmt.pf f "one{ %a }" print_cexpr e
    | CAll e ->
      Fmt.pf f "all{ %a }" print_cexpr e
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
      Fmt.pf f "%a = %a" print_cvalue v print_cexpr e
  end

and print_chnf f chnf  =
  begin match chnf with
    | CInt i ->
      Fmt.pf f "%n" i
    | COp op ->
      Common_print.print_op f op
    | CTuple cvl ->
      Fmt.pf
        f
        {|@[<hv2>(@,%a@;<0 -2>)@]|} 
        Fmt.(list ~sep:comma print_cvalue)  cvl
    | CLambda (fname,e) ->
      Fmt.pf
        f
        {|λ%s@[<hv2>⟨@,%a@;<0 -2>⟩@]|} fname print_cexpr e
 end

let print_cprogram f p =
  begin match p with
    | CPOne e ->
      print_cexpr f e
  end
