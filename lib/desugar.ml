open Vc
open Evc

(* desugaring *)

(* Fresh variables... needs lots of work *)
let ctn = ref 0
let fresh (_:cexpr list) = (* oopsies *)
  let f = "fresh" ^ (string_of_int !ctn) in
  incr ctn;
  f
let fresh2 (_:cexpr list) = (* oopsies *)
  let f1 = "fresh" ^ (string_of_int !ctn) in
  let f2 = "fresh" ^ (string_of_int !ctn) in
  incr ctn;
  f1, f2
let rec cfreshbinders cel =
  begin match cel with
    | [] -> [], []
    | ce :: cel' ->
      let (xl, binders) = cfreshbinders cel' in
      let x = fresh (List.map (fun x -> CVal x) xl @ cel') in (* fresh against remaining expressions and newly created variables *)
      CVar x :: xl , (x, ce) :: binders
  end

(* Little Core VC building blocks *)
let cbind x ce1 ce2 =
  CExist (x, (CEqSeq (CEq (CVar x, ce1), ce2)))
let ceqseq x ce1 ce2 ce3 =
  cbind x ce1 (CEqSeq (CEq (CVar x, ce2), ce3))
let cexists xl ce =
  List.fold_left (fun e -> fun x -> CExist (x, e)) ce xl

let chvars xl =
  List.map (fun x -> CVar x) xl
let chtuple_of_vars xl =
  CHnf (CTuple xl)
let chtuple xl =
  CHnf (CTuple (chvars xl))
let ctuple xl =
  CVal (chtuple xl)
let copapp op f1 f2 =
  (CApp (CHnf (COp op), CHnf (CTuple [CVar f1; CVar f2])))
let clambdatuple xl ce =
  let p = fresh [ce] in
  let ctuple = ctuple xl in
  let ceqseq = CEqSeq (CEq (CVar p, ctuple), ce) in
  CVal (CHnf (CLambda (p, ceqseq)))
let capp f x ce1 ce2 =
  cbind f ce1 (cbind x ce2 (CApp (CVar f, CVar x)))

let rec desugar (e:expr) : cexpr =
  begin match e with
    (* From VC *)
    | Exists (xl, e) ->
      cexists xl (desugar e)

    | Fail -> CFail
    | Choice (e1, e2) -> CChoice (desugar e1, desugar e2)
    | One e -> COne (desugar e)
    | All e -> CAll (desugar e)

    (* Extensions *)
    | EqSeq (e1, e2, e3) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      let ce3 = desugar e3 in
      let x = fresh [ce1;ce2;ce3] in
      ceqseq x ce1 ce2 ce3
    | Plus (e1, e2) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      let f1,f2 = fresh2 [ce1;ce2] in
      cbind f1 ce1 (cbind f2 ce2 (copapp Add f1 f2))
    | GreaterThan (e1, e2) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      let f1,f2 = fresh2 [ce1;ce2] in
      cbind f1 ce1 (cbind f2 ce2 (copapp Gt f1 f2))
    | Bind (x, e1, e2) ->
      cbind x (desugar e1) (desugar e2)
    | AppExpr (e1, e2) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      begin match ce1, ce2 with
        | CVal cv1, CVal cv2 ->
          CApp (cv1, cv2)
        | _, _ ->
          let f,x = fresh2 [ce2] in
          capp f x ce1 ce2
      end
    | TupleExpr el ->
      let cel = List.map desugar el in
      let (xl, binders) = cfreshbinders cel in
      let tup = chtuple_of_vars xl in
      List.fold_left (fun ce -> fun (b, ce1) -> cbind b ce1 ce) (CVal tup) binders
    | EqExpr (e1, e2) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      let x = fresh [ce2] in
      ceqseq x ce1 ce2 (CVal (CVar x))
    | LambdaTuple (xl, e) ->
      let ce = desugar e in
      clambdatuple xl ce
    | IfExpr (xl, e1, e2, e3) ->
      let ce1 = desugar e1 in
      let ce2 = desugar e2 in
      let ce3 = desugar e3 in
      let ctruelambda = clambdatuple [] ce2 in
      let ctrue = CEqSeq (CExpr (cexists xl ce1), ctruelambda) in 
      let cfalse = clambdatuple [] ce3 in
      let  ceone = COne (CChoice (ctrue, cfalse)) in
      let f,x = fresh2 [] in
      capp f x ceone (CVal (CHnf (CTuple [])))
(* Folded *)
    | Var x -> CVal (CVar x)
    | Int i -> CVal (CHnf (CInt i))
    | Op o -> CVal (CHnf (COp o))
    | Lambda (x, e) -> CVal (CHnf (CLambda (x, desugar e)))
  end
