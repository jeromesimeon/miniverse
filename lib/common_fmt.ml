open Common

let print_op f op =
  begin match op with
    | Gt -> Fmt.pf f "Gt"
    | Add -> Fmt.pf f "Add"
  end

let print_exists pp xl f e =
  Fmt.pf
    f
    {|@[<hov 1>∃%a@;<0 -1>.%a@]|}
    Fmt.(list ~sep:sp string) xl pp e
let print_tuple pp f el =
    Fmt.pf
    f
    {|@[<hv2>⟨@,%a@;<0 -2>⟩@]|} 
    Fmt.(list ~sep:comma pp) el

let print_one pp f e =
  Fmt.pf f "@[<hov 0 2>one {@ %a@]@ }" pp e
let print_all pp f e =
  Fmt.pf f "@[<hov 0 2>all {@ %a@]@ }" pp e
