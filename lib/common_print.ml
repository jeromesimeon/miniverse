open Common

let print_op f op =
  begin match op with
    | Gt -> Fmt.pf f "Gt"
    | Add -> Fmt.pf f "Add"
  end
