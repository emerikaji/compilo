open Node

let imprim_arbre n = 
  let rec imprim_arbre_rec n lv =
     (if lv > 0 then (String.make (lv*2-1) '-') ^ " " else "") ^ match !n with
    | Conc{left = l ; right = r} -> "Conc\n" ^ imprim_arbre_rec l (lv+1) ^ imprim_arbre_rec r (lv+1)
    | Union{left = l ; right = r} ->  "Union\n" ^ imprim_arbre_rec l (lv+1) ^ imprim_arbre_rec r (lv+1)
    | Star{element = e} -> "Star\n" ^ imprim_arbre_rec e (lv+1)
    | UN{element = e} -> "UN\n" ^ imprim_arbre_rec e (lv+1)
    | Atom{code = c; action = a; atomtype = Terminal} -> Printf.sprintf "Terminal %i with action %i\n" c a
    | Atom{code = c; action = a; atomtype = _} -> Printf.sprintf "Non-Terminal %i with action %i\n" c a
  in
  imprim_arbre_rec n 0
;;