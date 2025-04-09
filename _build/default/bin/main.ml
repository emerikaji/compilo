(* ---- type declarations ---- *)

type atomtype = Terminal | NonTerminal ;;

type node = Conc of {left: node ref ; right: node ref}
| Union of {left: node ref ; right: node ref}
| Star of {element: node ref}
| UN of {element: node ref}
| Atom of {code: int; action: int; atomtype: atomtype}
;;

(* ---- function declarations ---- *)

let genconc = function
| (p1, p2) -> ref (Conc{left = p1; right = p2})
;;

let genunion = function
| (p1, p2) -> ref (Union{left = p1; right = p2})
;;

let genun = function
| (e) -> ref (UN{element = e})
;;

let genstar = function
| (e) -> ref (Star{element = e})
;;

let genatom = function
| (c, a, t) -> ref (Atom{code = c; action = a; atomtype = t})
;;

let imprim_arbre n = 
  let rec imprim_arbre_rec n lv =
     (if lv > 0 then (String.make (lv*2-1) '-') ^ " " else "") ^ match !n with
    | Conc{left = l ; right = r} -> "Conc\n" ^ imprim_arbre_rec l (lv+1) ^ imprim_arbre_rec r (lv+1)
    | Union{left = l ; right = r} ->  "Union\n" ^ imprim_arbre_rec l (lv+1) ^ imprim_arbre_rec r (lv+1)
    | Star{element = e} -> "Star\n" ^ imprim_arbre_rec e (lv+1)
    | UN{element = e} -> "UN\n" ^ imprim_arbre_rec e (lv+1)
    | Atom{code = c; action = a; atomtype = Terminal} -> Printf.sprintf "Terminal '%c' with action %i\n" (Char.chr c) a
    | Atom{code = c; action = a; atomtype = _} -> Printf.sprintf "Non-Terminal %i with action %i\n" c a
  in
  imprim_arbre_rec n 0
;;


let scang0 = 0 ;;

let rec analyse g n = match !n with
| Conc{left = l ; right = r} when analyse g l -> analyse g r
| Conc{left = l ; right = r} -> false
| Union{left = l ; right = r} when analyse g l -> true
| Union{left = l ; right = r} -> analyse g r
| Star{element = e} -> true (* analyse e*)
| UN{element = e} -> true (* analyse e*)
| Atom{code = c; action = a; atomtype = t} when t = Terminal ->  if c = scang0 then true (* if a != 0 then Action.G0 (a) *) else false
| Atom{code = c; action = a; atomtype = t} -> if analyse g g.(c) then (* if a != 0 then Action.G0 (a) *) true else false
;;

(* ---- grammar 0 trees ---- *)

let s = genconc (
  genstar (
    genconc (
      genconc (
        genconc (
          genatom (1, 0, NonTerminal), (* N *)
          genatom (Char.code '>', 0, Terminal)
        ),
        genatom (2, 0, NonTerminal) (* E *)
      ),
      genatom (Char.code ',', 0, Terminal)
    )
  ),
  genatom (Char.code ';', 0, Terminal)
)
;;

let n = genatom (Char.code '_', 0, Terminal)
;;

let e = genconc (
  genatom (3, 0, NonTerminal), (* T *)
  genstar (
    genconc (
      genatom (Char.code '+', 0, Terminal),
      genatom (3, 0, NonTerminal) (* T *)
    )
  )
)
;;

let t = genconc (
  genatom (4, 0, NonTerminal), (* F *)
  genstar (
    genconc (
      genatom (Char.code '.', 0, Terminal),
      genatom (4, 0, NonTerminal) (* F *)
    )
  )
)
;;

let f = genatom(0, 0, Terminal) ;;

let g0 = [| s; n; e; t; f |] ;;

(* ---- main ---- *)

(*
Scan.G0
if analyse A[S] then OK
  *)

print_string (imprim_arbre g0.(0)) ;;
Printf.printf "%b" (analyse g0 g0.(0)) ;;


(* ---- ----- ---- *)
