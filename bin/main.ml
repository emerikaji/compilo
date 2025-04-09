let a = [||];;
let dico_nt = [];;
let dico_t = [];;

(* ---- type declarations ---- *)

type atomtype = Terminal | NonTerminal ;;

type node =
| Conc of {left: node ref ; right: node ref}
| Union of {left: node ref ; right: node ref}
| Star of {element: node ref}
| UN of {element: node ref}
| Atom of {code: int; action: int; atomtype: atomtype}
;;

let pile: node ref Stack.t = Stack.create ();;

type pcode =
| LDA
| LDC
;;

let p_code: int array = [||];;

let pilex: int array = [||];;

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
| Star{element = e} -> ignore (analyse g e); true
| UN{element = e} -> ignore (analyse g e); true
| Atom{code = c; action = a; atomtype = t} when t = Terminal ->  if c = scang0 then ((if a != 0 then ignore ()); true) else false
| Atom{code = c; action = a; atomtype = t} -> if analyse g g.(c) then ((if a != 0 then ignore ()); true) else false
;;

let rec actiong0 act cr_type = match act with
| 1 -> let t1 = Stack.pop (pile) in let t2 = Stack.pop (pile) in (match !t2 with | Atom { code; _ } -> a.(code) <- t1 | _ -> invalid_arg "Expected Atom node")
| 2 -> Stack.push (genatom ()) pile
| 3 -> let t1 = Stack.pop (pile) in let t2 = Stack.pop (pile) in Stack.push (genunion (t2, t1)) pile
| 4 -> let t1 = Stack.pop (pile) in let t2 = Stack.pop (pile) in Stack.push (genconc (t2, t1)) pile
| 5 -> if cr_type = Terminal then Stack.push (genatom ()) pile else Stack.push (genatom ()) pile
| 6 -> let t1 = Stack.pop (pile) in  Stack.push (genstar (t1)) pile
| 7 -> let t1 = Stack.pop (pile) in Stack.push (genun (t1)) pile
| _ -> invalid_arg "action"
;;

let spx = ref 0;;
let co = ref 0;;

let interpret x = match x with
| LDA -> spx := (!spx + 1) in pilex.(!spx) <- (Array.get (!co+1) p_code) in co := (!co+2)
| _ -> 

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
      genatom (Char.code ',', 1, Terminal)
    )
  ),
  genatom (Char.code ';', 0, Terminal)
)
;;

let n = genatom (Char.code '_', 2, Terminal) (* IDNTer *)
;;

let e = genconc (
  genatom (3, 0, NonTerminal), (* T *)
  genstar (
    genconc (
      genatom (Char.code '+', 0, Terminal),
      genatom (3, 3, NonTerminal) (* T *)
    )
  )
)
;;

let t = genconc (
  genatom (4, 0, NonTerminal), (* F *)
  genstar (
    genconc (
      genatom (Char.code '.', 0, Terminal),
      genatom (4, 4, NonTerminal) (* F *)
    )
  )
)
;;

let f = genunion (
  genunion (
    genunion (
      genunion (
        genatom (Char.code '_', 5, Terminal), (* IDNTer *)
        genatom (Char.code '_', 5, Terminal) (* ELTer *)
      ),
      genconc (
        genconc (
          genatom (Char.code '(', 0, Terminal),
          genatom (2, 0, NonTerminal) (* E *)
        ),
        genatom (Char.code ')', 0, Terminal)
      )
    ),
    genconc (
      genconc (
        genatom (Char.code '[', 0, Terminal),
        genatom (2, 0, NonTerminal) (* E *)
      ),
      genatom (Char.code ']', 6, Terminal)
    )
  ),
  genconc (
    genconc (
      genatom (Char.code '{', 0, Terminal), (* (/ *)
      genatom (2, 0, NonTerminal) (* E *)
    ),
    genatom (Char.code '}', 7, Terminal) (* /) *)
  )
)

let g0 = [| s; n; e; t; f |] ;;

(* ---- main ---- *)

(*
Scan.G0
if analyse A[S] then OK
  *)

print_string (imprim_arbre g0.(0)) ;;
Printf.printf "%b" (analyse g0 g0.(0)) ;;


(* ---- ----- ---- *)
