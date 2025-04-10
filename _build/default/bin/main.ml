open Compilo.Node

let dico_t: (string, int) Hashtbl.t = Hashtbl.create 12;;
Hashtbl.add dico_t "->" 0;;
Hashtbl.add dico_t "," 1;;
Hashtbl.add dico_t ";" 2;;
Hashtbl.add dico_t "+" 3;;
Hashtbl.add dico_t "." 4;;
Hashtbl.add dico_t "(" 5;;
Hashtbl.add dico_t ")" 6;;
Hashtbl.add dico_t "[" 7;;
Hashtbl.add dico_t "]" 8;;
Hashtbl.add dico_t "(/" 9;;
Hashtbl.add dico_t "/)" 10;;
Hashtbl.add dico_t "IDNTer" 11;;
Hashtbl.add dico_t "ELTer" 12;;

let dico_nt: (string, int) Hashtbl.t = Hashtbl.create 4;;
Hashtbl.add dico_nt "N" 1;;
Hashtbl.add dico_nt "E" 2;;
Hashtbl.add dico_nt "T" 3;;
Hashtbl.add dico_nt "F" 4;;

let s = genconc (
  genstar (
    genconc (
      genconc (
        genconc (
          genatom (Hashtbl.find dico_nt "N", 0, NonTerminal),
          genatom (Hashtbl.find dico_t "->", 0, Terminal)
        ),
        genatom (Hashtbl.find dico_nt "E", 0, NonTerminal)
      ),
      genatom (Hashtbl.find dico_t ",", 1, Terminal)
    )
  ),
  genatom (Hashtbl.find dico_t ";", 0, Terminal)
)
;;

let n = genatom (Hashtbl.find dico_t "IDNTer", 2, Terminal)
;;

let e = genconc (
  genatom (Hashtbl.find dico_nt "T", 0, NonTerminal),
  genstar (
    genconc (
      genatom (Hashtbl.find dico_t "+", 0, Terminal),
      genatom (Hashtbl.find dico_nt "T", 3, NonTerminal)
    )
  )
)
;;

let t = genconc (
  genatom (Hashtbl.find dico_nt "F", 0, NonTerminal),
  genstar (
    genconc (
      genatom (Hashtbl.find dico_t ".", 0, Terminal),
      genatom (Hashtbl.find dico_nt "F", 4, NonTerminal)
    )
  )
)
;;

let f = genunion (
  genunion (
    genunion (
      genunion (
        genatom (Hashtbl.find dico_t "IDNTer", 5, Terminal),
        genatom (Hashtbl.find dico_t "ELTer", 5, Terminal)
      ),
      genconc (
        genconc (
          genatom (Hashtbl.find dico_t "(", 0, Terminal),
          genatom (Hashtbl.find dico_nt "E", 0, NonTerminal)
        ),
        genatom (Hashtbl.find dico_t ")", 0, Terminal)
      )
    ),
    genconc (
      genconc (
        genatom (Hashtbl.find dico_t "[", 0, Terminal),
        genatom (Hashtbl.find dico_nt "E", 0, NonTerminal)
      ),
      genatom (Hashtbl.find dico_t "]", 6, Terminal)
    )
  ),
  genconc (
    genconc (
      genatom (Hashtbl.find dico_t "(/", 0, Terminal),
      genatom (Hashtbl.find dico_nt "E", 0, NonTerminal)
    ),
    genatom (Hashtbl.find dico_t "/)", 7, Terminal)
  )
)

let g0 = [| s; n; e; t; f |];;

let pile: node ref Stack.t = Stack.create ();;

let tokens = ref [||];;
let token_index = ref (-1);;
let current_token = ref 0;;

let scan_g0 () =
  token_index := !token_index + 1;
  if !token_index < Array.length !tokens then
    let token_str = (!tokens).(!token_index) in
    Printf.printf "Trying to match token: %s\n" token_str;
    current_token := Hashtbl.find dico_t token_str
  else
    current_token := -1
;;

let tokens_gpl = ref [||];;
let token_index_gpl = ref (-1);;
let current_token_gpl = ref 0;;

let scan_gpl () =
  token_index_gpl := !token_index_gpl + 1;
  if !token_index_gpl < Array.length !tokens_gpl then
    let token_str = (!tokens_gpl).(!token_index_gpl) in
    Printf.printf "Trying to match token: %s\n" token_str;
    current_token_gpl := Hashtbl.find dico_t token_str
  else
    current_token_gpl := -1
;;

scan_g0 ()

let print_stack pile =
  Printf.printf "\nCurrent stack state:\n";
  Stack.iter (fun el -> 
    match !el with
    | Atom { code; _ } -> Printf.printf "Atom with code: %d\n" code
    | _ -> Printf.printf "Other node type\n"
  ) pile;
  Printf.printf "End of stack state\n\n"
;;

let a_gpl: node ref array = Array.make (Array.length g0)  (genatom (0, 0, Terminal));;

let action_g0 c act atype = match act with
| 1 ->
  print_stack pile;  (* Print stack before action *)
  let t1 = Stack.pop (pile) in
  let t2 = Stack.pop (pile) in
  (
    match !t2 with
    | Atom { code; _ } -> print_int code; a_gpl.(code) <- t1
    | _ -> invalid_arg "Expected Atom node"
  )
| 2 ->
  print_stack pile;  (* Print stack before action *)
  Stack.push (genatom (c, act, atype)) pile
| 3 ->
  print_stack pile;  (* Print stack before action *)
  let t1 = Stack.pop (pile) in
  let t2 = Stack.pop (pile) in
  Stack.push (genunion (t2, t1)) pile
| 4 ->
  print_stack pile;  (* Print stack before action *)
  let t1 = Stack.pop (pile) in
  let t2 = Stack.pop (pile) in
  Stack.push (genconc (t2, t1)) pile
| 5 when atype = Terminal ->
  print_stack pile;  (* Print stack before action *)
  Stack.push (genatom (c, act, Terminal)) pile
| 5 when atype = NonTerminal ->
  print_stack pile;  (* Print stack before action *)
  Stack.push (genatom (c, act, NonTerminal)) pile
| 6 ->
  print_stack pile;  (* Print stack before action *)
  let t1 = Stack.pop (pile) in
  Stack.push (genstar (t1)) pile
| 7 ->
  print_stack pile;  (* Print stack before action *)
  let t1 = Stack.pop (pile) in
  Stack.push (genun (t1)) pile
| _ -> invalid_arg "action"
;;

let action_gpl c act atype = match act with
| _ -> ignore (genatom (c, act, atype))
;;


let rec analyse_g0 n = match !n with
| Conc{left = l ; right = r} when analyse_g0 l -> analyse_g0 r
| Conc{left = _ ; right = _} -> false
| Union{left = l ; right = _} when analyse_g0 l -> true
| Union{left = _ ; right = r} -> analyse_g0 r
| Star{element = e} -> ignore (analyse_g0 e); true
| UN{element = e} -> ignore (analyse_g0 e); true
| Atom{code = c; action = a; atomtype = t} when t = Terminal -> 
  if c = !current_token then
    (
      if a != 0 then
        (
          scan_g0 ();
          ignore (action_g0 c a t)
        );
      true
    )
  else false
| Atom{code = c; action = a; atomtype = t} ->
  if analyse_g0 g0.(c) then
    (
      if a != 0 then
        (
          ignore (action_g0 c a t)
        );
      true
    )
  else false
;;

let rec analyse_gpl n = match !n with
| Conc{left = l ; right = r} when analyse_gpl l -> analyse_gpl r
| Conc{left = _ ; right = _} -> false
| Union{left = l ; right = _} when analyse_gpl l -> true
| Union{left = _ ; right = r} -> analyse_gpl r
| Star{element = e} -> ignore (analyse_gpl e); true
| UN{element = e} -> ignore (analyse_gpl e); true
| Atom{code = c; action = a; atomtype = t} when t = Terminal -> 
  if c = !current_token_gpl then
    (
      if a != 0 then
        (
          scan_gpl ();
          ignore (action_gpl c a t)
        );
      true
    )
  else false
| Atom{code = c; action = a; atomtype = t} ->
  if analyse_gpl a_gpl.(c) then
    (
      if a != 0 then
        (
          ignore (action_gpl c a t)
        );
      true
    )
  else false
;;

Format.print_bool (analyse_g0 g0.(0));;

Format.print_bool (analyse_gpl a_gpl.(0));;
