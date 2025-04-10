type atomtype = Terminal | NonTerminal ;;

type node =
| Conc of {left: node ref ; right: node ref}
| Union of {left: node ref ; right: node ref}
| Star of {element: node ref}
| UN of {element: node ref}
| Atom of {code: int; action: int; atomtype: atomtype}
;;

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