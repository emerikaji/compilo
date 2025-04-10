let pile_x: int array = [||];;
let spx: int ref = ref 0;;

let pile_code: int array = [||];;
let c0: int ref = ref 0;;

type pcode =
| LDA | LDV | LDC
| JMP | JIF | JSR | RSR
| SUP | SUPE | INF | INFE | EG | DIFF
| ADD | MIN | MULT | DIV | NEG | INC | DEC
| AND | OR | NOT
| RD | RDLN | WRT | WRTLN
| AFF | STOP
;;

let interpret = function
| LDA ->  (* LDA addr *)
  (
    spx := !spx + 1;
    pile_x.( !spx ) <- pile_code.( !c0 + 1 );
    c0 := !c0 + 2
  )
| LDV -> (* LDV addr *)
  (
    spx := !spx + 1;
    pile_x.( !spx ) <- pile_x.( pile_code.( !c0 + 1 ) );
    c0 := !c0 + 2
  )
| LDC -> (* LDC val *)
  (
    spx := !spx + 1;
    pile_x.( !spx ) <- pile_code.( !c0 + 1 );
    c0 := !c0 + 2
  )
| AFF -> (* addr val AFF *)
  (
    pile_x.(pile_x.( !spx - 1 )) <- pile_x.( !spx );
    spx := !spx - 2;
    c0 := !c0 + 1
  )
| JMP -> (* JMP addr *)
  (
    c0 := pile_code.( !c0 + 1 );
  )
| JIF -> (* cond JIF addr *)
  (
    if (pile_x.( !spx )) = 0 then
      c0 := pile_code.( !c0 + 1 )
    else
      c0 := !c0 + 2;

    spx := !spx - 1
  )
| JSR -> (* JSR addr *)
  (
    spx := !spx + 1;
    pile_x.( !spx ) <- !c0 + 2;
    c0 := pile_code.( !c0 + 1 )
  )
| RSR -> (* addr RSR *)
  (
    c0 := pile_x.( !spx );
    spx := !spx - 1
  )
| SUP -> (* val val SUP *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) > pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| INF -> (* val val INF *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) < pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| SUPE -> (* val val SUPE *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) >= pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| INFE -> (* val val INFE *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) <= pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| EG -> (* val val EG *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) = pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| DIFF -> (* val val DIFF *)
  (
    pile_x.( !spx - 1 ) <- if pile_x.( !spx ) != pile_x.( !spx - 1 ) then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| ADD -> (* val val ADD *)
  (
    pile_x.( !spx - 1 ) <- pile_x.( !spx ) + pile_x.( !spx - 1 );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| MIN -> (* val val MIN *)
  (
    pile_x.( !spx - 1 ) <- pile_x.( !spx ) - pile_x.( !spx - 1 );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| MULT -> (* val val MULT *)
  (
    pile_x.( !spx - 1 ) <- pile_x.( !spx ) * pile_x.( !spx - 1 );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| DIV -> (* val val DIV *)
  (
    pile_x.( !spx - 1 ) <- pile_x.( !spx ) / pile_x.( !spx - 1 );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| NEG -> (* val NEG *)
  (
    pile_x.( !spx ) <- -pile_x.( !spx );
    c0 := !c0 + 1;
  )
| INC -> (* val INC *)
  (
    pile_x.( !spx ) <- pile_x.( !spx ) + 1;
    c0 := !c0 + 1;
  )
| DEC -> (* val DEC *)
  (
    pile_x.( !spx ) <- -pile_x.( !spx ) - 1;
    c0 := !c0 + 1;
  )
| AND -> (* cond cond AND *)
  (
    pile_x.( !spx - 1 ) <- if (pile_x.( !spx ) + pile_x.( !spx - 1 )) = 2 then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| OR -> (* cond cond OR *)
  (
    pile_x.( !spx - 1 ) <- if (pile_x.( !spx ) + pile_x.( !spx - 1 )) > 0 then 1 else 0;
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| NOT -> (* cond NOT *)
  (
    pile_x.( !spx ) <- 1 - pile_x.( !spx );
    c0 := !c0 + 1;
  )
| RD -> (* RD *)
  (
    spx := !spx + 1;
    pile_x.( !spx ) <- read_int ();
    c0 := !c0 + 1;
  )
| RDLN -> (* RDLN *)
  (
    (* TODO *)
    c0 := !c0 + 1;
  )
| WRT -> (* val WRT *)
  (
    Printf.printf "%d" pile_x.( !spx );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| WRTLN -> (* val WRTLN *)
  (
    Printf.printf "%d\n" pile_x.( !spx );
    spx := !spx - 1;
    c0 := !c0 + 1;
  )
| STOP -> invalid_arg "stop shouldn't be there"
;;

let exec = function
| STOP -> exit 0
| x -> interpret x
;;