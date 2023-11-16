
(*Vor dem Ausführen den Programms #use "init.ml";; schreiben
   Wenn du nicht das komplette Programm ausführen willst, sondern nur die Funktionen im Toplevel
   ausrufen möhtest, musst du vorher let default_parser = parse_prop_formula;; schreiben*)
#use "src/inint.ml";;

let default_parser = parse_prop_formula;;

(*Aufgabe: Modifiziere den untenstehenden Code für den (rekursiven) Davis-Putnam-Logemann-Loveland-Algorithmus so, 
  dass bei Erfüllbarkeit auch eine gültige Variablenbelegung ausgegeben wird. Versuche die Ausgabe auch gut lesbar zu gestalten. 
  Unten findest du Beispiele zum Testen*)

let one_literal_rule_assignment clauses assignment =
  let u = hd (find (fun cl -> length cl = 1) clauses) in
  let new_assignment = [(u, true)] @ assignment in
  let u' = negate u in
  let clauses1 = filter (fun cl -> not (mem u cl)) clauses in
  let new_clauses = image (fun cl -> subtract cl [u']) clauses1 in
  new_clauses, new_assignment;;

let affirmative_negative_rule_assignment clauses assignment =
  let neg',pos = partition negative (unions clauses) in
  let new_assignment = map (fun l -> (l, true)) pos @ map (fun l -> (l, false)) neg' @ assignment in
  let neg = image negate neg' in
  let pos_only = subtract pos neg and neg_only = subtract neg pos in
  let pure = union pos_only (image negate neg_only) in
  if pure = [] then failwith "affirmative_negative_rule" 
  else filter (fun cl -> intersect cl pure = []) clauses;;


let posneg_count cls l =                         
  let m = length(filter (mem l) cls)                 
  and n = length(filter (mem (negate l)) cls) in
  m + n;;                                  

let rec dpll_assignment clauses assignment =       
  if clauses = [] then true else if mem [] clauses then false else
  try dpll_assignment(one_literal_rule_assignment clauses assignment) with Failure _ ->
  try dpll_assignment(affirmative_negative_rule_assignment clauses assignment) with Failure _ ->
  let pvs = filter positive (unions clauses) in
  let p = maximize (posneg_count clauses) pvs in
  dpll_assignment (insert [p] clauses) new_assignment or dpll_assignment (insert [negate p] clauses) new_assignment;;


let ex1 = defcnfs <<(a \/ b) /\ (a \/ ~c \/ d) /\ (b \/ c \/ ~d) /\ (~a) /\ (a \/ ~d)>>;;
dpll_assignment ex1 [];;

let ex2 = defcnfs <<(a \/ b) /\ (a \/ ~b \/ c) /\ (b \/ ~c) /\ (b \/ c)>>;;
dpll_assignment ex2 [];;

let ex3 = defcnfs << (p \/ q \/ r) /\ (~r \/ ~q \/ p) /\ (~p \/ ~q) /\ (q \/ ~p)>>;;
dpll_assignment ex3 [];;
