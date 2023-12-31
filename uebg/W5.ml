(* Code von Johannes Folltmann, Simon Korswird und Ludwig Monnerjahn *)

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
  let assignment = [(u, true)] @ assignment in
  let u' = negate u in
  let clauses1 = filter (fun cl -> not (mem u cl)) clauses in
  image (fun cl -> subtract cl [u']) clauses1, assignment;;

let affirmative_negative_rule_assignment clauses assignment =
  let neg',pos = partition negative (unions clauses) in
  let assignment = map (fun l -> (l, true)) pos @ map (fun l -> (l, false)) neg' @ assignment in
  let neg = image negate neg' in
  let pos_only = subtract pos neg and neg_only = subtract neg pos in
  let pure = union pos_only (image negate neg_only) in
  if pure = [] then failwith "affirmative_negative_rule" 
  else filter (fun cl -> intersect cl pure = []) clauses, assignment;;


let posneg_count cls l =                         
  let m = length(filter (mem l) cls)                 
  and n = length(filter (mem (negate l)) cls) in
  m + n;;                                  


let rec dpll_assignment clauses assignment =
  if clauses = [] then true, assignment
  else if List.mem [] clauses then false, assignment
  else
    try
      let result, new_assignment = one_literal_rule_assignment clauses assignment in
      dpll_assignment result new_assignment
    with Failure _ ->
      try
        let result, new_assignment = affirmative_negative_rule_assignment clauses assignment in
        dpll_assignment result new_assignment
      with Failure _ ->
        let pvs = List.filter positive (List.flatten clauses) in
        let p = maximize (posneg_count clauses) pvs in
        let true_branch, true_assignment = dpll_assignment (insert [p] clauses) (assignment @ [(p, true)]) in
        let false_branch, false_assignment = dpll_assignment (insert [negate p] clauses) (assignment @ [(p, false)]) in
        true_branch || false_branch, true_assignment @ false_assignment;;
  

let ex1 = defcnfs <<(a \/ b) /\ (a \/ ~c \/ d) /\ (b \/ c \/ ~d) /\ (~a) /\ (a \/ ~d)>>;;
dpll_assignment ex1 [];;

let ex2 = defcnfs <<(a \/ b) /\ (a \/ ~b \/ c) /\ (b \/ ~c) /\ (b \/ c)>>;;
dpll_assignment ex2 [];;

let ex3 = defcnfs << (p \/ q \/ r) /\ (~r \/ ~q \/ p) /\ (~p \/ ~q) /\ (q \/ ~p)>>;;
dpll_assignment ex3 [];;
