(* Hier werden die Dateien von der Website von Harrison benötigt, alternativ, kann man vorher im Terminal einmal "init.ml" laden*)
#use "init.ml";;          
(*
1. Schreiben sie eine Funktion, ähnlich zu onatoms, die als Input eine Formel fm hat, in der substituiert werden soll,
 eine Formel x, die substituiert werden soll und eine Formel y, die eingesetzt wird. Sie soll also einer Substitution einer Teilformel entsprechen
*)


let rec substitute fm x y = 






(*Testfälle*)
substitute (And(Imp(Atom("x"), Atom("y")), Atom("z") )) (Imp(Atom("x"), Atom("y"))) (Iff(Atom("z"), Atom("x")));;

substitute (Exists("x", Or(Atom("z"), And(Atom("x"),Exists("y",Atom("y"))))))  (Exists("y",Atom("y"))) (Forall("z", Atom("z")));;


(*
2. Schreiben sie eine Funktion, die alle propositionalen Formeln der Größe n erzeugt und als Liste ausgibt.
  Hierbei sollen die Operatoren (And, Or, etc.) von Größe 1 sein und Atome auch die Größe 1 haben.
*)
let rec atomizer k = 
  match k with
  0 -> [True;False]
  | n -> [Atom(P(string_of_int n))] @ (atomizer (n-1));;


let rec makeProp n =


(*Testfall*)
let props = makeProp 3 (atomizer 3);;

(*
3. Schreiben sie eine Funktion, die alle propositionalen Formeln der Größe n als Liste nehmen und als Tupel die Anzahl der Tautologien und die Anzahl von Formeln insgesamt ausgeben.
*)


let rec countTaut lfm taut total =

(*Testfall*)
countTaut props 0 0;;