(* Hier werden die Dateien von der Website von Harrison benötigt, alternativ, kann man vorher im Terminal einmal "init.ml" laden*)
#use "src/init.ml";;          
(*
1. Schreiben sie eine Funktion, ähnlich zu onatoms, die als Input eine Formel fm hat, in der substituiert werden soll,
 eine Formel x, die substituiert werden soll und eine Formel y, die eingesetzt wird. Sie soll also einer Substitution einer Teilformel entsprechen
*)


let rec substitute fm x y = 
  match fm with
  True -> True
  | False -> False
  | Atom(a) -> if (Atom(a) = x) then y else Atom(a)
  | Not(a) -> Not(substitute a x y)
  | And(a,b) -> And(substitute a x y, substitute b x y)
  | Or(a,b) -> Or(substitute a x y, substitute b x y)
  | Imp(a,b) -> Imp(substitute a x y, substitute b x y)
  | Iff(a,b) -> Iff(substitute a x y, substitute b x y)
  | Forall(a,b) -> Forall(a, substitute b x y)
  | Exists(a,b) -> Exists(a, substitute b x y);;






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

let rec listFirst = function
  [] -> failwith "Empty List"
  | x::xs -> x;;

let rec listSecond = function
  [] -> failwith "Empty List"
  | x::xs -> listFirst xs;;

let rec sublistsTwo = function
  [] -> []
  | x::xs -> (List.map (fun y -> [x;y]) xs) @ [[x;x]] @ (sublistsTwo xs);;

let rec makePropStep i =
  match i with
  [] -> []
  | fm::rest -> [And(listFirst fm, listSecond fm)] @ [Or(listFirst fm, listSecond fm)] @ [Imp(listFirst fm, listSecond fm)] @ [Iff(listFirst fm, listSecond fm)] @ (makePropStep rest);;

let rec makePropStepNot i =
  match i with
  [] -> []
  | fm::rest -> [Not(fm)] @ (makePropStepNot rest);;

let rec makeProp n i =
  match n with
  0 -> failwith "n must be greater than 0"
  | 1 -> i
  | n when n mod 2 = 0 -> makePropStepNot (makeProp (n - 1) i)
  | n -> makePropStep (sublistsTwo (makeProp ((n - 1)/2 ) i)) @ makePropStepNot (makeProp (n - 1) i);;


  (*Testfall*)
let props = makeProp 3 (atomizer 3);;

(*
3. Schreiben sie eine Funktion, die alle propositionalen Formeln der Größe n als Liste nehmen und als Tupel die Anzahl der Tautologien und die Anzahl von Formeln insgesamt ausgeben.
*)


let rec countTaut lfm taut total =
  match lfm with
  [] -> (taut, total)
  | fm::rest -> if (tautology fm) then (countTaut rest (taut+1) (total+1)) else (countTaut rest taut (total+1));;

(*Testfall*)
countTaut props 0 0;;