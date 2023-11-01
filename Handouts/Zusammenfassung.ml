
type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * ('a)formula
                 | Exists of string * ('a)formula;;

let mk_and p q = And(p,q) and mk_or p q = Or(p,q)
and mk_imp p q = Imp(p,q) and mk_iff p q = Iff(p,q)
and mk_forall x p = Forall(x,p) and mk_exists x p = Exists(x,p);;

let dest_iff fm = 
match fm with Iff(p,q) -> (p,q) | _ -> failwith "dest_iff" ;;

let dest_imp fm = 
match fm with Imp(p,q) -> (p,q) | _ -> failwith "dest_imp" ;;

let antecedent fm = fst(dest_imp fm);;
let consequent fm = snd(dest_imp fm);;

let dest_and fm = 
match fm with And(p,q) -> (p,q) | _ -> failwith "dest_and" ;;

let dest_or fm = 
match fm with Or(p,q) -> (p,q) | _ -> failwith "dest_or" ;;

let rec conjuncts fm = 
match fm with And(p,q) -> conjuncts p @ conjuncts q | _-> [fm];;
let rec disjuncts fm = 
match fm with Or(p,q) -> disjuncts p @ disjuncts q | _-> [fm];;

let rec onatoms f fm = 
  match fm with
  Atom a -> f a
  | Not(p) -> Not(onatoms f p)
  | And(p,q) -> And(onatoms f p, onatoms f q)
  | Or (p,q) -> Or(onatoms f p, onatoms f q)
  | Imp(p,q) -> Imp(onatoms f p, onatoms f q)
  | Iff(p,q) -> Iff(onatoms f p, onatoms f q)
  | Forall(x, p) -> Forall(x, onatoms f p)
  | Exists(x, p) -> Exists(x, onatoms f p)
  | _ -> fm ;;
  
  
  
  
let rec overatoms f fm b = 
match fm with
Atom(a) -> f a b
| Not(p) -> overatoms f p b
| And(p, q) | Or(p,q) |Imp(p,q) |Iff(p,q) ->
    overatoms f p (overatoms f q b)
|Forall(x,p) | Exists(x,p) -> overatoms f p b
| _ -> b;;

let atom_union f fm = setify (overatoms (fun h t -> f(h)@t) fm []);;
let atoms fm = atom_union (fun a -> [a]) fm;;



let default_parser = parse_prop_formula;;
type prop = P of string;;

let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p,q) -> (eval p v) & (eval q v)
  | Or(p,q) -> (eval p v) or (eval q v)
  | Imp(p,q) -> not(eval p v) or (eval q v)
  | Iff(p,q) -> (eval p v) = (eval q v);;

    

let rec onallvaluations subfn v ats =
  match ats with
    [] -> subfn v
  | p::ps -> let v' t q = if q = p then t else v(q) in
              onallvaluations subfn (v' false) ps &
              onallvaluations subfn (v' true) ps;;


let print_truthtable fm =
  let ats = atoms fm in
  let width = itlist (max ** String.length ** pname) ats 5 + 1 in
  let fixw s = s^String.make(width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
      let lis = map (fun x -> truthstring(v x)) ats
      and ans = truthstring(eval fm v) in
      print_string(itlist (^) lis ("| "^ans)); print_newline(); true in
  let separator = String.make (width * length ats + 9) '-' in
  print_string(itlist (fun s t -> fixw(pname s) ^ t) ats "| formula");
  print_newline(); print_string separator; print_newline();
  let _ = onallvaluations mk_row (fun x -> false) ats in
  print_string separator; print_newline();;




let tautology fm =
  onallvaluations (eval fm) (fun s -> false) (atoms fm);;
      
let unsatisfiable fm = tautology(Not fm);;
      
let satisfiable fm = not(unsatisfiable fm);;
      
      
      
      
let psubst subfn = onatoms (fun p -> tryapplyd subfn p (Atom p));;