#use "OCaml/init.ml";;
#use "OCaml/intro.ml";;
#use "Vortrag/preparation.ml";;

let psimplify1 fm =
  match fm with
    Not False -> True
  | Not True -> False
  | Not(Not p) -> p
  | And(p,False) | And(False,p) -> False
  | And(p,True) | And(True,p) -> p
  | Or(p,False) | Or(False,p) -> p
  | Or(p,True) | Or(True,p) -> True
  | Imp(False,p) | Imp(p,True) -> True
  | Imp(True,p) -> p
  | Imp(p,False) -> Not p
  | Iff(p,True) | Iff(True,p) -> p
  | Iff(p,False) | Iff(False,p) -> Not p
  | _ -> fm;;

let rec psimplify fm =
  match fm with
    Not p -> psimplify1(Not(psimplify p))
  | And(p,q) -> psimplify1(And(psimplify p, psimplify q))
  | Or(p,q) -> psimplify1(Or(psimplify p, psimplify q))
  | Imp(p,q) -> psimplify1(Imp(psimplify p , psimplify q))
  | Iff(p,q) -> psimplify1(Iff(psimplify p , psimplify q))
  | _ -> fm;;

(* examples:
   psimplify <<(true ==> (x <=> false)) ==> ~(y \/ false /\ z)>>;;
   psimplify <<((x ==> y) ==> true) \/ ~false>>;;
*)

let negative = function (Not p) -> true | _ -> false;;

let positive lit = not(negative lit);;

let negate = function (Not p) -> p | p -> Not p;;

(* "normal" negation normalform*)

let rec nnf fm =
  match fm with
  | And(p,q) -> And(nnf p,nnf q)
  | Or(p,q) -> Or(nnf p,nnf q)
  | Imp(p,q) -> Or(nnf(Not p),nnf q)
  | Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
  | Not(Not p) -> nnf p
  | Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
  | Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
  | Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
  | Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
  | _ -> fm;;

let nnf fm = nnf(psimplify fm);;

(*
    example: let fm = <<(p <=> q) <=> ~(r ==> s)>>;;
*)

(*  modified negation normalform *)
(*  let's equivalence how it is and otherwise it's the same
    the function aboth. We do this to avoid the exponential
    blow up.
*)

let rec nenf fm =
  match fm with
  Not(Not p) -> nenf p
  | Not(And(p,q)) -> Or(nenf(Not p),nenf(Not q))
  | Not(Or(p,q)) -> And(nenf(Not p),nenf(Not q))
  | Not(Imp(p,q)) -> And(nenf p,nenf(Not q))
  | Not(Iff(p,q)) -> Iff(nenf p,nenf(Not q))
  | And(p,q) -> And(nenf p,nenf q)
  | Or(p,q) -> Or(nenf p,nenf q)
  | Imp(p,q) -> Or(nenf(Not p),nenf q)
  | Iff(p,q) -> Iff(nenf p,nenf q)
  | _ -> fm;;

let nenf fm = nenf(psimplify fm);;

(* DNF via Truthables: *)

let list_conj l = if l = [] then True else end_itlist mk_and l;;

let list_disj l = if l = [] then False else end_itlist mk_or l;;

(*  mk_lits constructs and combines our Literals in the right way
    for one disjunct.
*)
let mk_lits pvs v =
  list_conj (map (fun p -> if eval p v then p else Not p) pvs);;

(*  allsatvaluations gives us a List of all valuations v which
    hold under subfn.
*)

let rec allsatvaluations subfn v pvs =
  match pvs with
    [] -> if subfn v then [v] else []
  | p::ps -> let nv t q = if q = p then t else v(q) in
    allsatvaluations subfn (nv false) ps @
    allsatvaluations subfn (nv true) ps;;

(*
    dnf now uses allsatvaluations to get all valuations v
    under which fm holds and then using this to get the DNF
*)

let dnf fm =
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun s -> false) pvs in
  list_disj (map (mk_lits (map (fun p -> Atom p) pvs)) satvals);;

(* example: let fm =  <<~(p_1 \/ ~(~p_2 /\ p_3)) \/ (p_3 /\ ~(~p_2 \/ p_1))>>;; *)

(* DNF via transformation *)

let rec distrib fm =
  match fm with
      And(p,(Or(q,r))) -> Or(distrib(And(p,q)),distrib(And(p,r)))
    | And(Or(p,q),r) -> Or(distrib(And(p,r)),distrib(And(q,r)))
    | _ -> fm;;

let rec rawdnf fm =
  match fm with
      And(p,q) -> distrib(And(rawdnf p,rawdnf q))
    | Or(p,q) -> Or(rawdnf p,rawdnf q)
    | _ -> fm;;


(* Set based representation of DNF *)

let distrib s1 s2 = setify(allpairs union s1 s2);;

let rec purednf fm =
  match fm with
      And(p,q) -> distrib (purednf p) (purednf q)
    | Or(p,q) -> union (purednf p) (purednf q)
    | _ -> [[fm]];;

(* example: let fm = <<(p \/ q /\ r) /\ (~p \/ ~r)>>;; *)

(*  trivial sorts out Disjuncts which are somwhoew of the
    form .../\ p /\ ... /\ ~p /\ ... because they will never
    be satisfied.
*)

let trivial lits =
  let pos,neg = partition positive lits in
  intersect pos (image negate neg) <> [];;

(* example: filter (non trivial) (purednf <<(p \/ q /\ r) /\ (~p \/ ~r)>>);; *)

(*
    simpdnf now uses trivial to simplify the formula and addtionally
    is doing subsumption to simplify (e.g. D1 \/ D2 \/ ... with
    D1 = l1 /\ l2 /\.../\ln and D2 = l1 /\ l2 /\.../\ ln /\ k1 /\... /\km)
    can be transformed to D1 \/ D3 \/ ...
*)

let simpdnf fm =
  if fm = False then [] else if fm = True then [[]] else
  let djs = filter (non trivial) (purednf(nnf fm)) in
  filter (fun d -> not(exists (fun nd -> psubset nd d) djs)) djs;;

let dnf fm = list_disj(map list_conj (simpdnf fm));;

(* CNF: *)

let purecnf fm = image (image negate) (purednf(nnf(Not fm)));;

let simpcnf fm =
  if fm = False then [[]] else if fm = True then [] else
  let cjs = filter (non trivial) (purecnf fm) in
  filter (fun c -> not(exists (fun nc -> psubset nc c) cjs)) cjs;;

let cnf fm = list_conj(map list_disj (simpcnf fm));;