(*
    Write a new tautology Function which checks if
    a formula is valid or not by using the CNF.
*)

(*
    sear_compl searches for complementary literals
    inside a conjunct.   
*)

let rec sear_compl l storage = 
    match l with
    | [] -> false
    | hd::tl -> if List.mem (neg hd) storage then true else sear_compl tl storage;;

(*
    it_conj will at the end iterate over the CNF and
    checks if each conjunct is valid    
*)

let rec it_conj cnf_set = 
    match cnf_set with
    | [] -> true
    | hd::tl -> if sear_compl hd hd then false else it_conj tl;;


(*
    tautology uses it_conj to check if the formula fm
    is valid.   
*)

let tautology fm = it_conj (purecnf fm);;