(*Beispiel Regel II:*)

let default_parser = parse_prop_formula;;

let e1 = [[<<p>>;<<r>>;<<~q>>];[<<~r>>;<<p>>];[<<q>>;<<~r>>]];;

affirmative_negative_rule e1;;

(*Beispiel Regel III:*)

let e2 = [[<<p>>;<<r>>;<<q>>];[<<~r>>;<<~q>>;<<p>>];[<<~p>>;<<~q>>];[<<q>>;<<~p>>]];;

resolution_rule e2;;

(*Beispiel Regel I:*)

let e3 = [[<<p>>;<<r>>;<<~s>>];[<<q>>;<<r>>];[<<s>>;<<p>>];[<<s>>]];;

one_literal_rule e3;;