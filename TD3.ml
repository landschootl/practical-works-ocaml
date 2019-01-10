(*
Exercice 1
*)

(* 1 *)
let rec puissance b n = match n with
	| 0 -> 1
	| _ -> b * puissance b (n-1);;
	
(* 2 *)
let rec puissance_rapide b n =
	if n=0 then 1
	else if b mod 2 = 0 then puissance_rapide (b*b) (n/2)
	else b * (puissance_rapide b (n-1));;
	

(*
Exercice 2
*)

(* 1 *)
let rec n_a_un n = match n with
	| 0 -> []
	| _ -> n::(n_a_un (n-1));;
	
(* 2 *)
let rec un_a_n n = match n with
	| 0 -> []
	| _ -> (un_a_n (n-1)) @ [n];;
	
(* 3 *)
let rec supprime l n = match l with
	| [] -> []
	| t::q -> if t<n then (supprime q n) else t::(supprime q n);;

(* 4 *)
let rec long_prefixe l = match l with
	| [] -> 0
	| [e] -> 1
	| t1::t2::q -> if t2=t1 then 1+(long_prefixe (t1::q)) else 1;;
	
(* 5 *)
let rec inverser l = match l with
	| [] -> []
	| t::q -> (inverser q) @ [t];;
	
(* 6 *)
let rec long_suffixe l = match l with
	| [] -> 0
	| _ -> (long_prefixe (inverser l));;
	
(* 7 *)
let rec appartient e l = match l with
	| [] -> false;
	| t::q -> if t=e then true else (appartient e q);;
	
(* 8 *)
let rec nb_occ e l = match l with
	| [] -> 0
	| t::q -> if t=e then 1+(nb_occ e q) else (nb_occ e q);;
	
(* 9 *)
let rec ensemble l = match l with
	| [] -> []
	| t::q -> if (appartient t q) then (ensemble q) else t::(ensemble q);;
'afirst 
(* 10 *)
let rec regroupe l = match l with
	| [] -> []
	| t::q -> if (appartient t (inverser q)) then (regroupe q) else (t,nb_occ t l)::(regroupe q);;    

