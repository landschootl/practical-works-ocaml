(*
##########################################
######## ATTENTION : l=liste #############
##########################################

EXERCICE 1 :
*)
(* 6 *)
let rec taille l = match l with
	| [] -> 0
	| _::q -> 1+(taille q);;

let rec minimum l = match l with
	| [] -> failwith "erreur"  		(*cas d'erreur*)
	| [e] -> e						(*cas d'arret*)
	| t::q -> let  m = (minimum q) in if t<m then t else m;;
	
let list = [1;2;3;4;5;6;8;9;7;1;5;2;5;6;4;78;85;41;4;5;54;54;54;87;54;5;84;54;45;54];;

minimum list;;

(*
EXERCICE 2 :
*)
(* 1 *)
let rec inserer l e = match l with (*l est la list et e l'élément à inserer*)
	| [] -> [e]
	| t::q -> if t>e then e::l else t::(inserer q e);;
	
	
ou


let rec inserer l e = match l with (*l est la list et e l'élément à inserer*)
	| [] -> [e]
	| t::q when t>e -> e::l
	| t::q -> t::(inserer q e);;
	

inserer [1;2;3;4;5;6;7;8;9;10] 5;;

(* 2 *)
let rec tri_insertion l = match l with
	| [] -> failwith "erreur"
	| t::q -> inserer (tri_insertion l) t;;

(* 3 *)
let rec sousList l i j = if j<0 then failwith "erreur" else 
	match l with 	(*l est la list et i,j les deux indices*)
		| [] -> failwith "erreur"
		| t::q -> if j=0 then [t] else if i=0 then t::(sousList q i (j-1)) else (sousList q (i-1) (j-1));;

(* 4 *)
let rec fusion l1 l2 = match l1,l2 with
	|([],[])->[]
	|(_,[])->l1
	|([],_)->l2
	|(t1::q1, t2::q2)-> if(t1<t2) then (t1::(fusion q1 l2)) else (t2:: (fusion l1 q2 ));;

(* 5 *)
let rec triFusion l = match l with
	| [] -> []
	| [a] -> [a]
	| a::q -> let n = (taille l) in fusion (triFusion (sousList l 0 (n/2-1))) (triFusion (sousList l ((n/2)) (n-1)));; 			(*La variable n est local et donc connu que après le in*)
