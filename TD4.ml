(*
Exercice 1
*)
(**1**)
# "1"="1";;
- : bool = true
# "1"=="1";;
- : bool = false

(**2**)
let first (a,b) = a;;

# first (2,4);;
- : int = 2
# first ("1",false);;
- : string = "1"

(**3**)
let compose f g x = f (g x);;

(**4**)
let id x = x;;

(**5**)
# let l = ((id 1),(id true));;
val l : int * bool = (1, true)
# let g f = ((f true),( f 2));;
Error: This expression has type int but an expression was expected of type
         bool




(*
Exercice 2
*)
(**1**)
type entier = Zero | Succ of entier;; (* un type entier est soit zero ou un Succ d'un autre entier.*)

(**2**)
let rec addition a b = match a with
	| Zero -> b
	| Succ s -> Succ (addition b s);;
	
let a = Succ (Succ Zero);;
let b = Succ (Succ (Succ Zero));;
addition a b;;



let rec multiplication  a b = match a with
	| Zero -> Zero (*si c'est zero on renvois 0*)
	| Succ(Zero) -> b (*si c'est un on renvois b*)
	| Succ s -> addition (multiplication s b) b;;

(**3**)
type listeEntiers = Vide | Cons of entier * listeEntiers;; (*  |=ou     *=et     *)
				 (* Soit l'élément est vide ou un entier, puis il y a un autre élément apres*).

Cons(Zero,Vide);; (* listeEntier contenant que Zero *)
Vide (* listeEntier vide *)
Cons(Zero, Cons(Zero,Vide));; (* listeEntier qui possède deux Zeros *)

(**4**)
let rec additionListeEntier l = match l with
	| Vide -> Zero
	| Cons(t,q) -> addition t (additionListeEntier q);;


let x = Succ(Succ Zero);;
let y = Succ(Succ(Succ Zero));;
let a = Cons( x , Cons(y,Vide) );;
additionListeEntier a;;

(**5**)
type ('a,'b) liste = Vide | ConstA of 'a * ('a,'b) liste | ConstB of 'b * ('a,'b) liste;;

(*
Exercice 3
*)
(**1**)
type 'a arbre = Vide | Element of 'a * 'a arbre * 'a arbre;;

(**2**)
let rec additionArbre a = match a with
	| Vide -> Zero
	| Element(e, d, g) -> addition e (addition (additionArbre d) (additionArbre g));; (*e est l'élement de l'arbre, d est son arbre droit, g est son arbre g*)

(**3**)
let rec projectionArbre a = match a with
	| Vide -> []
	| Element(e, d, g) -> (projectionArbre d) @ [e] @ (projectionArbre g);; (* ON écrase l'arbre, donc les elements de droite se retrouvent à droite, les élements de gauche se retrouvent à gauche et l'élément courant se retrouvent au milieu ! *)
	
(*
Ecrire un type pour les formules logique propositionnelle.
Form -> p|V|F|Form ^ Form|Form v Form|!Form
*)
type form = P of string | V | F | Et of form * form| Ou of form * form| Negation of form;; (*P of string pour indiquer que c'est n'importe quel variable*)

let b = Et(V,F);; (* test 1 *)
let b = Et(Ou(F,V),Negation(V));; (* test 2 *)
let z = Ou(Et(Ou(Et(Et(F,V),V),Et(V,F)),V),Et(Ou(Et(Et(F,V),V),Et(V,F)),V));;


(*
Propagation
*)
let rec simplification f = match f with
	| V -> V
	| F -> F
	| Et(V,V) -> V
	| Et(F,x) -> F
	| Et(x,F) -> F
	| Et(x,V) -> simplification x
	| Et(V,y) -> simplification y
	| Et(x,y) -> Et(simplification x, simplification y)
	| Ou(V,y) -> V
	| Ou(x,V) -> V
	| Ou(x,F) -> simplification x
	| Ou(F,y) -> simplification y
	| Ou(x,y) -> Ou(simplification x, simplification y)
	| Negation(x) -> Negation(simplification x)
	| _ -> f;;

let rec propagation f = match f with
	[ V -> V
	| F -> F
	| _ -> simplification l;;
