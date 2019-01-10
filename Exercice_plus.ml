(*
Ecrire un programme Caml permettant de calculer la surface d'un cercle à partir de son rayon:
*)

# let pi = 3.14;;
val pi : float = 3.14
# let rayon r = pi*.r*.r;;
val rayon : float -> float = <fun>
# rayon 2.0;;
- : float = 12.56


(*
Ecrire une fonction permettant de vérifier si une varaible est divisible par une autre:
*)

# let divisible (a,b) = a mod b=0;;
val divisible : int * int -> bool = <fun>
# divisible(10,5);;
- : bool = true


(*
Ecrire une fonction permettant de calculer la somme des nombres entiers qui vont de x à y:
*)

let rec somme(x,y)= if x=y then x else if x>y then somme(y,x) else x+somme(x+1,y);;


(*
Ecrire des fonctions mutuellement récursives en utilisant la définition suivante de la suite de Fibonacci:
U0=1 ; V0=1; Un=U(n-1)+2*V(n-1); Vn=3*U(n-1)+V(n-1)
*)

(*metode 1*)
let rec u(n) = if n=0 then 1 else u(n-1)+2*v(n-1) and v(n) = if n=0 then 1 else 3*u(n-1)+v(n-1);;

(*metode 2*)
let rec u = function n -> if (n=0) then 1 else u(n-1)+2*v(n-1)
and v = function n -> if (n=0) then 1 else 3*u(n-1)+v(n-1);;


(*
Ecrire une fonction permettant la composition de deux fonctions.
*)

let compose f g x = f (g x);;
