(*
EXERCICE
*)
(**1**)
type avion = {reference:int; annee:int; nbHeureVol: float; capacitePassagers: int; louer: bool};;

let avion0 = {reference=0; annee=1999; nbHeureVol=150.21; capacitePassagers=285; louer=true};;
let avion1 = {reference=1; annee=1874; nbHeureVol=174.95; capacitePassagers=571; louer=false};;
let avion2 = {reference=2; annee=2012; nbHeureVol=56.78; capacitePassagers=321; louer=false};;

let garage = [avion0;avion1;avion2];;

(**2**)
let rec caractAvion l r = match l with (* l=liste avion et r=reference *)
	| [] -> failwith "element non present dans la liste"
	| t::q -> if t.reference=r then t else caractAvion q r;;
	
(**3**)
let rec listeRef l n = match l with (* l=liste avion et n=nombre place *)
	| [] -> []
	| t::q -> if t.louer=false && t.capacitePassagers>=n then t.reference::(listeRef q n) else listeRef q n;;
	
(**4**)
let rec update l nbHeures r = match l with
	| [] -> []
	| t::q -> if t.reference=r then [{reference=t.reference; annee=t.annee; nbHeureVol=t.nbHeureVol+.nbHeures; capacitePassagers=t.capacitePassagers; louer=not(t.louer)}] else update q nbHeures r;;
	
(**5**)
let rec listeRef l hVol = match l with (* l=liste avion et hVol=heure de vol *)
	| [] -> []
	| t::q -> if t.nbHeureVol>hVol then t.reference::(listeRef q hVol) else listeRef q hVol;;
	
(**6**)
let plusVieuxAvion liste=
	let rec plusVieux l max r=match l with
		| []->[]
		| [e]-> if max>e.annee then [e.annee;e.reference]  else [max;r]
		| t::q-> if max>t.annee then plusVieux q t.annee t.reference else plusVieux q max r
	in plusVieux liste 10000 0;;

(**7**)
let rec sousListe l m p= match l with
	| [] -> []
	| t::q -> if m >= 0 && t.louer=false && t.capacitePassagers>=p then  t::(sousListe q (m-1) p) else (sousListe q (m-1) p);;

(**8**)
let rec majAvion l p m=match l with
	| []-> []
	| t::q-> if t.capacitePassagers>=m && t.louer=false && p>=0 then {reference=t.reference ;annee=t.annee; nbHeureVol=t.nbHeureVol; capacitePassagers=t.capacitePassagers; louer=true}::majAvion q (p-1) m else majAvion q (p-1) m;;
	
(**9**)
let rec supAvion l a = match l with
	| [] -> []
	| t::q -> if t.annee <= a then t::supAvion q a else supAvion q a;;
	
(**10**)
let rec retourGarage l p = match l with
	| [] -> []
	| t::q -> if t.capacitePassagers=p then t::(retourGarage q p) else retourGarage q p;;
	
(**11**) (*Avec le tri QuickSort*)
let rec trier l = match l with
	| [] -> []
	| [t] -> [t]
	| t::q -> 
