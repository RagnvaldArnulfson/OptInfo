let rec suppr_rep = function
	| [] -> []
	| h::d::q when h = d -> suppr_rep (d::q)
	| h::d -> h::suppr_rep d;;
	
suppr_rep [1;5;5;4;8;9;6;5;4;4;5;6;5;5;5;5;5;2;4;4];;

let rec selec p = function
	| [] -> []
	| h::q when p(h) -> h::selec p q
	| _::q -> selec p q;;

selec (fun a -> a mod 3 = 1) [57;4;97;47;51];;


exception reliquat_non_nul of int * int list;;

let rec caissier_term d c =	
	let rec cais d = fun
		| [] billets -> if d <> 0 then raise (reliquat_non_nul (d, billets)) else billets
		| (h::q) billets when h <= d -> cais (d - h) (h::q) (h::billets)
		| (h::q) billets -> cais d q billets
	in cais d c [];;

caissier_term 483 [200;100;80;10;3];;

(*let rec caissier_s d = function
	| [] -> 0
	| h::q -> let k = d/h in h*k + caissier_s (d-k*h) q;;

let rec caissier init d = function
	| [] -> init
	| h::q -> let k = d/h in (caissier init (d-k*h) q) - h*k;;

let rec caissier_bis d = fun
	| [] atteint -> d
	| (h::q) atteint when atteint + h <= d -> caissier_bis d (h::q) (atteint+h) - h
	| (h::q) atteint -> caissier_bis d q atteint;;

caissier_bis 485 [200;100;50;20] 0;;

caissier_s 485 [200;100;50;20];;
caissier 485 485 [200;100;50;20];;*)

exception reliquat_non_nul of int;;

let rec caissier = fun 
 | [] 0 -> []
 | [] a -> raise (reliquat_non_nul a)
 | (h::q) a when h <= a -> t::(caissier (t::q) (a - h))
 | (t::q) a -> caissier q a;;

caissier [200;100;50;20] 473;;

(*let rec ppdiv n = function
	| d when d*d > n -> n
	| d when n mod d = 0 -> d
	| d -> ppdiv n (d+1);;

let rec decomp d = function
	| 1 -> d
	| n -> let div = (ppdiv n 2) in decomp (div::d) (n/div);;
	
let rec regroupe rgrp compt = function
	| [] -> rgrp
	| h::d::q when h = d -> regroupe rgrp (compt+1) (d::q)
	| h::q -> regroupe ((h,compt)::rgrp) 1 q;;
	
let rec imprime resu = function
	| [] -> resu
	| (a,n)::q when q <> [] & n <> 1 -> imprime (resu ^ string_of_int(a) ^ "^" ^ string_of_int(n) ^ " * ") q
	|	(a,n)::q when q <> [] -> imprime (resu ^ string_of_int(a) ^ " * ") q
	| (a,n)::q when n <> 1 -> imprime (resu ^ string_of_int(a) ^ "^" ^ string_of_int(n)) q
	| (a,n)::q -> imprime (resu ^ string_of_int(a)) q;;*)
	
let decomposition n =
	let rec ppdiv n = function
		| d when d*d > n -> n
		| d when n mod d = 0 -> d
		| d -> ppdiv n (d+1)
		
	and decomp d = function
		| 1 -> d
		| n -> let div = (ppdiv n 2) in decomp (div::d) (n/div)
	
	and regroupe rgrp compt = function
	| [] -> rgrp
	| h::d::q when h = d -> regroupe rgrp (compt+1) (d::q)
	| h::q -> regroupe ((h,compt)::rgrp) 1 q
		
	and imprime resu = function
	| [] -> resu
	| (a,n)::q when q <> [] & n <> 1 -> imprime (resu ^ string_of_int(a) ^ "^" ^ string_of_int(n) ^ " * ") q
	|	(a,n)::q when q <> [] -> imprime (resu ^ string_of_int(a) ^ " * ") q
	| (a,n)::q when n <> 1 -> imprime (resu ^ string_of_int(a) ^ "^" ^ string_of_int(n)) q
	| (a,n)::q -> imprime (resu ^ string_of_int(a)) q
		
	in imprime "" (regroupe [] 1 (decomp [] n));;

decomposition 7521;;

let rec intervalle = fun
	| 0 0 l -> [] 
	| 0 b (h::q) -> h::(intervalle 0 (b-1) q)
	| a b (h::q) -> intervalle (a-1) b q
	| a b [] -> [];;

let operation c d =
	let (a,a') = c and (b,b') = d in
	if b - a > 0
		then
		begin
			if b mod a = 0 then
				(b - a, "(" ^ b' ^ " - " ^ a' ^ ")")::(b / a, "(" ^ b' ^ " / " ^ a' ^ ")")::[a + b, "(" ^ a' ^ " + " ^ b' ^ ")"; a * b, "(" ^ a' ^ " * " ^ b' ^ ")"]
			else
				(b - a, "(" ^ b' ^ " - " ^ a' ^ ")")::[a + b, "(" ^ a' ^ " + " ^ b' ^ ")"; a * b, "(" ^ a' ^ " * " ^ b' ^ ")"]
		end
	else if a <> b
		then
		begin
			if a mod b = 0 then
				(a - b, "(" ^ a' ^ " - " ^ b' ^ ")")::(a / b, "(" ^ a' ^ " / " ^ b' ^ ")")::[a + b, "(" ^ a' ^ " + " ^ b'  ^ ")"; a * b, "(" ^ a' ^ " * " ^ b' ^ ")"]
			else
				(a - b, "(" ^ a' ^ " - " ^ b' ^ ")")::[a + b, "(" ^ a' ^ " + " ^ b'  ^ ")"; a * b, "(" ^ a' ^ " * " ^ b' ^ ")"]
		end
	else
		(a / b, "(" ^ a' ^ " / " ^ b' ^ ")")::[a + b, "(" ^ a' ^ " + " ^ b' ^ ")"; a * b, "(" ^ a' ^ " * " ^ b' ^ ")"];;

let rec elem_comp_liste t = function
	| [] -> []
	| h::q -> (operation t h)@(elem_comp_liste t q);;

let rec liste_comp_liste a = function
	| [] -> []
	| h::q -> (elem_comp_liste h a)@(liste_comp_liste a q);;

let rec decoupe = function
  | [] -> []
  | [a] -> []
  | [a; b] -> [[a], [b]]
  | (h::q) -> ([h], q)::(map (function x, y -> (x, y)) (decoupe q)) ;;	
	
let evaluation plaquettes =
	let rec eval = function
		| [] -> []
		| (a,b)::q -> liste_comp_liste a b@eval q
	in eval (decoupe plaquettes);;

let etape = function
	| [] -> []
	| (a,b)::q -> (elem_comp_liste a (map (function x, y -> x) q))@q;;

let etape = function
	| [] -> []
	| a::q -> elem_comp_liste a q@q;;

let rec find_n n = function
	| [] -> ""
	| (a,b)::q when a = n -> b
	| (a,b)::q -> find_n n q;;

(*let evaluation plaquettes =
	let rec eval = function
		| [] -> []
		| (a,b)::q -> etape q plaquettes
	in eval (decoupe plaquettes);;*)

let lecompteestbon n p =
	let rec comptebon n plaquettes =
		let plaq = evaluation plaquettes in
			let ans = find_n n plaq in
				if ans = "" then (comptebon n plaq) else ans
	in comptebon n (map (function x -> x, string_of_int(x)) p);;

lecompteestbon 952 [25;50;75;100;3;6];;
