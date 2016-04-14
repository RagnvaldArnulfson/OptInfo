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

caissier_term 445 [200;100;80;10;3];;

exception reliquat_non_nul of int;;

let rec caissier = fun 
 | [] 0 -> []
 | [] a -> raise (reliquat_non_nul a)
 | (h::q) a when h <= a -> h::(caissier (h::q) (a - h))
 | (h::q) a -> caissier q a;;

caissier [200;100;50;20] 480;;

	
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

decomposition 24;;

#open "random";;

let lecompteestbon n l =
	let operation c d =
		let (a,a')=c and (b,b')=d in
		match (a,b) with
		| a,b when b > a && b mod a = 0 ->
				(b-a,"("^b'^"-"^a'^")")::(b/a,"("^b'^"/"^a'^")")::[a+b,"("^a'^"+"^b'^")";a*b,"("^a'^"*"^b'^")"]
		| a,b when b > a -> (b-a,"("^b'^"-"^a'^")")::[a+b,"("^a'^"+"^b'^")";a*b,"("^a'^"*"^b'^")"]
		| a,b when a <> b && a mod b = 0 ->
				(a-b,"("^a'^"-"^b'^")")::(a/b,"("^a'^"/"^b'^")")::[a+b,"("^a'^"+"^b'^")";a*b,"("^a'^"*"^b'^")"]
		| a,b  when a <> b -> (a-b,"("^a'^"-"^b'^")")::[a+b,"("^a'^"+"^b'^")";a*b,"("^a'^"*"^b'^")"]
		| a,b -> (a/b,"("^a'^"/"^b'^")")::[a+b,"("^a'^"+"^b'^")";a*b,"("^a'^"*"^b'^")"]
	in
	let rec elem_liste t hist = function
		| [] -> []
		| h::q when h <> t -> (map (function x -> hist@(x::q)) (operation t h))@(elem_liste t (h::hist) q)
		| h::q -> elem_liste t hist q
	and etape hist = function
		| [] -> []
		| h::q -> (elem_liste h hist q)@(etape (h::hist) q)
	and find_n n = function
		| [] -> ""
		| (a,b)::q when a = n -> b
		| h::q -> find_n n q
	and compte_bon n = function
		| [] -> [-1,"Pas de Solution"]
		| []::q -> compte_bon n q
		| h::q -> let res = find_n n h in if res = "" then compte_bon n ((etape [] h)@q) else [n,res]
								
	in snd(hd(compte_bon n [map (function x -> x, string_of_int x) l]));;

lecompteestbon 155 [2;5;1;10;100;7];;

let auto_lecomptestbon =
	let genererprobleme =
		let n = 100 + random__int 899 in
		let r = (function big -> (1-big)*(1 + random__int 10)+big*25*(1+random__int 4))
		in (n,[r(random__int 2);r(random__int 2);r(random__int 2);r(random__int 2);r(random__int 2);r(random__int 2)])
	in let (a,b) = genererprobleme in a, b, lecompteestbon a b;;
	
let eratosthene n = 
	let rec generer = function
  	| 2 -> [2]
  	| n -> n::generer (n-1)
  and barrer i = function
  	| [] -> []
  	| h::q when h mod i = 0 && h <> i -> barrer i q
  	| h::q -> h::barrer i q
	and erat l = function
  	| 2 -> barrer 2 l
  	| n -> erat (barrer n l) (n-1)
 	in erat (generer n) n ;;

eratosthene 50;;
