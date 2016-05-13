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
	let elem_liste hist f_partial_step =
		let t::partial_step = f_partial_step in
		let rec e_liste hist = function
			| [] -> []
			| h::q -> (map (function x -> hist@(x::q)) (operation t h))@(e_liste (h::hist) q)
		in e_liste hist partial_step
	in 
	let rec etape hist = function
		| [] -> []
		| h::q as l -> (elem_liste hist l)@(etape (h::hist) q)
	and find_n n = function
		| [] -> ""
		| (a,b)::q when a = n -> b
		| h::q -> find_n n q
	and compte_bon n = function
		| [] -> [-1,"Pas de Solution"]
		| []::q -> compte_bon n q
		| h::q -> let res = find_n n h in if res = "" then compte_bon n ((etape [] h)@q) else [n,res]
								
	in snd(hd(compte_bon n [map (function x -> x, string_of_int x) l]));;

lecompteestbon 10 [2;2;2;2];;

lecompteestbon 517 [2;5;7;10;14;17];;
