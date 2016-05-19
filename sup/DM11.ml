let recherche_bin a ob =
	let n = vect_length a in
	let rec dicho g d =
		if g = d then
			if ob = a.(g) then g else (-1)
		else
			let m = (g+d)/2 in
			if ob <= a.(m) then dicho g m else dicho (m+1) d
	in dicho 0 (n-1);;
	
recherche_bin [|1;5;7;9;15;19;25|] 15;;
recherche_bin [|1;5;7;9;15;19;25|] 17;;

exception Pair_Found of (int*int);;

let paire_dicho a s =
	let n = vect_length a in
	try
		for i=0 to n-1 do
			let b = a.(i) in let j = recherche_bin a (s-b) in
			if j <> (-1) && i <> j  then
				raise (Pair_Found (b,a.(j)))
		done;
		(-1,-1)
	with Pair_Found (b,c) -> (b,c);;

let paire_dicho_2 a s =
	let n = vect_length a and i = ref 0 and j = ref (-1) in
	while j := recherche_bin a (s-a.(!i)); !j = (-1) && !i < n-1 do	
		incr i;
	done;
	if !i < n-1 && !i <> !j then (a.(!i),a.(!j)) else (-1,-1);;

let paire_dicho_lin a s =
	let n = vect_length a in
	let i = ref 0 and j = ref (n-1) and sum = ref (a.(0) + a.(n-1)) in
	while !i < !j && !sum <> s do
		sum := a.(!i) + a.(!j);
		if !sum < s then incr i else if !sum > s then decr j;
	done;
	if !sum = s then (a.(!i),a.(!j)) else (-1,-1);;

paire_dicho [|1;5;7;9;15;19;25|] 2;;
paire_dicho [|1;5;7;9;15;19;25|] 22;;
paire_dicho [|1;5;7;9;15;19;25|] 21;;
paire_dicho_2 [|1;5;7;9;15;19;25|] 2;;
paire_dicho_2 [|1;5;7;9;15;19;25|] 22;;
paire_dicho_2 [|1;5;7;9;15;19;25|] 21;;
paire_dicho_lin [|1;5;7;9;15;19;25|] 2;;
paire_dicho_lin [|1;5;7;9;15;19;25|] 22;;
paire_dicho_lin [|1;5;7;9;15;19;25|] 21;;

let rec fusion_sans_rep = fun
	| [] l -> l
	| l [] -> l
	| (h::q) (h'::q') when h = h' -> h::fusion_sans_rep q q'
	| (h::q) (h'::q' as l) when h <= h' -> h::fusion_sans_rep q l
	| l (h::q) -> h::fusion_sans_rep l q;;
	
fusion_sans_rep [1;2;3;5;9;12;13;15] [1;5;6;7;8;10;11;13;16];;
