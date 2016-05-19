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

let recherche_bin_eg a ob =
	let n = vect_length a in
	let rec dicho g d =
		if ob = a.(g) then g 
		else if g = d then (-1)
		else
			let m = (g+d)/2 in
			if ob <= a.(m) then dicho g m else dicho (m+1) d
	in dicho 0 (n-1);;
	
recherche_bin_eg [|1;5;7;9;15;19;25|] 15;;
recherche_bin_eg [|1;5;7;9;15;19;25|] 17;;

let rec fusion_sans_rep = fun
	| [] l -> l
	| l [] -> l
	| (h::q) (h'::q') when h = h' -> h::fusion_sans_rep q q'
	| (h::q) (h'::q' as l) when h <= h' -> h::fusion_sans_rep q l
	| l (h::q) -> h::fusion_sans_rep l q;;
	
fusion_sans_rep [1;2;3;5;9;12;13;15] [1;5;6;7;8;10;11;13;16];;

let rec cherche_seq = function
	| [] -> [],[]
	| h::h'::q when h <= h' -> let (s,r) = cherche_seq (h'::q) in (h::s, r) 
	| h::q -> [h],q;;

cherche_seq [5;7;8;9;12];;
cherche_seq [1;2;3;2;1];;

let rec fusion = fun
	| [] l -> l
	| l [] -> l
	| (h::q) (h'::q' as l) when h <= h' -> h::fusion q l
	| l (h::q) -> h::fusion l q;;

let rec tri_ajout = function
	| [] -> []
	| l -> let (s,r) = cherche_seq l in fusion s (tri_ajout r);;

tri_ajout [7;5;9;6;4;1;2;8;3];;

let rec fusion_seq = function
	| [] -> []
	| l -> let (s,r) = cherche_seq l in let (s',r') = cherche_seq r in
				 fusion (fusion s s') (fusion_seq r');;

fusion_seq [7;5;9;6;4;1;2;8;3];;

let rang a b =
	let n = vect_length a in
	let rec dicho g d =
		if g = d then (if g = n-1 && b > a.(g) then g+1 else g)
		else
			let m = (g+d)/2 in
			if b <= a.(m) then dicho g m else dicho (m+1) d
	in dicho 0 (n-1);;

rang [|2;4;6;8;10;25;31|] 25;;
rang [|2;4;6;8;10;25;31|] 31;;
rang [|2;4;6;8;10;25;31|] 26;;
rang [|2;4;6;8;10;25;31|] 32;;

let fusion_g_p g p =
	let m = vect_length g and n = vect_length p in
	let a = make_vect (n+m) 0 and i = ref 0 and j = ref 0 in
	while !i + !j < m + n do
		if !j < n then
			(let hp = p.(!j) in
			let v = rang g hp in blit_vect g (!i) a (!i + !j) (v - !i);
			i := v;a.(!i + !j) <- hp; incr j;print_int(!j);)
		else (blit_vect g (!i) a (!i + !j) (m - !i); i := m)
	done;
	a;;
	
fusion_g_p [|6;7;8;10;15;19;25;75;96;120|] [|3;17;25;130|];;
fusion_g_p [|1;2;3;4;5;6;7;8;10|] [|3;7|];;
