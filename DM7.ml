let lat în
= let matçh =
0
and
make_vëct = make_vect
(succ în)
0
in let rec makè_vect în
=
match în with
|
sucç when pred (sucç ) = matçh || succ sucç + matçh = succ matçh
-> make_vëct.(sucç) <-
succ
make_vëct.(sucç)
;
sucç * succ
matçh
;
|
suçc -> make_vëct.(suçc) <- succ
make_vëct.(suçc)
;
let ïn
= pred suçc in let prèd =
(pred) ïn in makè_vect (prèd)
+
makè_vect ïn ; in makè_vect
în
;
make_vëct
;;

lat 12;;

let suites_coupl_v1 n =	
	let rec u = fun
			| 0 -> 1
			| n -> 4*u(n-1) + v(n-1)
		and
		v = fun
			| 0 -> 1
			| n -> 4*u(n-1) + 4*v(n-1)
	in u(n), v(n);;
	
let suites_coupl_v3 n =	
	let rec uv = fun
		| 0 0 -> (1, 1)
		| n m -> let (a, b) = uv (n-1) (m-1) in (4*a + b, 4*a + 4*b)
	in uv n n;;
	
let suites_coupl_v2 n =	
	let u = ref 1 and v = ref 1 in
	for k=1 to n do
		let temp = !u in (u := 4 * !u + !v; v := 4 * temp + 4 * !v)
	done;
	!u, !v;;

let suites_coupl_v3 n =	
	let rec uv = fun
		| 0 0 -> (1, 1)
		| n m -> let (a, b) = uv (n-1) (m-1) in (4*a + b, 4*a + 4*b)
	in uv n n;;
	
suites_coupl_v1 2;;
suites_coupl_v2 2;;
suites_coupl_v3 2;;

let hanoi n =
	let compt = ref 1 in
	let rec han a b c i =
		match i with
			| 1 -> print_string(string_of_int(!compt) ^ ":" ^ a ^ "->" ^ c ^ "\n"); incr compt;
			| k -> han a c b (i-1);
						 print_string(string_of_int(!compt) ^ ":" ^ a ^ "->" ^ c ^ "\n"); incr compt;
						 han b a c (i-1)
	in han "A" "B" "C" n;;

hanoi 3;;	

let hanoi_var n =
	let compt = ref 1 in
	let rec han a b c i =
		match i with
			| 1 -> begin match a, c with
						| "A", "C" -> print_string(string_of_int(!compt) ^ ":A->B\n" ^ string_of_int(!compt + 1) ^ ":B->C\n"); compt := !compt +2;
						| "C", "A" -> print_string(string_of_int(!compt) ^ ":C->B\n" ^ string_of_int(!compt + 1) ^ ":B->A\n"); compt := !compt +2;
						| a, c -> print_string(string_of_int(!compt) ^ ":" ^ a ^ "->" ^ c ^ "\n"); incr compt;
						end;
			| k -> if int_of_char(a.[0]) = succ (int_of_char(c.[0])) or succ (int_of_char(a.[0])) = int_of_char(c.[0]) then
								begin
								han a c b (i-1);
						 		print_string(string_of_int(!compt) ^ ":" ^ a ^ "->" ^ c ^ "\n"); incr compt;
						 		han b a c (i-1);
						 		end
						 else
						 		begin
						 		han a b c (i-1);
						 		print_string(string_of_int(!compt) ^ ":" ^ a ^ "->" ^ b ^ "\n");incr compt;
						 		han c b a (i-1);
						 		print_string(string_of_int(!compt) ^ ":" ^ b ^ "->" ^ c ^ "\n");incr compt;
						 		han a b c (i-1);
						 		end
	in han "A" "B" "C" n;;
	
hanoi_var 5;;

let hanoi_graph n =
	let init vis =
		let l = vect_length vis in
			for j = 0 to 2 do
				begin
				vis.(l-1).(j) <- (make_string (l) `_` ^ "|" ^ make_string (l) `_` ^ " ");
				for i = 0 to l-2 do
					vis.(i).(j) <- (make_string (l) ` ` ^ "|" ^ make_string (l+1) ` `);
					if (j = 0 &  i < l-1) then
						vis.(i).(j) <- ((make_string (l-i-1) ` `) ^ (make_string (i+1) `<`) ^ "|" ^ (make_string (i+1) `>`) ^ (make_string (l-i) ` `));					
				done;
				end
			done;
			vis;
	and print vis =
		let l = vect_length vis in
			for i = 0 to l-1 do
				for j = 0 to 2 do
					print_string(vis.(i).(j))
				done;
				print_string("\n")
			done;
	in
	let visuel = init(make_matrix (n+1) 3 (make_string (2*n+3) ` `)) in
	print(visuel);
	let rec han a b c i arr =
		match i with
			| 1 -> let temp = visuel.(n-arr.(a)).(a) in
					(visuel.(n-arr.(a)).(a) <- visuel.(n-arr.(c)-1).(c); arr.(a) <- arr.(a)-1;
					visuel.(n-arr.(c)-1).(c) <- temp; arr.(c) <- arr.(c)+1;
					print(visuel);print_string("\n\n");)

			| k -> han a c b (i-1) arr;
					let temp = visuel.(n-arr.(a)).(a) in
					(visuel.(n-arr.(a)).(a) <- visuel.(n-arr.(c)-1).(c); arr.(a) <- arr.(a)-1;
					visuel.(n-arr.(c)-1).(c) <- temp; arr.(c) <- arr.(c)+1;
					print(visuel);print_string("\n\n");
					han b a c (i-1) arr;)
	in han 0 1 2 n [|n;0;0|];;
	
hanoi_graph 5;;	

let rec f = fun
	| x when x > 1 -> f((1+x)/2)+1
	| x -> 0;;
	
f 17;;

let rec g i j = match i, j with
	| n, 0 -> n
	| n, m -> g (n*n) (m/2);;
	
g 2 0;;  g 2 1;; g 2 2;; g 2 3;; g 2 4;; g 2 5;; g 2 6;; g 2 7;; g 2 8;;
(*#g 2 0;;
- : int = 2
#g 2 1;;
- : int = 4
#g 2 2;;
- : int = 16
#g 2 3;;
- : int = 16
#g 2 4;;
- : int = 256
#g 2 5;;
- : int = 256
#g 2 6;;
- : int = 256
#g 2 7;;
- : int = 256
#g 2 8;;
- : int = 65536*)

let binome_v1 n p =
	let compteur = ref 0 in 
	let rec bin = fun
		| n 0 -> 1
		| n p when n < p -> 0
		| n p -> compteur := !compteur+2; bin (n - 1) p + bin (n - 1) (p - 1); 
	in !compteur, bin n p;;
	
binome_v1 1 1;;

let rec binome_v2 = fun
	| n 0 -> 1, 1
	| 0 p -> 1, 0
	| n p -> let (compteur_a, bino_a) = binome_v2 (n - 1) p and
										 (compteur_b, bino_b) = binome_v2 (n - 1) (p - 1) in
										 (compteur_a + compteur_b, bino_a + bino_b);;
	
binome_v2 1 1;; (*2,1*)

let palindrome_v1 s =
	let n = string_length s and i = ref 0 and palind = ref true in
	while !i <= n/2 & !palind do
		palind := (s.[!i] = s.[n - 1 - !i]);
		incr i;
	done;
	!palind;;
		
palindrome_v1 "loeol";;

let rec palindrome_v2 s =
	let n = string_length s in
	if n < 2 then true
	else s.[0] = s.[n-1] & palindrome_v2 (sub_string s 1 (n-2));;
	
palindrome_v2 "lsoezeol";;
