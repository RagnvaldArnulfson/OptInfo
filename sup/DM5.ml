let eratosthene n =
	let a = make_vect (n + 1) true and prem = ref [] and compteur = ref 0 in
	for i = 2 to n do
		begin
		if a.(i) then
			begin
			incr compteur;
			prem := !prem @ [i];
			let j = ref 2 in
			while i * !j <= n do
				(a.(i * !j) <- false; incr j)
			done;
			end
		end
	done;
	!compteur, !prem;;

let horner a x =
	let n = (vect_length a - 1) and result = ref 0 in
	for i = 0 to n do
		result := !result * x + a.(n - i);
	done;
	!result;;

let add_int a b =
	let ap = ref (copy_vect a)   and bq = ref (copy_vect b)   and
	    p  = (vect_length a) - 1 and q  = (vect_length b) - 1 in
	let n = max p q   and   resu = ref [||]   and   r = ref 0 in
	for i = p + 1 to n do ap := concat_vect !ap [|0|] done;
	for i = q + 1 to n do bq := concat_vect !bq [|0|] done;
	for i = 0     to n do
	begin
		let s = !ap.(i) + !bq.(i) + !r in
		if s < 10 then (resu := concat_vect !resu [|s|];    r := 0)
		else 	       (resu := concat_vect !resu [|s-10|]; r := 1)
	end
	done;
	if !r = 1 then concat_vect !resu [|1|] else !resu;;
	
(*let decalage i b =
	let resu = ref (copy_vect b) in
	for k = 1 to i do resu := concat_vect [|0|] !resu done;
	!resu;;

let produit_court_long c b =
	let resu = ref [||] and r = ref 0 and p = (vect_length b) - 1 in
	if 	c = 0 then [||]
	else if c = 1 then b
	else
		begin
		for i = 0 to p do
			let s = b.(i) * c + !r in
				(resu := concat_vect !resu [|s mod 10|]; r := s / 10)
		done;
		if !r > 0 then resu := concat_vect !resu [|!r|];
		!resu;
		end
	;;

let mult_int a b =
	let ap = ref (copy_vect a)   and bq = ref (copy_vect b)   and
	    p  = (vect_length a) - 1 and q  = (vect_length b) - 1 in
	let n  = max p q	     and resu = ref [||] 	  in
	for i = p + 1 to n do ap := concat_vect !ap [|0|] done;
	for i = q + 1 to n do bq := concat_vect !bq [|0|] done;
	for i = 0 to n do
		let s = produit_court_long !ap.(i) (decalage i !bq) in resu := add_int !resu s
	done;
	!resu;;*)(*methode DM3*)

let mult_poly a b =
	let n = vect_length a and m = vect_length b in
		let result = ref (make_vect (n+m) 0) in
		for i = 0 to n-1 do
			for j = 0 to m-1 do
				!result.(i+j) <- !result.(i+j) + a.(i) * b.(j)
			done;
		done;
	!result;;

let mult_int a b =
	let n = vect_length a and m = vect_length b in
	let result = ref (make_vect (n+m) 0) in
	for i = 0 to n-1 do
		for j = 0 to m-1 do
			let s = !result.(i+j) + a.(i) * b.(j) in
			(!result.(i+j) <- s mod 10; !result.(i+j+1) <- !result.(i+j+1) + (s / 10))
		done;
	done;
	!result;;

let vect_of_string a =
	let n = (string_length a) - 1 and v = ref [||] in
	for i = 0 to n do
		v := concat_vect !v [|(int_of_char a.[n-i]) - 48|];
	done;
	!v;;

let string_of_vect a =
	let n = (vect_length a) - 1 and s = ref "" in
	for i = 0 to n do
		s := !s ^ (string_of_int a.(n-i));
	done;
	!s;;

let add_string a b =
	string_of_vect (add_int (vect_of_string a) (vect_of_string b));;

let mult_string a b =
	string_of_vect (mult_int (vect_of_string a) (vect_of_string b));;

let caissier c s =
	let reste = ref s and n = vect_length c and output = ref [||] in
	for i=0 to n-1 do
		begin
		let compteur = ref 0 in
		while !reste >= c.(i) do
			begin
			reste := !reste - c.(i);
			incr compteur;
			end
		done;
		output := concat_vect !output [|!compteur|];
		end
	done;
	!output, !reste;;

#open "float";;

let heron a eps =
	let result = ref a in
	while !result - ((!result + (a / !result)) / 2.) > (eps / 2.) do
		result := (!result + (a / !result)) / 2.;
	done;
	!result;;
	
let spy =
	let a = ref 1. and b = ref 1. in
	while (!a + !b) - !a - !b = 0. do
		a := !a * 2.
	done;
	while (!a + !b) - !a - !b <> 0. do
		b := !b + 1.
	done;
	!a,!b;; (* a=2^53, b=2.0*)
