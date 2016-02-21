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

eratosthene 25;;

let horner a x =
	let n = (vect_length a - 1) and result = ref 0 in
		for i = 0 to n do
			result := !result * x + a.(n - i);
		done;
	!result;;

horner [|1;1;2|] 2;;

let add_int a b =
	let ap = ref (copy_vect a)   and bq = ref (copy_vect b)   and
	    p  = (vect_length a) - 1 and q  = (vect_length b) - 1 in
	let n  = max p q and resu = ref [||] 	and 	r = ref 0 in
	for i = p + 1 to n do ap := concat_vect !ap [|0|] done;
	for i = q + 1 to n do bq := concat_vect !bq [|0|] done;
	for i = 0     to n do
		begin
			let s = !ap.(i) + !bq.(i) + !r in
				if s < 10 then (resu := concat_vect !resu [|s|];    r := 0)
				else (resu := concat_vect !resu [|s-10|]; r := 1)
		end
	done;
	if !r = 1 then concat_vect !resu [|1|] else !resu;;

add_int [|7;5;4;8;2|] [|1;2;3;4;5;6;7|];;

let vect_of_string a =
	let n = (string_length a) - 1 and v = ref [||] in
	for i = 0 to n do
		v := concat_vect !v [|a.[n-i]|];
	done;
	!v;;

vect_of_string "654654";;

let string_of_vect a =
	let n = (vect_length a) - 1 and s = ref "" in
	for i = 0 to n do
		s := !s ^ (string_of_int a.(n-i));
	done;
	!s;;

string_of_vect [|1;1;2|];;	

let caissier a s =
	let reste = ref s and n = vect_length a and output = ref [||] and i = ref 0 in
	for i=0 to n-1 do
	begin
		let compteur = ref 0 in
		while !reste >= a.(i) do
		begin
			reste := !reste - a.(i);
			incr compteur;
		end
		done;
		output := concat_vect !output [|!compteur|];
	end
	done;
	!output, !reste;;
	
caissier [|750;45;15;5|] 1578;;

(*#open "float";;*)

let heron a eps =
	let result = ref a in
	while !result - ((!result + (a / !result)) / 2.) > (eps / 2.) do
		result := (!result + (a / !result)) / 2.;
	done;
	!result;;
	
heron 25. 0.01;;

let spy =
	let a = ref 1. and b = ref 1. in
	while (!a + !b) - !a - !b = 0. do
		a := !a * 2.
	done;
	while (!a + !b) - !a - !b <> 0. do
		b := !b + 1.
	done;
	!a,!b;; (* a=2^53, b=2.0*)
