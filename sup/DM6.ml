exception Premier;;
exception Composé of int;;

let ppdiv_except n =
	for i = 2 to n do
		if n mod i = 0 then raise (Composé i)
		else if i * i > n then raise Premier;
	done;;
	
let ppdiv n =
	try
		ppdiv_except n;
		-1
	with
		| Composé i -> i;
		| Premier -> n;;
		
ppdiv 5;;
ppdiv 77;;

#open "hashtbl";;

let fibonacci n =
	let h = new (n+1) in
	add h 0 0;
	add h 1 1;
	let rec fibo n =
		try
			find h n
    		with
			| Not_found -> let r = fibo (n-1) + fibo (n-2) in add h n r; r;
	in fibo n;;
	
let fibonacci_v22 n =
	let h = make_vect (n+1) (-1) in
	h.(0) <- 0;
	h.(1) <- 1;
	let rec fibo_v22 n =
  		if h.(n) < 0 then (let r = fibo_v22 (n-1) + fibo_v22 (n-2) in h.(n) <- r; r;)
  		else h.(n);
	in fibo_v22 n;;


fibonacci 147;;
fibonacci_v22 147;;
