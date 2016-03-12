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

let rec fibonacci n =
	let h = new 51 in
	add h 0 0;
	add h 1 1;
	let rec fibo n =
  	begin
  	try
      find h n
    with
			|Not_found -> let r = fibo (n - 1) + fibo (n - 2) in add h n r; r;
		end
	in fibo n;;

fibonacci 54;;
