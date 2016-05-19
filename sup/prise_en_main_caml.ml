let ppdiv1 n = 
		let i = ref 2 in
      while (n mod !i != 0)
      		do i := succ !i
      done;
      !i;;

let ppdiv2 n = 
		let i = ref 2 in
      while (n mod !i != 0 & !i * !i < n)
      		do i := succ !i
      done;
      if !i * !i < n then !i else n;;

let ppdv_imbriqué n = 
		let i = ref 2 and p = ref n and div = ref [] in
      while (!p > 1) do
      		let compteur = ref 0 in 
      		while (!p mod !i = 0) do
      				(p := (!p / !i); compteur := succ !compteur) done;
      		if !compteur != 0 then div := !div @ [(!i,!compteur)];
      		i := succ !i;
      done;
      !div;;

let indice n i =
		let p = ref n and compteur = ref 0 in
		while (!p mod i = 0) do
      		(p := (!p / i); compteur := succ !compteur) done;
      !compteur;;
      
let puiss a n = int_of_float(float_of_int(a)**float_of_int(n));;

let ppdv_découpé n = 
		let i = ref 2 and p = ref n and div = ref [] in
      while (!p > 1) do
				if (!p mod !i = 0) then
						(let ind = indice !p !i in
						(div := !div @ [(!i,ind)]; p := !p / (puiss !i ind));)
				else i := succ !i;
      done;
      !div;;

ppdv_découpé 174;;

let cherche_mini l i =
		let n = (vect_length l) and mini = ref l.(i) and indice_mini = ref i in 
		for j = i to n-1 do
			if l.(j) <= !mini then (mini := l.(j); indice_mini := j;) 
		done;
		(!mini,!indice_mini);;
				
cherche_mini [|4;8;5;6;2;7;7;5;9;5;4|] 1;;

let tri_selection l = let n = (vect_length l) in
		for k = 0 to n-2 do
			let (aux, j) = cherche_mini l k in
			(l.(j) <- l.(k); l.(k) <- aux; print_int(aux);)
		done;
		l;;
			
tri_selection [|4;5;4;6;2;7;9;4|];;	
			
