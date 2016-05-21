(*superposition des noms de variables*)
let ™ = 1 in T;;
let š = 2 in s;;
let Š = 3 in S;;
let œ = 4 in o;;
let Œ = 5 in O;;
let ƒ = 6 in f;;



(*Underscore : Mot clé normalement réservé*)

(*Accepté, mais inutile, _ inaccessible*)
let _ = 0;;

(*Considéré comme une fonction et non un int, _ est un argument inaccessible*)
let c_ = 1;;
c 0;;
c (c (c (c c)));; (*pas dû à l'underscore, mais rigolo*)



(*Autre syntaxe pour les floats*)
let a = 8• in a +• 9•1;;
let b = 2… in b /… 3…4;;



(*Instructions à des endroits rigolos*)

let a = ref 0 in if incr a; !a = 1 then print_string("oui");;

(*equivalent à un do while*)
let a = ref 0 in while incr a; !a < 10 do () done; !a;; 

(*superposition de j*)
let j = ref 9 in for j = 0 to print_string("deb");incr j; !j do print_int(j) done; !j;;
