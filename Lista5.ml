(*Aleksandra Wolska*)

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function
 [] -> LNil
 | x :: xs -> LCons(x, lazy (toLazyList xs));;

let rec ltake = function
 (0, _) -> []
 | (_, LNil) -> []
 | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);;


(* zadanie 1*)

let rec lrepeat k lxs =
  let rec repeatElem elem count xs =
    if count = 0 
      then xs
    else repeatElem elem (count - 1) (LCons(elem, lazy xs))
  in match lxs with
       LNil -> LNil
     | LCons(el, lazy tailFun) -> repeatElem el k (lrepeat k tailFun);;

ltake (12, (lrepeat 3 (toLazyList ['a'; 'b'; 'c'; 'd']))) = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'; 'd'; 'd'; 'd'];;
ltake (15, (lrepeat 3 (toLazyList (ltake (15, lfrom 1))))) = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5];;
ltake (15, (lrepeat 3 (toLazyList []))) = [];;

(* zadanie 2 *)

let lfib =
    let rec fib x y =
      LCons(x, lazy (fib y (x + y)))
    in fib 1 1;;

ltake (6, lfib) = [1; 1; 2; 3; 5; 8];;


(* zadanie 3 *)

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

let rec lTree n = LNode(n, (fun() -> lTree(2*n)), (fun() -> lTree(2*n + 1)));;

let lBreadth tree =
  let rec nodeQueueHelper queue =
    match queue with
      [] -> LNil
    | LEmpty :: tl -> nodeQueueHelper tl
    | LNode(value, left, right)::tl -> LCons(value, lazy (nodeQueueHelper (tl @ [left(); right()])))
  in nodeQueueHelper [tree];;

ltake (20, lBreadth (lTree 1)) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;
ltake (20, lBreadth (LEmpty)) = [];;

