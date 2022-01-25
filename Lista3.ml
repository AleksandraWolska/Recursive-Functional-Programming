(* Aleksandra Wolska*)


(* zadanie 2a *)
let curry3lukier f x y z = f (x, y, z);;
let curry3bezlukru = function f -> function x -> function y -> function z -> f (x, y, z);;

(* zadanie 2b*)
let uncurry3lukier f (x, y, z) = f x y z;;
let uncurry3lezlukru = function f -> function (x, y, z) -> f x y z;;

(* zadanie 3*)
let sumProd xs = 
  List.fold_left(
    fun (suma, iloczyn) head -> (suma + head , iloczyn * head))        (*odpowiednik (+) lub *  *)
    (0,1) xs;;

sumProd [0; 1; 2; 3; 4] = (10, 0);;
sumProd [2; 3; 4; 5] = (14, 120);;


(* zadanie 5a *)
let insertionsort isOrdered xs =
  let rec insert x xsNew =
    match xsNew with
      [] -> [x]
      | head::tail as xsNew -> if isOrdered head x 
                                then head :: insert x tail
                                else x::xsNew
  in List.fold_left (fun acc xNew -> insert xNew acc) [] xs;;

let isLess tuple1 tuple2 = 
  fst tuple1 <= fst tuple2;;

  let isBigger tuple1 tuple2 = 
    fst tuple1 >= fst tuple2;;


    insertionsort isLess [(5, 3); (4, 1); (2, 5); (4, 2)] = [(2, 5); (4, 1); (4, 2); (5, 3)];;
    insertionsort isBigger [(5, 3); (4, 1); (2, 5); (4, 2)] = [(5, 3); (4, 1); (4, 2); (2, 5)];;
    insertionsort isLess [(5, 1); (5, 2); (5, 3); (5, 4)] = [(5, 1); (5, 2); (5, 3); (5, 4)] ;;


(* zadanie 5b *)
(* 
 let isLess tuple1 tuple2 = 
  fst tuple1 <= fst tuple2;;

let rec mergesort isOrdered xs =
  let xMiddle = List.length xs / 2 in
    if xMiddle == 0 
      then xs
    else let rec merge = function
      (head1 :: tail1 as xs1), (head2 :: tail2 as xs2) ->
          if isOrdered head1 head2 
            then head1 :: merge(tail1, xs2)
          else head2 :: merge(xs1, tail2)
      | [], xs2 -> xs2
      | xs1, [] -> xs1

	    
      let (xsleft, right) = split [] xs xMiddle                                 
      	 in merge(mergesort isOrdered xsleft, mergesort isOrdered xsRight);;


        mergesort isLess [(5, 3); (4, 1); (2, 5); (4, 2)] = [(2, 5); (4, 1); (4, 2); (5, 3)];;
        mergesort isLess [(5, 1); (5, 2); (5, 3); (5, 4)] = [(5, 1); (5, 2); (5, 3); (5, 4)] ;;  *)