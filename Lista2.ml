(* Aleksandra Wolska *)

(*zadanie 2a*)
let rec fib x =
  match x with
    0 -> 0
  | 1 -> 1
  | _ -> fib (x - 2) + fib (x - 1);;

fib 42 = 267914296;;
fib 1 = 1;;
fib 5 = 5;;

(*zadanie 2b    - duzo szybsze*)
let fibTail x =
  let rec fibTailIter (x, prev_0, prev_1) =
    match x with
      0 -> prev_0
    | 1 -> prev_1
    | _ -> fibTailIter((x - 1), prev_1, (prev_0 + prev_1))
  in fibTailIter(x, 0, 1);;

fibTail 42 = 267914296;;
fib 1 = 1;;
fib 5 = 5;;

(*zadanie 3*)
let root3 a =
  let rec root3Tail x = 
    if (abs_float((x *. x *. x -. a) /. abs_float(a)) <= 1e-015)
      then x
    else root3Tail(x +. (a /. (x *. x) -. x) /. 3.)
  in root3Tail(if a <= 1. 
              then a 
              else a /. 3.);;


root3 64. = 4.;;
root3 3. = 1.4422495703074083;;

(*zadanie 4*)

let [_; _; x; _; _] = [-2; -1; 0; 1; 2];;
x = 0;;

let [(_, _);(x, _)] = [(1, 2); (0, 1)];;
x = 0;;

(*zadanie 5*)
let rec initSegment (xs, ys) =
  match (xs, ys) with
   (_, []) -> false
  |  ([], _) -> true
  | (_, _) -> if List.hd xs = List.hd ys then
                initSegment(List.tl xs, List.tl ys)
              else false;;

initSegment([1; 2; 3, 4], [1]) == false;;
initSegment(["h"; "a"; "l"; "o"], []) == false;;
initSegment([], ["h"; "a"; "l"; "o"]) == true;;
initSegment(["h"; "a"], ["h"; "a"; "l"; "o"]) == true;;
initSegment([1; 2], [1; 2; 3]) == true;;

(*zadanie 6a*)
let rec replaceNth (xs, n, x) =
  match (xs, n) with
   ([], _) -> []
   | (head::tail, 0) -> x::tail
   | (head::tail, _) -> head::replaceNth(tail, n-1, x)
  ;;

replaceNth (["o"; "l"; "a"; "m"; "a"; "k"; "o"; "t"; "a"], 1, "s") = ["o"; "s"; "a"; "m"; "a"; "k"; "o"; "t"; "a"];;
replaceNth ([], 0, 0) = [];;
replaceNth ([1; 2; 3; 4; 5; 6], 4, 0) = [1; 2; 3; 4; 0; 6];;

