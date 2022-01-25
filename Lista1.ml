(* Aleksandra Wolska *)

(*Zadanie 1*)
let rec flatten1 xss = 
  if xss <> [] 
  then List.hd xss @ flatten1 (List.tl xss)
  else [];;

flatten1 [];;
flatten1 [[1; 2; 3]; [4; 5; 6]];;
flatten1 [["Czy"; "to"];["dziala"]];;


(*zadanie 2*)
let rec count (x,xs) =
  if xs <> [] 
  then (
    let numOfReps = 
    if List.hd xs = x 
    then 1 
    else 0 
    in numOfReps + count (x, List.tl xs)
  )else 0;;
  
count (0, []);;    
count (1, [2; 5; 1; 8; 1; 0; 1]);;
count ("a", ["p"; "a"; "r"; "a"; "d"; "y"; "g"; "m"; "a"; "t"; "y"]);;


(*zadanie 3*)
let rec replicate (x, i) =
  if i > 0 
  then [x] @ replicate(x, i-1)
  else [];;

replicate ("nic", 0);;
replicate ("halo", 3);;
replicate (1, 6);;


(*zadanie 4*)
let rec sqrList xs  =
  if xs <> [] 
  then [(List.hd xs)*(List.hd xs)] @ sqrList (List.tl xs)
  else [];;

sqrList []
sqrList [1; 2; 3; 4; 5];;


(*zadanie 5*)
let palindrome xs  =
  xs = List.rev xs;;

palindrome [];;
palindrome [1];;
palindrome ["k"; "a"; "j"; "a"; "k"]
palindrome [1;2;3];;


(*zadanie 6*)
let rec listLength xs =
  if xs <> [] then (
    let len = 
      if xs <> [] 
      then 1 
      else 0 
      in
    len + listLength (List.tl xs))
  else 0;;

listLength [];;
listLength [1; 2; 3; 4; 5; 6];;
listLength ["k"; "o"; "t"];;



