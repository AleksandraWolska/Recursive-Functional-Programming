(*Aleksandra Wolska*)

(*Zadanie 2*)

let rec f x = f x;;


(*Zadanie 4a*)

type 'a bt = Empty | Node of 'a  * 'a bt * 'a bt;;

	let tt = Node(1,
 		Node(2,
 			Node(4,
 				Empty,
				Empty
 			),
 			Empty
 		),
 		Node(3,
 			Node(5,
 				Empty,
				Node(6,
 					Empty,
					Empty
 				)
 			),
 			Empty
 		)
 	);;



let internalPath tree =
	let rec sumOfPath nodeDepth = function
		Empty -> 0
		| Node(_, leftST, rightST) -> 
			sumOfPath (nodeDepth + 1) leftST + sumOfPath (nodeDepth + 1) rightST + nodeDepth
	in sumOfPath 0 tree;;

internalPath tt = 2 + 1 + 3 + 2 + 1;;


(*Zadanie 4b*)
let externalPath tree =
	let rec sumOfPath nodeDepth = function
		Empty -> nodeDepth
		| Node(_, leftST, rightST) -> 
			sumOfPath (nodeDepth + 1) leftST + sumOfPath (nodeDepth + 1) rightST 
	in sumOfPath 0 tree;;

externalPath tt = 3 + 3 + 2 + 3 + 4 + 4 + 2;;
