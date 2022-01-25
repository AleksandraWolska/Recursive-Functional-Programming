(* Aleksandra Wolska *)

(* zadanie 1 *)
module type QUEUE_FUN =
sig
 type 'a t
 exception Empty of string
 val empty: unit -> 'a t
 val enqueue: 'a * 'a t -> 'a t
 val dequeue: 'a t -> 'a t
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
end;;


(* a - kolejka na liscie*)
module QueueList : QUEUE_FUN =

  struct
    type 'a t = 'a list 
    exception Empty of string

    let enqueue(element, queue) = queue @ [element]

    let dequeue = function
      | [] -> []
      | head::tail -> tail

    let first = function
      | [] -> raise (Empty "module QueueList: first")
      | head::tail -> head

    let empty() = []

    let isEmpty queue =
      queue = []

  end;;

let myQueue = QueueList.empty();;
QueueList.first(QueueList.enqueue(2, myQueue)) = 2;;
QueueList.dequeue(QueueList.enqueue(3, myQueue));;
QueueList.isEmpty(QueueList.enqueue(1, myQueue)) = false;;
QueueList.isEmpty myQueue = true;;



(* b - kolejka na parze list*)
module QueueListPair : QUEUE_FUN =

  struct

    type 'a t = 'a list * 'a list
    exception Empty of string

    let normalize queue =
          match queue with
            ([], list) -> (List.rev list, [])
          | _ -> queue

    let first queue =
      match fst queue with
        [] -> raise (Empty "module QUEUE_LIST: first")
      | h::t -> h          

    let enqueue(element, queue) =
      normalize (fst queue, element::(snd queue))

    let dequeue = function
      | ([], _) -> ([], [])
      | (head::tail, reversedQueue) -> normalize (tail, reversedQueue)


    let empty() = ([], [])

    let isEmpty queue =
      fst queue = []

  end;;

  let myQueueListPair = QueueList.empty();;
  QueueList.first(QueueList.enqueue(2, myQueueListPair)) = 2;;
  QueueList.dequeue(QueueList.enqueue(3, myQueueListPair));;
  QueueList.isEmpty(QueueList.enqueue(1, myQueueListPair)) = false;;
  QueueList.isEmpty myQueueListPair = true;;

