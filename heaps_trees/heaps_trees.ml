module type ORDERED = sig
  type t

  val eq : t -> t -> bool

  val lt : t -> t -> bool

  val leq : t -> t -> bool
end

module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap

  val isEmpty : heap -> bool

  val insert : Elem.t -> heap -> heap

  val merge : heap -> heap -> heap

  val findMin : heap -> Elem.t

  val deleteMin : heap -> heap
end

exception Empty

module LeftistHeap (Element : ORDERED) : HEAP with module Elem = Element =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  let rank = function E -> 0 | T (r, _, _, _) -> r

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b) else T (rank a + 1, x, b, a)

  let rec merge h1 h2 =
    match (h1, h2) with
    | _, E -> h1
    | E, _ -> h2
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if Elem.leq x y then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

  let empty = E

  let isEmpty h = h = E

  let insert x h = merge (T (1, x, E, E)) h

  let findMin = function E -> raise Empty | T (_, x, _, _) -> x

  let deleteMin = function E -> raise Empty | T (_, _, a, b) -> merge a b

  (* exercise 3.2 *)
  let rec insert' x = function
    | E -> T (1, x, E, E)
    | T (_, y, a, b) as h ->
        if Elem.leq x y then makeT x a h else makeT y a (insert' x b)

  (* (\* exercise 3.3 *)
  let fromList l =
    let rec aux ret = function
      | [] -> ret
      | hd :: tl -> aux (insert hd ret) tl
    in
    aux empty l
end
