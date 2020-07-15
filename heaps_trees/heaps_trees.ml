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

module BinomialHeap (Element : ORDERED) : HEAP with module Elem = Element =
struct
  module Elem = Element

  type tree = Node of int * Elem.t * tree list

  type heap = tree list

  let empty = []

  let isEmpty s = s = []

  let rank (Node (r, _, _)) = r

  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [ t ]
    | t' :: ts' as ts ->
        if rank t < rank t' then t :: ts else ins_tree (link t t') ts'

  let insert x ts = ins_tree (Node (0, x, [])) ts

  let rec merge ts1 ts2 =
    match (ts1, ts2) with
    | [], ts2 -> ts2
    | ts1, [] -> ts1
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree t =
    match t with
    | [] -> raise Empty
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then (t, ts) else (t', t :: ts')

  let findMin ts =
    let t, _ = remove_min_tree ts in
    root t

  let deleteMin ts =
    let Node (_, _, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2

  (* exercise 3.5 *)
  let findMin' t =
    let rec find min = function
      | [] -> min
      | t :: ts ->
          let root = root t in
          if Elem.leq root min then find root ts else find min ts
    in
    match t with [] -> raise Empty | t :: ts -> find (root t) ts
end

module type SET = sig
  type elem

  type set

  val empty : set

  val insert : elem -> set -> set

  val member : elem -> set -> bool
end

module RedBlackTree (Element : ORDERED) : SET with type elem = Element.t =
struct
  type elem = Element.t

  type color = R | B

  type tree = E | T of color * tree * elem * tree

  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T (_, a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | a, b, c, d -> T (a, b, c, d)

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
          if Element.lt x y then balance (color, ins a, y, b)
          else if Element.lt y x then balance (color, a, y, ins b)
          else s
    in
    match ins s with T (_, a, y, b) -> T (B, a, y, b) | _ -> raise Empty

  (* exercise 3.9 *)
  let fromOrdList l =
    let rec aux ret = function
      | [] -> ret
      | [ x ] -> insert x ret
      | x1 :: (x2 :: tl2 as tl) ->
          if x1 == x2 then aux ret tl else aux (insert x2 (insert x1 ret)) tl2
    in
    aux empty l
end
