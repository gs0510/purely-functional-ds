exception Empty

exception Subscript

module type STACK = sig
  type 'a stack

  val empty : 'a stack

  val isEmpty : 'a stack -> bool

  val cons : 'a -> 'a stack -> 'a stack

  val head : 'a stack -> 'a

  val tail : 'a stack -> 'a stack

  val ( ++ ) : 'a stack -> 'a stack -> 'a stack
end

module ListStack : STACK = struct
  type 'a stack = 'a list

  let empty = []

  let isEmpty s = s = []

  let cons x s = x :: s

  let head = function [] -> raise Empty | h :: _ -> h

  let tail = function [] -> raise Empty | _ :: t -> t

  let rec ( ++ ) xs ys = match xs with [] -> ys | xh :: xt -> xh :: (xt ++ ys)
end

module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil

  let isEmpty s = Nil = s

  let cons x s = Cons (x, s)

  let head = function Nil -> raise Empty | Cons (x, _) -> x

  let tail = function Nil -> raise Empty | Cons (_, s) -> s

  let rec ( ++ ) xs ys =
    if isEmpty xs then ys else cons (head xs) (tail xs ++ ys)
end

let rec update lst i y =
  match (lst, y) with
  | [], _ -> raise Subscript
  | _ :: xs, 0 -> y :: xs
  | x :: xs, _ -> x :: update xs (i - 1) y

let rec suffixes xs =
  match xs with [] -> [ [] ] | hd :: tl -> hd :: suffixes tl

module type SET = sig
  type elem

  type set

  val empty : set

  val insert : elem -> set -> set

  val member : elem -> set -> bool
end

module type ORDERED = sig
  type t

  val eq : t -> t -> bool

  val lt : t -> t -> bool

  val leq : t -> t -> bool
end

module UnbalancedSet (Element : ORDERED) : SET with type elem = Element.t =
struct
  type elem = Element.t

  type tree = E | T of tree * elem * tree

  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T (a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  (* exercise 2.2 *)
  let member1 x = function
    | E -> false
    | T (_, y, _) as set ->
        let rec aux x z = function
          | E -> x == z
          | T (a, y, b) -> if Element.lt x y then aux x z a else aux x y b
        in
        aux x y set

  let rec insert x = function
    | E -> T (E, x, E)
    | T (a, y, b) as s ->
        if Element.lt x y then T (insert x a, y, b)
        else if Element.lt y x then T (a, y, insert x b)
        else s

  (* exercise 2.3 & 2.4 *)
  let rec insert2 x s =
    if member1 x s then s
    else
      match s with
      | E -> T (E, x, E)
      | T (a, y, b) when Element.lt x y -> T (insert2 x a, y, b)
      | T (a, y, b) when Element.lt y x -> T (a, y, insert2 x b)
      | _ -> s

  (* exercise 2.5*)
  let rec complete x num =
    match num with
    | 0 -> E
    | num ->
        let subtree = complete x (num - 1) in
        T (subtree, x, subtree)

  let rec create x n =
    match n with
    | 0 -> E
    | n when (n - 1) mod 2 = 0 ->
        let subtree = create x ((n - 1) / 2) in
        T (subtree, x, subtree)
    | _ ->
        let half = (n - 1) / 2 in
        let l = create x half in
        let r = create x (half + 1) in
        T (l, x, r)
end
