exception Empty

module type QUEUE = sig
  type 'a queue

  val empty : 'a queue

  val isEmpty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue

  val head : 'a queue -> 'a

  val tail : 'a queue -> 'a queue
end

module BatchedQueue : QUEUE = struct
  type 'a queue = 'a list * 'a list

  let empty = ([], [])

  let isEmpty (f, _) = f = []

  let checkf ((f, r) as q) = if f = [] then (List.rev r, []) else q

  let snoc (f, r) x = checkf (f, x :: r)

  let head = function [], _ -> raise Empty | x :: _, _ -> x

  let tail = function [], _ -> raise Empty | _ :: f, r -> checkf (f, r)
end

module type DEQUE = sig
  type 'a queue

  val empty : 'a queue

  val isEmpty : 'a queue -> bool

  val cons : 'a -> 'a queue -> 'a queue

  val head : 'a queue -> 'a

  val tail : 'a queue -> 'a queue

  val snoc : 'a queue -> 'a -> 'a queue

  val last : 'a queue -> 'a

  val init : 'a queue -> 'a queue
end

module Deque : DEQUE = struct
  type 'a queue = 'a list * 'a list

  let empty = ([], [])

  let isEmpty = function [], _ -> true | _ -> false

  let get_half n l =
    let rec loop n acc l =
      match (n, l) with
      | 0, _ | _, [] -> (acc, l)
      | n, x :: xs -> loop (n - 1) (x :: acc) xs
    in
    loop n [] l

  let checkf = function
    | [], r ->
        let f, r = get_half (List.length r / 2) r in
        (List.rev r, f)
    | f, [] ->
        let f, r = get_half (List.length f / 2) f in
        (f, List.rev r)
    | q -> q

  let cons x (f, r) = checkf (x :: f, r)

  let head = function [], _ -> raise Empty | x :: _, _ -> x

  let tail = function [], _ -> raise Empty | _ :: f, r -> checkf (f, r)

  let snoc (f, r) x = checkf (f, x :: r)

  let last = function _, [] -> raise Empty | _, x :: _ -> x

  let init = function _, [] -> raise Empty | f, _ :: r -> checkf (f, r)
end
