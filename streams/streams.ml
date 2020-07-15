module type STREAM = sig
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  val (++) : 'a stream -> 'a stream -> 'a stream
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

module Stream : STREAM = struct
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  let rec (++) s1 s2 =
    lazy (
      match s1 with
      | lazy Nil -> Lazy.force s2
      | lazy (Cons (hd,tl)) -> Cons (hd, tl ++ s2)
    )

  let rec take n s =
    lazy (
      if n = 0 then Nil
      else
        match s with
        | lazy Nil -> Nil
        | lazy (Cons (hd,tl)) -> Cons (hd, take (n-1) tl)
    )

  let rec drop n s =
    lazy (
      match n, s with
      | 0, _ -> Lazy.force s
      | _, lazy Nil -> Nil
      | _, lazy (Cons (_, tl)) -> Lazy.force (drop (n-1) tl)
    )

  let reverse s =
    let rec rev acc s =
      lazy (
        match s with
        | lazy Nil -> Lazy.force acc
        | lazy (Cons (hd, tl)) -> Lazy.force (rev (lazy (Cons (hd, acc))) tl)
      )
        in rev (lazy Nil) s
end
