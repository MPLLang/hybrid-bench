(* S small convenience module to avoid index arithmetic. *)
structure Points:
sig
  type point = real Seq.t
  type t

  val fromSeq: int -> real Seq.t -> t
  val toSeq: t -> real Seq.t
  val zero: int * int -> t

  val dims: t -> int
  val length: t -> int
  val nth: t -> int -> point
  val take: t -> int -> t
  val scale: real -> t -> t
  val slice: t -> int * int -> t
  val add: t * t -> t
end =
struct
  type point = real Seq.t
  type t = int * real Seq.t

  fun fromSeq d s =
    if Seq.length s mod d <> 0 then raise Size else (d, s)
  fun toSeq (i, s) = s
  fun zero (d, n) =
    (d, Seq.tabulate (fn _ => 0.0) (d * n))

  fun nth (d, s) i =
    (Seq.subseq s (i * d, d))
  fun take (d, s) n =
    (d, Seq.take s (d * n))
  fun dims (d, s) = d
  fun length (d, s) = Seq.length s div d
  fun scale (x: real) (d, s) =
    (d, Seq.map (fn y => y * x) s)
  fun slice (d, s) (i, n) =
    (d, Seq.subseq s (i * d, n * d))
  fun add ((d1, s1), (d2, s2)) =
    if d1 <> d2 orelse Seq.length s1 <> Seq.length s2 then
      raise Fail (Int.toString d1 ^ " " ^ Int.toString d2)
    else
      (d1, Seq.zipWith Real.+ (s1, s2))
end
