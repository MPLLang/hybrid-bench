(* S small convenience module to avoid index arithmetic. *)
structure Points:
sig
  type t

  val fromSeq: int -> real Seq.t -> t
  val toSeq: t -> real Seq.t

  val dims: t -> int
  val length: t -> int
  val nth: t -> int -> real Seq.t
  val take: t -> int -> t
  val equal: t * t -> bool
end =
struct
  type t = int * real Seq.t
  fun fromSeq d s =
    if Seq.length s mod d <> 0 then raise Size else (d, s)
  fun toSeq (i, s) = s
  fun nth (d, s) i =
    (Seq.subseq s (i * d, d))
  fun take (d, s) n =
    (d, Seq.take s (d * n))
  fun dims (d, s) = d
  fun length (d, s) = Seq.length s div d
  fun equal ((d1, s1), (d2, s2)) =
    d1 = d2 andalso Seq.equal Real.== (s1, s2)
end
