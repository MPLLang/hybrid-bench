(* S small convenience module to avoid index arithmetic. *)
structure Points:
sig
  type point = real Seq.t
  type t

  val fromSeq: int -> real Seq.t -> t
  val toSeq: t -> real Seq.t

  val dims: t -> int
  val length: t -> int
  val nth: t -> int -> point
  val take: t -> int -> t
end =
struct
  type point = real Seq.t
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
end
