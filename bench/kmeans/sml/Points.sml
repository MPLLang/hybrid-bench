(* S small convenience module to avoid index arithmetic. *)
structure Points:
sig
  type t

  val fromArraySeq: int -> real Seq.t -> t
  val toArraySeq: t -> real Seq.t

  val dims: t -> int
  val length: t -> int
  val nth: t -> int -> real Seq.t
  val take: t -> int -> t
end =
struct
  type t = int * real Seq.t
  fun fromArraySeq d s =
    if Seq.length s mod d <> 0 then raise Size else (d, s)
  fun toArraySeq (i, s) = s
  fun nth (d, s) i =
    (Seq.subseq s (i * d, d))
  fun take (d, s) n =
    (d, Seq.take s (d * n))
  fun dims (d, s) = d
  fun length (d, s) = Seq.length s div d
end
