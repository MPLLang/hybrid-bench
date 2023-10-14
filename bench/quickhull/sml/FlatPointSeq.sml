structure FlatPointSeq:
sig
  type t

  val fromArraySeq: (real * real) Seq.t -> t
  val toArraySeq: t -> (real * real) Seq.t

  val length: t -> int
  val nth: t -> int -> (real * real)

  val viewData: t -> real Seq.t
end =
struct

  type t = real Seq.t (* of twice the length *)

  fun fromArraySeq s =
    let
      val n = Seq.length s
      val result = ForkJoin.alloc (2 * n)
    in
      ForkJoin.parfor 5000 (0, n) (fn i =>
        let val (x, y) = Seq.nth s i
        in Array.update (result, 2 * i, x); Array.update (result, 2 * i + 1, y)
        end);
      ArraySlice.full result
    end


  fun nth fs i =
    (Seq.nth fs (2 * i), Seq.nth fs (2 * i + 1))
  fun length fs = Seq.length fs div 2

  fun toArraySeq fs =
    Seq.tabulate (nth fs) (length fs)

  fun viewData x = x
end
