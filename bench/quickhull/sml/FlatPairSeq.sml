structure FlatPairSeq:
sig
  type 'a t

  val fromArraySeq: ('a * 'a) Seq.t -> 'a t
  val toArraySeq: 'a t -> ('a * 'a) Seq.t

  val length: 'a t -> int
  val nth: 'a t -> int -> ('a * 'a)

  val viewData: 'a t -> 'a Seq.t
end =
struct

  type 'a t = 'a Seq.t (* of twice the length *)

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
