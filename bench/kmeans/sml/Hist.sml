structure Hist:
sig

  type grain = int

  (* returns the result of each bin *)
  val hist: grain
            -> {combine: 'a * 'a -> 'a, neutral: 'a, num_bins: int}
            -> {lo: int, hi: int, get_bin: int -> int, get_elem: int -> 'a}
            -> 'a Seq.t

end =
struct

  type grain = int


  fun hist grain {combine, neutral, num_bins} {lo, hi, get_bin, get_elem} =
    let
      fun fresh_acc () = Array.array (num_bins, neutral)

      fun block blo bhi =
        let
          val acc = fresh_acc ()
        in
          Util.for (blo, bhi) (fn i =>
            let
              val bin = get_bin i
              val x = Array.sub (acc, bin)
            in
              Array.update (acc, bin, combine (x, get_elem i))
            end);
          acc
        end

      fun combine_accs (acc1, acc2) =
        Array.tabulate (num_bins, fn i =>
          combine (Array.sub (acc1, i), Array.sub (acc2, i)))

      val n = hi - lo
      val num_blocks = Util.ceilDiv n grain
    in
      ArraySlice.full
        (SeqBasis.reduce 1 combine_accs (fresh_acc ()) (0, num_blocks) (fn b =>
           let
             val blo = lo + b * grain
             val bhi = Int.min (hi, blo + grain)
           in
             block blo bhi
           end))
    end

end
