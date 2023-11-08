structure Hist:
sig

  type grain = int

  (* returns the result of each bin *)
  val hist: grain
            -> {combine: 'a * 'a -> 'a, neutral: 'a, num_bins: int}
            -> {lo: int, hi: int, get_bin: int -> int, get_elem: int -> 'a}
            -> 'a Seq.t


  (* The above hist is more "purely functional" and updates bins via:
   *   j = get_bin i
   *   bins[j] := combine(bins[j], get_elem i)
   *
   * However, this can be expensive, because `get_elem ...` might need to
   * allocate, and also `combine ...` might need to allocate.
   *
   * We can optimize this by operating on bins in-place:
   *   j = get_bin i
   *   modify_bin i (bins[j])
   *
   * Now, modify_bin can be more specialized to avoid allocation and write
   * results directly into the bin's accumulator.
   *)
  val inplace_hist:
    grain
    -> {combine: 'a * 'a -> 'a, fresh_neutral: unit -> 'a, num_bins: int}
    -> {lo: int, hi: int, get_bin: int -> int, modify_bin: int -> 'a -> unit}
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


  fun inplace_hist grain {combine, fresh_neutral, num_bins}
    {lo, hi, get_bin, modify_bin} =
    let
      fun fresh_acc () =
        Array.tabulate (num_bins, fn _ => fresh_neutral ())

      fun block blo bhi =
        let
          val acc = fresh_acc ()
        in
          Util.for (blo, bhi) (fn i =>
            let val bin = get_bin i
            in modify_bin i (Array.sub (acc, bin))
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
