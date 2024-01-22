structure Hist:
sig

  type grain = int
  type device_identifier = Device.device_identifier

  (* returns the result of each bin
   *
   * the elements are: { get_elem i | lo <= i < hi }
   * and, for each i, we use `get_bin i` to figure out which bin to add the
   * corresponding element to.
   *)
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


  (* nearly identical to inplace_hist, but also takes an extra argument `gpu`
   * where `gpu(i,j)` should be equivalent to performing a full hist on the
   * range [i,j).
   *)
  val inplace_hist_hybrid:
    grain (* cpu grain *)
    -> grain (* gpu grain *)
    -> real (* gpu split *)
    -> {combine: 'a * 'a -> 'a, fresh_neutral: unit -> 'a, num_bins: int}
    -> { lo: int
       , hi: int
       , get_bin: int -> int
       , modify_bin: int -> 'a -> unit
       , gpu: device_identifier -> (int * int) -> 'a Seq.t
       }
    -> 'a Seq.t


  val inplace_hist_hybrid_two_level:
    grain (* cpu grain *)
    -> grain (* gpu grain *)
    -> real (* gpu split *)
    -> { combine_inplace: 'a * 'a -> unit
       , fresh_neutral: unit -> 'a
       , num_bins: int
       }
    -> { lo: int
       , hi: int
       , get_bin: int -> int
       , modify_bin: int -> 'a -> unit
       , gpu: device_identifier -> (int * int) -> 'a Seq.t
       }
    -> 'a Seq.t

end =
struct
  type grain = int
  type device_identifier = Device.device_identifier


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


  fun inplace_hist_hybrid cpu_grain gpu_grain gpu_split
    (hist_args as {combine: 'a * 'a -> 'a, fresh_neutral: unit -> 'a, num_bins})
    { lo
    , hi
    , get_bin
    , modify_bin
    , gpu: device_identifier -> int * int -> 'a Seq.t
    } : 'a Seq.t =
    let
      fun fresh_acc () =
        Array.tabulate (num_bins, fn _ => fresh_neutral ())

      fun big_block blo bhi =
        (* let
          val acc = fresh_acc ()
        in
          Util.for (blo, bhi) (fn i =>
            let val bin = get_bin i
            in modify_bin i (Array.sub (acc, bin))
            end);
          ArraySlice.full acc
        end *)
        inplace_hist cpu_grain hist_args
          {lo = blo, hi = bhi, get_bin = get_bin, modify_bin = modify_bin}

      fun combine_accs (acc1: 'a Seq.t, acc2: 'a Seq.t) : 'a Seq.t =
        Seq.tabulate (fn i => combine (Seq.nth acc1 i, Seq.nth acc2 i)) num_bins

      val n = hi - lo
      val num_big_blocks = Util.ceilDiv n gpu_grain
    in
      HybridBasis.reduce_hybrid gpu_split 1 combine_accs
        (ArraySlice.full (fresh_acc ())) (0, num_big_blocks)
        ( fn b =>
            let
              val blo = lo + b * gpu_grain
              val bhi = Int.min (hi, blo + gpu_grain)
            in
              big_block blo bhi
            end

        , fn device =>
            fn (b1, b2) =>
              let
                val blo = lo + b1 * gpu_grain
                val bhi = Int.min (hi, lo + b2 * gpu_grain)
              in
                gpu device (blo, bhi)
              end
        )
    end


  fun inplace_hist_hybrid_two_level cpu_grain gpu_grain gpu_split
    (hist_args as
       {combine_inplace: 'a * 'a -> unit, fresh_neutral: unit -> 'a, num_bins})
    { lo
    , hi
    , get_bin
    , modify_bin
    , gpu: device_identifier -> int * int -> 'a Seq.t
    } : 'a Seq.t =
    let
      fun fresh_acc () =
        Seq.tabulate (fn _ => fresh_neutral ()) (num_bins)

      fun combine_accs (acc1: 'a Seq.t, acc2: 'a Seq.t) : 'a Seq.t =
        ( Util.for (0, Seq.length acc1) (fn i =>
            combine_inplace (Seq.nth acc1 i, Seq.nth acc2 i))
        ; acc1
        )


      fun base_cpu start stop =
        let
          val acc = fresh_acc ()
        in
          Util.for (start, stop) (fn i =>
            let val bin = get_bin i
            in modify_bin i (Seq.nth acc bin)
            end);
          acc
        end


      fun loop_cpu start stop =
        if stop - start <= cpu_grain then
          base_cpu start stop
        else
          let
            val mid = start + (stop - start) div 2
            val (l, r) = ForkJoin.par (fn _ => loop_cpu start mid, fn _ =>
              loop_cpu mid stop)
          in
            combine_accs (l, r)
          end


      fun loop start stop =
        if stop - start <= cpu_grain then
          base_cpu start stop
        else
          let
            val mid = start + (stop - start) div 2
            val (l, r) = ForkJoin.par (fn _ => loop_choose start mid, fn _ =>
              loop mid stop)
          in
            combine_accs (l, r)
          end


      and loop_choose start stop =
        if stop - start <= gpu_grain then
          loop_cpu start stop
        else
          ForkJoin.choice
            { prefer_cpu = fn _ => loop start stop
            , prefer_gpu = fn device => gpu device (start, stop)
            }


      val n = hi - lo
      val block_size = Real.floor (gpu_split * Real.fromInt n)
      val num_blocks = Util.ceilDiv n block_size

      fun outer_loop blo bhi =
        if blo >= bhi then
          raise Fail "impossible"
        else if blo + 1 = bhi then
          let
            val start = lo + blo * block_size
            val stop = Int.min (start + block_size, lo + n)
          in
            ForkJoin.choice
              { prefer_cpu = fn _ => loop start stop
              , prefer_gpu = fn device => gpu device (start, stop)
              }
          end
        else
          let
            val bmid = blo + (bhi - blo) div 2
            val (l, r) = ForkJoin.par (fn _ => outer_loop blo bmid, fn _ =>
              outer_loop bmid bhi)
          in
            combine_accs (l, r)
          end

    in
      outer_loop 0 num_blocks
    end

end
