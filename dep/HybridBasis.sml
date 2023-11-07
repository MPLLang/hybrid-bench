structure HybridBasis =
struct

  fun compute_mid split lo hi =
    lo + Real.ceil (Real.fromInt (hi - lo) * split)

  fun parfor_hybrid split grain (lo, hi) (f, g) =
    let
      fun base lo hi =
        ForkJoin.parfor grain (lo, hi) f

      fun loop lo hi =
        if hi - lo <= grain then
          ForkJoin.choice
            {prefer_cpu = fn _ => base lo hi, prefer_gpu = fn _ => g (lo, hi)}
        else
          let val mid = compute_mid split lo hi
          in ForkJoin.par (fn _ => loop_choose lo mid, fn _ => loop mid hi); ()
          end

      and loop_choose lo hi =
        ForkJoin.choice
          {prefer_cpu = fn _ => loop lo hi, prefer_gpu = fn _ => g (lo, hi)}
    in
      loop lo hi
    end


  fun reduce_hybrid split grain combine z (lo, hi) (f, g) =
    let
      fun base lo hi =
        SeqBasis.reduce grain combine z (lo, hi) f

      fun loop lo hi =
        if hi - lo <= grain then
          ForkJoin.choice
            {prefer_cpu = fn _ => base lo hi, prefer_gpu = fn _ => g (lo, hi)}
        else
          let
            val mid = compute_mid split lo hi
          in
            combine (ForkJoin.par (fn _ => loop_choose lo mid, fn _ =>
              loop mid hi))
          end

      and loop_choose lo hi =
        ForkJoin.choice
          {prefer_cpu = fn _ => loop lo hi, prefer_gpu = fn _ => g (lo, hi)}
    in
      loop lo hi
    end


  fun tabulate_hybrid split grain (lo, hi) (f, g) =
    let
      val data = ForkJoin.alloc (hi - lo)

      fun gpu i j =
        g (lo + i, lo + j, ArraySlice.slice (data, i, SOME (j - i)))

      fun loop i j =
        if j - i <= grain then
          ForkJoin.choice
            { prefer_cpu = fn _ =>
                Util.for (i, j) (fn k => Array.update (data, k, f (lo + k)))
            , prefer_gpu = fn _ => gpu i j
            }
        else
          let val mid = compute_mid split i j
          in ForkJoin.par (fn _ => loop_choose i mid, fn _ => loop mid j); ()
          end

      and loop_choose i j =
        ForkJoin.choice
          {prefer_cpu = fn _ => loop i j, prefer_gpu = fn _ => gpu i j}
    in
      loop 0 (hi - lo);
      ArraySlice.full data
    end


  fun filter_hybrid split grain (lo, hi)
    (f_elem: int -> 'a, f_keep: int -> bool, g: (int * int) -> 'a Seq.t) =
    let
      val n = hi - lo

      fun gpu i j =
        TreeSeq.fromArraySeq (g (i, j))

      fun base i j =
        let
          val flags: Word8.word array = ForkJoin.alloc (j - i)
          val num_keep = Util.loop (0, j - i) 0 (fn (count, k) =>
            if f_keep (i + k) then (Array.update (flags, k, 0w1); count + 1)
            else (Array.update (flags, k, 0w0); count))
          val output: 'a array = ForkJoin.alloc num_keep
        in
          Util.loop (0, j - i) 0 (fn (count, k) =>
            if Array.sub (flags, k) = 0w0 then count
            else (Array.update (output, count, f_elem (i + k)); count + 1));

          TreeSeq.fromArraySeq (ArraySlice.full output)
        end

      fun loop i j =
        if j - i <= grain then
          ForkJoin.choice
            {prefer_cpu = fn _ => base i j, prefer_gpu = fn _ => gpu i j}
        else
          let
            val mid = compute_mid split i j
          in
            TreeSeq.append (ForkJoin.par (fn _ => loop_choose i mid, fn _ =>
              loop mid j))
          end

      and loop_choose i j =
        ForkJoin.choice
          {prefer_cpu = fn _ => loop i j, prefer_gpu = fn _ => gpu i j}
    in
      (* TODO (maybe): use TreeSeq.toBlocks here instead? This would avoid the
       * full copy for TreeSeq.toArraySeq. But, it's not so straightforward,
       * because eventually we would need to copy to the GPU at which point
       * we would need to flatten anyway. Hmmm....
       *)
      TreeSeq.toArraySeq (loop lo hi)
    end

end
