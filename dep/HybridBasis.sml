structure HybridBasis =
struct
  open Device

  fun compute_mid split lo hi =
    if lo = hi then
      lo
    else
      let
        val mid = lo + Real.ceil (Real.fromInt (hi - lo) * split)
      in
        (* handle edge cases for nasty values of `split` *)
        if mid = lo then lo + 1 else if mid = hi then hi - 1 else mid
      end


  fun parfor_hybrid split grain (lo, hi)
    (f: int -> unit, g: device_identifier -> (int * int) -> unit) =
    let
      fun base lo hi =
        ForkJoin.parfor grain (lo, hi) f

      fun loop lo hi =
        if hi - lo <= grain then
          ForkJoin.choice
            { prefer_cpu = fn _ => base lo hi
            , prefer_gpu = fn device => g device (lo, hi)
            }
        else
          let val mid = compute_mid split lo hi
          in ForkJoin.par (fn _ => loop_choose lo mid, fn _ => loop mid hi); ()
          end

      and loop_choose lo hi =
        ForkJoin.choice
          { prefer_cpu = fn _ => loop lo hi
          , prefer_gpu = fn device => g device (lo, hi)
          }
    in
      loop lo hi
    end


  fun reduce_hybrid split grain combine z (lo, hi)
    (f: int -> 'a, g: device_identifier -> (int * int) -> 'a) =
    let
      fun base lo hi =
        SeqBasis.reduce grain combine z (lo, hi) f

      fun loop lo hi =
        if hi - lo <= grain then
          ForkJoin.choice
            { prefer_cpu = fn _ => base lo hi
            , prefer_gpu = fn device => g device (lo, hi)
            }
        else
          let
            val mid = compute_mid split lo hi
          in
            combine (ForkJoin.par (fn _ => loop_choose lo mid, fn _ =>
              loop mid hi))
          end

      and loop_choose lo hi =
        ForkJoin.choice
          { prefer_cpu = fn _ => loop lo hi
          , prefer_gpu = fn device => g device (lo, hi)
          }
    in
      loop lo hi
    end


  fun tabulate_hybrid split grain (lo, hi)
    (f: int -> 'a, g: device_identifier -> (int * int * 'a Seq.t) -> unit) =
    let
      val data = ForkJoin.alloc (hi - lo)

      fun gpu device i j =
        g device (lo + i, lo + j, ArraySlice.slice (data, i, SOME (j - i)))

      fun loop i j =
        if j - i <= grain then
          ForkJoin.choice
            { prefer_cpu = fn _ =>
                Util.for (i, j) (fn k => Array.update (data, k, f (lo + k)))
            , prefer_gpu = fn device => gpu device i j
            }
        else
          let val mid = compute_mid split i j
          in ForkJoin.par (fn _ => loop_choose i mid, fn _ => loop mid j); ()
          end

      and loop_choose i j =
        ForkJoin.choice
          { prefer_cpu = fn _ => loop i j
          , prefer_gpu = fn device => gpu device i j
          }
    in
      loop 0 (hi - lo);
      ArraySlice.full data
    end


  fun filter_hybrid split grain (lo, hi)
    ( f_elem: int -> 'a
    , f_keep: int -> bool
    , g: device_identifier -> (int * int) -> 'a Seq.t
    ) =
    let
      val n = hi - lo

      fun gpu device i j =
        TreeSeq.fromArraySeq (g device (i, j))

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
            { prefer_cpu = fn _ => base i j
            , prefer_gpu = fn device => gpu device i j
            }
        else
          let
            val mid = compute_mid split i j
          in
            TreeSeq.append (ForkJoin.par (fn _ => loop_choose i mid, fn _ =>
              loop mid j))
          end

      and loop_choose i j =
        ForkJoin.choice
          { prefer_cpu = fn _ => loop i j
          , prefer_gpu = fn device => gpu device i j
          }
    in
      (* TODO (maybe): use TreeSeq.toBlocks here instead? This would avoid the
       * full copy for TreeSeq.toArraySeq. But, it's not so straightforward,
       * because eventually we would need to copy to the GPU at which point
       * we would need to flatten anyway. Hmmm....
       *)
      TreeSeq.toArraySeq (loop lo hi)
    end


  fun filter_hybrid_with_cleanup split grain (lo, hi)
    ( f_elem: int -> 'a
    , f_keep: int -> bool
    , g: device_identifier -> (int * int) -> 'b
    , g_cleanup: 'b -> 'a Seq.t
    ) =
    let
      datatype choice_result = CpuResult of 'a TreeSeq.t | GpuResult of 'b

      fun fixup choice_result =
        case choice_result of
          CpuResult t => t
        | GpuResult xx => TreeSeq.fromArraySeq (g_cleanup xx)

      val n = hi - lo

      fun gpu device i j = g device (i, j)

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
          fixup (ForkJoin.choice
            { prefer_cpu = fn _ => CpuResult (base i j)
            , prefer_gpu = fn device => GpuResult (gpu device i j)
            })
        else
          let
            val mid = compute_mid split i j
          in
            TreeSeq.append (ForkJoin.par (fn _ => loop_choose i mid, fn _ =>
              loop mid j))
          end

      and loop_choose i j =
        fixup (ForkJoin.choice
          { prefer_cpu = fn _ => CpuResult (loop i j)
          , prefer_gpu = fn device => GpuResult (gpu device i j)
          })
    in
      (* TODO (maybe): use TreeSeq.toBlocks here instead? This would avoid the
       * full copy for TreeSeq.toArraySeq. But, it's not so straightforward,
       * because eventually we would need to copy to the GPU at which point
       * we would need to flatten anyway. Hmmm....
       *)
      TreeSeq.toArraySeq (loop lo hi)
    end

end
