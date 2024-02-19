structure HybridMerge =
struct

  fun slice_idxs s i j =
    ArraySlice.subslice (s, i, SOME (j - i))

  val gpu_merge_block = BenchParams.Mergesort.gpu_merge_block

  fun write_merge_gpu ctx (xs, ys) output =
    let
      val xs_fut = FutharkSort.Int32Array1.new ctx xs (Seq.length xs)
      val ys_fut = FutharkSort.Int32Array1.new ctx ys (Seq.length ys)
      val sorted_fut =
        FutharkSort.Entry.merge_i32 ctx
          (Int64.fromInt gpu_merge_block, xs_fut, ys_fut)
      val () = FutharkSort.Int32Array1.values_into sorted_fut output
      val () = FutharkSort.Int32Array1.free xs_fut
      val () = FutharkSort.Int32Array1.free ys_fut
      val () = FutharkSort.Int32Array1.free sorted_fut
    in
      ()
    end
    handle FutharkSort.Error msg => Util.die ("Futhark error: " ^ msg)


  val write_merge_gpu = fn ctx =>
    fn (xs, ys) =>
      fn output =>
        let
          val n = Seq.length output
          val (result, tm) = Util.getTime (fn _ =>
            write_merge_gpu ctx (xs, ys) output)
        in
          print
            ("write_merge_gpu (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm
             ^ "s\n");

          result
        end


  val merge_grain = BenchParams.Mergesort.merge_grain
  val merge_split = BenchParams.Mergesort.merge_split

  fun split n =
    Real.ceil (merge_split * Real.fromInt n)

  fun write_merge ctx (s1, s2) t =
    if ArraySlice.length s1 = 0 then
      Util.foreach s2 (fn (i, x) => ArraySlice.update (t, i, x))
    else if ArraySlice.length s2 = 0 then
      Util.foreach s1 (fn (i, x) => ArraySlice.update (t, i, x))
    else if ArraySlice.length t <= merge_grain then
      ForkJoin.choice
        { prefer_cpu = fn _ => Merge.writeMerge Int32.compare (s1, s2) t
        , prefer_gpu = fn _ => write_merge_gpu ctx (s1, s2) t
        }
    else
      let
        val n1 = ArraySlice.length s1
        val n2 = ArraySlice.length s2
        val half = split (n1 + n2)

        val (mid1, mid2) =
          DualBinarySearch.split_count_slice Int32.compare (s1, s2) half

        val l1 = slice_idxs s1 0 mid1
        val r1 = slice_idxs s1 mid1 n1
        val l2 = slice_idxs s2 0 mid2
        val r2 = slice_idxs s2 mid2 n2

        val tl = slice_idxs t 0 (mid1 + mid2)
        val tr = slice_idxs t (mid1 + mid2) (ArraySlice.length t)
      in
        ForkJoin.par (fn _ => write_merge_choose ctx (l1, l2) tl, fn _ =>
          write_merge ctx (r1, r2) tr);
        ()
      end


  and write_merge_choose ctx (s1, s2) t =
    if ArraySlice.length t <= merge_grain then
      write_merge ctx (s1, s2) t
    else
      ForkJoin.choice
        { prefer_cpu = fn _ => write_merge ctx (s1, s2) t
        , prefer_gpu = fn _ => write_merge_gpu ctx (s1, s2) t
        }


  fun merge ctx (s1, s2) =
    let
      val n = ArraySlice.length s1 + ArraySlice.length s2
      val out = ArraySlice.full (ForkJoin.alloc n)
    in
      write_merge ctx (s1, s2) out;
      out
    end

end
