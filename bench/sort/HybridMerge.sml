structure HybridMerge =
struct

  fun slice_idxs s i j =
    ArraySlice.subslice (s, i, SOME (j - i))


  fun write_merge_gpu ctx (xs, ys) output =
    let
      val xs_fut = FutharkSort.Int32Array1.new ctx xs (Seq.length xs)
      val ys_fut = FutharkSort.Int32Array1.new ctx ys (Seq.length ys)
      val sorted_fut = FutharkSort.Entry.merge ctx (xs_fut, ys_fut)
      val () = FutharkSort.Int32Array1.values_into sorted_fut output
      val () = FutharkSort.Int32Array1.free xs_fut
      val () = FutharkSort.Int32Array1.free ys_fut
      val () = FutharkSort.Int32Array1.free sorted_fut
    in
      ()
    end


  fun write_merge ctx thresh (s1, s2) t =
    if ArraySlice.length t <= 5000 then
      Merge.writeMergeSerial Int32.compare (s1, s2) t
    else if ArraySlice.length s1 = 0 then
      Util.foreach s2 (fn (i, x) => ArraySlice.update (t, i, x))
    else
      let
        val n1 = ArraySlice.length s1
        val n2 = ArraySlice.length s2
        val mid1 = n1 div 2
        val pivot = ArraySlice.sub (s1, mid1)
        val mid2 = BinarySearch.search Int32.compare s2 pivot

        val l1 = slice_idxs s1 0 mid1
        val r1 = slice_idxs s1 (mid1 + 1) n1
        val l2 = slice_idxs s2 0 mid2
        val r2 = slice_idxs s2 mid2 n2

        val _ = ArraySlice.update (t, mid1 + mid2, pivot)
        val tl = slice_idxs t 0 (mid1 + mid2)
        val tr = slice_idxs t (mid1 + mid2 + 1) (ArraySlice.length t)
      in
        ForkJoin.par (fn _ => write_merge_choose ctx thresh (l1, l2) tl, fn _ =>
          write_merge ctx thresh (r1, r2) tr);
        ()
      end


  and write_merge_choose ctx thresh (s1, s2) t =
    if
      ArraySlice.length t < 5000 orelse ArraySlice.length t > thresh
      orelse ArraySlice.length s1 = 0 orelse ArraySlice.length s2 = 0
    then
      write_merge ctx thresh (s1, s2) t
    else
      ForkJoin.choice
        { prefer_cpu = fn _ => write_merge ctx thresh (s1, s2) t
        , prefer_gpu = fn _ => write_merge_gpu ctx (s1, s2) t
        }


  val merge_split = CommandLineArgs.parseReal "merge-split" 0.0


  fun merge ctx (s1, s2) =
    let
      val n = ArraySlice.length s1 + ArraySlice.length s2
      val out = ArraySlice.full (ForkJoin.alloc n)
      val thresh = Real.ceil (merge_split * Real.fromInt n)
    in
      write_merge ctx thresh (s1, s2) out;
      out
    end

end
