structure HybridSort =
struct

  fun sort_gpu ctx seq : Int32.int Seq.t =
    let
      val seq_fut = FutharkSort.Int32Array1.new ctx seq (Seq.length seq)
      val sorted_fut = FutharkSort.Entry.sort ctx seq_fut
      val sorted = FutharkSort.Int32Array1.values sorted_fut
      val () = FutharkSort.Int32Array1.free seq_fut
      val () = FutharkSort.Int32Array1.free sorted_fut
    in
      ArraySlice.full sorted
    end


  fun sort_cpu xs =
    if Seq.length xs < 1000 then
      Quicksort.sort Int32.compare xs
    else
      let
        val half = Seq.length xs div 2
        val left = Seq.take xs half
        val right = Seq.drop xs half
      in
        Merge.merge Int32.compare (ForkJoin.par (fn _ => sort_cpu left, fn _ =>
          sort_cpu right))
      end


  val quickThresh = CommandLineArgs.parseInt "quick-thresh" 5000
  val gpuMinThresh = CommandLineArgs.parseInt "gpu-min-thresh" 100000
  val split_frac = CommandLineArgs.parseReal "split" 0.33

  fun split n =
    Real.ceil (split_frac * Real.fromInt n)

  fun sort ctx (xs: Int32.int Seq.t) =
    if Seq.length xs <= quickThresh then
      Quicksort.sort Int32.compare xs
    else
      let
        val half = split (Seq.length xs)
        val left = Seq.take xs half
        val right = Seq.drop xs half
        val (left', right') =
          ForkJoin.par (fn _ => sort_choose ctx left, fn _ => sort ctx right)
      in
        Merge.merge Int32.compare (left', right')
      end

  and sort_choose ctx xs =
    if Seq.length xs >= gpuMinThresh then
      ForkJoin.choice
        {prefer_cpu = fn _ => sort ctx xs, prefer_gpu = fn _ => sort_gpu ctx xs}
    else
      sort ctx xs

end
