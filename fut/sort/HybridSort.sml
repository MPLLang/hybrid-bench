structure HybridSort =
struct

  fun cpuOnlySort xs =
    if Seq.length xs < 1000 then
      Quicksort.sort Int32.compare xs
    else
      let
        val half = Seq.length xs div 2
        val left = Seq.take xs half
        val right = Seq.drop xs half
      in
        Merge.merge Int32.compare
          (ForkJoin.par (fn _ => cpuOnlySort left, fn _ => cpuOnlySort right))
      end


  fun gpuOnlySort ctx xs =
    ForkJoin.choice {cpu = fn _ => Util.die "uh oh", gpu = FutSort.sort ctx xs}


  val quickThresh = CommandLineArgs.parseInt "quick-thresh" 5000
  val gpuMinThresh = CommandLineArgs.parseInt "gpu-min-thresh" 100000
  val split = CommandLineArgs.parseReal "split" 0.66

  fun sort ctx (xs: Int32.int Seq.t) =
    if Seq.length xs <= quickThresh then
      Quicksort.sort Int32.compare xs
    else
      let
        val half = Real.ceil (split * Real.fromInt (Seq.length xs))
        val left = Seq.take xs half
        val right = Seq.drop xs half
        val (left', right') = ForkJoin.par (fn _ => sort ctx left, fn _ =>
          sortChoose ctx right)
      in
        Merge.merge Int32.compare (left', right')
      end

  and sortChoose ctx xs =
    if Seq.length xs >= gpuMinThresh then
      ForkJoin.choice {cpu = fn _ => sort ctx xs, gpu = FutSort.sort ctx xs}
    else
      sort ctx xs

end
