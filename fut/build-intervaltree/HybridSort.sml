structure HybridSort =
struct

  (* fun cpuOnlySort xs =
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
    ForkJoin.choice {cpu = fn _ => Util.die "uh oh", gpu = FutSort.sort ctx xs} *)


  val quickThresh = CommandLineArgs.parseInt "quick-thresh" 5000
  val gpuMinThresh = CommandLineArgs.parseInt "gpu-min-thresh" 50000
  val split = CommandLineArgs.parseReal "split" 0.66

  fun cmpWith vals (i, j) =
    Int32.compare (Seq.nth vals (Int32.toInt i), Seq.nth vals (Int32.toInt j))

  (* sort idxs where the comparison 'i < j' is defined by vals[i] < vals[j] *)
  fun sort ctx (vals: Int32.int Seq.t) (idxs: Int32.int Seq.t) =
    if Seq.length idxs <= quickThresh then
      Quicksort.sort (cmpWith vals) idxs
    else
      let
        val half = Real.ceil (split * Real.fromInt (Seq.length idxs))
        val left = Seq.take idxs half
        val right = Seq.drop idxs half
        val (left', right') = ForkJoin.par (fn _ => sort ctx vals left, fn _ =>
          sortChoose ctx vals right)
      in
        Merge.merge (cmpWith vals) (left', right')
      end

  and sortChoose ctx vals idxs =
    if Seq.length idxs >= gpuMinThresh then
      ForkJoin.choice
        { cpu = fn _ => sort ctx vals idxs
        (* NOTE: vals needs to be initialized on GPU beforehand... *)
        , gpu = FutSort.sort ctx (*vals*) idxs
        }
    else
      sort ctx vals idxs

end
