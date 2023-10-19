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

  val sort_gpu = fn ctx =>
    fn seq =>
      let
        val n = Seq.length seq
        val (result, tm) = Util.getTime (fn _ => sort_gpu ctx seq)
      in
        print ("sort_gpu (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n");
        result
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


  val grain = CommandLineArgs.parseInt "grain" 5000
  val sort_split = CommandLineArgs.parseReal "sort-split" 0.36

  fun split n =
    Real.ceil (sort_split * Real.fromInt n)

  fun sort ctx input =
    let
      val n = Seq.length input

      fun base xs = Quicksort.sort Int32.compare xs

      fun loop (xs: Int32.int Seq.t) =
        if Seq.length xs <= grain then
          ForkJoin.choice
            {prefer_cpu = fn _ => base xs, prefer_gpu = fn _ => sort_gpu ctx xs}
        else
          let
            val half = split (Seq.length xs)
            val left = Seq.take xs half
            val right = Seq.drop xs half
            val (left', right') =
              ForkJoin.par (fn _ => loop_choose left, fn _ => loop right)
          in
            HybridMerge.merge ctx (left', right')
          end

      and loop_choose xs =
        ForkJoin.choice
          {prefer_cpu = fn _ => loop xs, prefer_gpu = fn _ => sort_gpu ctx xs}

    in
      loop input
    end

end
