structure HybridSort =
struct

  val gpu_sort_name = CommandLineArgs.parseString "gpu-sort" "radix"
  val futhark_sort =
    case gpu_sort_name of
      "radix" => FutharkSort.Entry.radix_sort_i32
    | "merge" => FutharkSort.Entry.merge_sort_i32
    | "bitonic" => FutharkSort.Entry.bitonic_merge_sort_i32
    | _ => Util.die ("unknown -gpu-sort " ^ gpu_sort_name)


  fun sort_gpu ctx seq : Int32.int Seq.t =
    let
      val seq_fut = FutharkSort.Int32Array1.new ctx seq (Seq.length seq)
      val sorted_fut = futhark_sort ctx seq_fut
      val sorted = FutharkSort.Int32Array1.values sorted_fut
      val () = FutharkSort.Int32Array1.free seq_fut
      val () = FutharkSort.Int32Array1.free sorted_fut
    in
      ArraySlice.full sorted
    end
    handle FutharkSort.Error msg => Util.die ("Futhark error: " ^ msg)


  val sort_gpu = fn ctx =>
    fn seq =>
      let
        val n = Seq.length seq
        val (result, tm) = Util.getTime (fn _ => sort_gpu ctx seq)
      in
        print ("sort_gpu (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n");
        result
      end


  val qsort_grain = CommandLineArgs.parseInt "qsort-grain" 100000


  fun sort_cpu xs =
    if Seq.length xs <= qsort_grain then
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


  val sort_split = CommandLineArgs.parseReal "sort-split" 0.36
  val sort_grain = CommandLineArgs.parseInt "sort-grain" 1500000

  fun split n =
    Real.ceil (sort_split * Real.fromInt n)

  fun sort ctx input =
    let
      val n = Seq.length input

      fun base xs = Quicksort.sort Int32.compare xs

      fun loop (xs: Int32.int Seq.t) =
        if Seq.length xs <= sort_grain then
          ForkJoin.choice
            { prefer_cpu = fn _ => sort_cpu xs
            , prefer_gpu = fn _ => sort_gpu ctx xs
            }
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
        if Seq.length xs <= sort_grain then
          loop xs
        else
          ForkJoin.choice
            {prefer_cpu = fn _ => loop xs, prefer_gpu = fn _ => sort_gpu ctx xs}

    in
      loop input
    end

end
