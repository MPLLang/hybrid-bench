structure HybridSort =
struct
  structure CtxSet = CtxSetFn (structure F = FutharkSort)

  val gpu_sort_name = CommandLineArgs.parseString "gpu-sort" "radix"
  val futhark_sort =
    case gpu_sort_name of
      "radix" => FutharkSort.Entry.radix_sort_i32
    | "merge" => FutharkSort.Entry.merge_sort_i32
    | "bitonic" => FutharkSort.Entry.bitonic_merge_sort_i32
    | _ => Util.die ("unknown -gpu-sort " ^ gpu_sort_name)

  val do_raw = CommandLineArgs.parseFlag "raw"


  val memcpy_i32_into_cuda =
    _import "memcpy_i32_into_cuda" : Int32.int array * Int64.int * MLton.Pointer.t * Int64.int * Int64.int -> unit;


  val memcpy_i32_from_cuda =
    _import "memcpy_i32_from_cuda" : Int32.int array * Int64.int * MLton.Pointer.t * Int64.int * Int64.int -> unit;


  val cuda_malloc_managed_i32 =
    _import "cuda_malloc_managed_i32" : Int64.int -> MLton.Pointer.t;

  
  val cuda_free =
    _import "cuda_free" : MLton.Pointer.t -> unit;


  val get_raw_ptr =
    _import "get_raw_ptr" : Int32.int array * Int64.int -> MLton.Pointer.t;


  val check_raw_ptrs_equal =
    _import "check_raw_ptrs_equal" : MLton.Pointer.t * MLton.Pointer.t -> bool;


  fun sort_gpu_raw ctx seq =
    let
      val (a, lo, n) = ArraySlice.base seq

      val cuda_ptr = cuda_malloc_managed_i32 n

      val ((), tm) = Util.getTime (fn () =>
        ForkJoin.parfor 1 (0, Util.ceilDiv n 10000) (fn b =>
          let
            val a_start = lo + b*10000
            val a_stop = Int.min (lo + n, a_start + 10000)
            val cuda_ptr_start = b * 10000
            val len = a_stop - a_start
          in
            memcpy_i32_into_cuda (a, a_start, cuda_ptr, cuda_ptr_start, len)
          end))
      val () = print ("sort_gpu_raw memcpy: " ^ Time.fmt 4 tm ^ "s\n")

      val (seq_fut, tm) = Util.getTime (fn () => FutharkSort.Int32Array1.new_raw ctx cuda_ptr n)
      val () = print ("sort_gpu_raw new: " ^ Time.fmt 4 tm ^ "s\n")

      val (sorted_seq_fut, tm) = Util.getTime (fn () =>
        let
          val a = futhark_sort ctx seq_fut
        in
          FutharkSort.Context.sync ctx;
          a
        end)
      val () = print ("sort_gpu_raw sort: " ^ Time.fmt 4 tm ^ "s\n")
      val sorted_result_ptr = FutharkSort.Int32Array1.values_raw sorted_seq_fut

      val result: Int32.int array = ForkJoin.alloc n
      val ((), tm) = Util.getTime (fn () =>
        ForkJoin.parfor 1 (0, Util.ceilDiv n 10000) (fn b =>
          let
            val start = b * 10000
            val stop = Int.min (n, start + 10000)
            val len = stop - start
          in
            memcpy_i32_from_cuda (result, start, sorted_result_ptr, start, len)
          end))
      val () = print ("sort_gpu_raw memcpy: " ^ Time.fmt 4 tm ^ "s\n")

      val () = FutharkSort.Int32Array1.free seq_fut
      val () = FutharkSort.Int32Array1.free sorted_seq_fut
      val () = cuda_free cuda_ptr
    in
      ArraySlice.full result
    end
    handle FutharkSort.Error msg => Util.die ("Futhark error: " ^ msg)


  fun sort_gpu ctx seq : Int32.int Seq.t =
    let
      val (seq_fut, tm) = Util.getTime (fn _ =>
        FutharkSort.Int32Array1.new ctx seq (Seq.length seq))
      val _ = print ("sort_gpu new array: " ^ Time.fmt 4 tm ^ "s\n")
      val (sorted_fut, tm) = Util.getTime (fn _ =>
        let
          val a = futhark_sort ctx seq_fut
        in
          FutharkSort.Context.sync ctx;
          a
        end)
      val _ = print ("sort_gpu futhark_sort: " ^ Time.fmt 4 tm ^ "s\n")
      val (sorted, tm) = Util.getTime (fn _ =>
        FutharkSort.Int32Array1.values sorted_fut)
      val _ = print ("sort_gpu values: " ^ Time.fmt 4 tm ^ "s\n")
      val ((), tm) = Util.getTime (fn _ =>
        let
          val () = FutharkSort.Int32Array1.free seq_fut
          val () = FutharkSort.Int32Array1.free sorted_fut
        in
          ()
        end)
      val _ = print ("sort_gpu free: " ^ Time.fmt 4 tm ^ "s\n")
    in
      ArraySlice.full sorted
    end
    handle FutharkSort.Error msg => Util.die ("Futhark error: " ^ msg)


  val sort_gpu = fn ctx =>
    fn seq =>
      let
        val n = Seq.length seq
        val (result, tm) = Util.getTime (fn _ =>
          if do_raw then sort_gpu_raw ctx seq else sort_gpu ctx seq)
      in
        print ("sort_gpu (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n");
        result
      end


  val qsort_grain = BenchParams.Mergesort.qsort_grain


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


  val sort_split = BenchParams.Mergesort.sort_split
  val sort_grain = BenchParams.Mergesort.sort_grain

  fun split n =
    Real.ceil (sort_split * Real.fromInt n)

  fun sort ctxSet input =
    let
      val n = Seq.length input

      fun loop (xs: Int32.int Seq.t) =
        if Seq.length xs <= sort_grain then
          ForkJoin.choice
            { prefer_cpu = fn _ => sort_cpu xs
            , prefer_gpu = fn device =>
                sort_gpu (CtxSet.choose ctxSet device) xs
            }
        else
          let
            val half = split (Seq.length xs)
            val left = Seq.take xs half
            val right = Seq.drop xs half
            val (left', right') =
              ForkJoin.par (fn _ => loop_choose left, fn _ => loop right)
          in
            HybridMerge.merge ctxSet (left', right')
          end

      and loop_choose xs =
        if Seq.length xs <= sort_grain then
          loop xs
        else
          ForkJoin.choice
            { prefer_cpu = fn _ => loop xs
            , prefer_gpu = fn device =>
                sort_gpu (CtxSet.choose ctxSet device) xs
            }

    in
      loop input
    end

end
