fun reduceOnGPU (arr: MLton.Pointer.t, lo, hi) =
  let
    val result = GPUKernels.reductionManaged (arr, lo, hi)
  in
    print ("|onGPU size " ^ Int.toString (hi-lo) ^ "\n");
    result
  end


fun plusReduceOnlyCPU (arr, n) =
  SeqBasis.reduce 10000 op+ 0 (0, n) (fn i => 
    Int32.toInt (MLton.Pointer.getInt32 (arr, i))
  )

fun plusReduceOnlyGPU (arr, n) =
  reduceOnGPU (arr, 0, n)

val splitFraction = CommandLineArgs.parseReal "split" 0.66
val _ = print ("split " ^ Real64.toString splitFraction ^ "\n")

fun plusReduceHybrid (arr, n) =
  let
    val lock = SpinLock.new ()
    fun maybeGPU (lo, hi) = 
      if (hi - lo) >= 100000 andalso SpinLock.trylock lock then 
        let
          val result = reduceOnGPU(arr, lo, hi)

          val _ = SpinLock.unlock lock
        in
          result
        end
      else
        wtr (lo, hi)

     and wtr (lo, hi) =
        if (hi - lo) = 0 then
          0
        else if hi - lo < 10000 then
          SeqBasis.foldl op+ 0 (lo, hi) (fn i => Int32.toInt (MLton.Pointer.getInt32 (arr, i)))
        else
          let 
            val mid = lo + Real64.round (splitFraction * Real64.fromInt (hi-lo))
            val (left, right) =
              ForkJoin.par (fn() => wtr (lo, mid),
                            fn() => maybeGPU (mid, hi))
          in
            left + right
          end 
  in
    wtr(0, n)
  end


val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "hybrid"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val reduce =
  case impl of
    "cpu" => plusReduceOnlyCPU
  | "gpu" => plusReduceOnlyGPU
  | "hybrid" => plusReduceHybrid
  | _ => Util.die ("unknown -impl " ^ impl)

fun genInput () =
  let
    (* need 4n bytes to store n 32-bit integers *)
    val arr = GPUKernels.allocCudaManagedMemory (4 * Int64.fromInt n)
  in
    ForkJoin.parfor 10000 (0, n) (fn i =>
      MLton.Pointer.setInt32 (arr, i, Int32.fromInt 1)
    );
    
    (arr, n)
  end

fun freeInput (arr, _) =
  GPUKernels.freeCudaMemory arr

val (input, tm) = Util.getTime (fn _ => genInput ())
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run ("reduce " ^ impl) (fn _ =>
  ( print "\n+---------------------------\n"
  ; reduce input
  )
)
val _ = freeInput input
val _ = print ("result " ^ Int.toString result ^ "\n")