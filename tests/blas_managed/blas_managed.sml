fun reduceOnGPU (arr, lo, hi) =
  let
    val result = GPUKernels.reduction (arr, lo, hi)
  in
    print ("|onGPU\n"
           ^ "|  lo = " ^ Int.toString lo ^ "\n"
           ^ "|  hi-lo = " ^ Int.toString (hi-lo) ^ "\n"
           ^ "|  result = " ^ Int.toString result ^ "\n");
    result
  end

fun plusReduceOnlyCPU arr =
  SeqBasis.reduce 10000 op+ 0 (0, Array.length arr) (fn i => Array.sub (arr, i))


fun plusReduceOnlyGPU arr =
  reduceOnGPU (arr, 0, Array.length arr)

val splitFraction = CommandLineArgs.parseReal "split" 0.66
val _ = print ("split " ^ Real64.toString splitFraction ^ "\n")

fun plusReduceHybrid arr =
  let
    val n = Array.length arr
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
          let
          (* Array.sub (arr1, lo) *)
          (* val _ = print ("on cpu" ^ "\n") *)
          in
            SeqBasis.foldl op+ 0 (lo, hi) (fn i => Array.sub (arr, i))
          (*Util.loop (lo, hi) 0 (fn(i,j) => i + Array.sub (arr1, j))*)
          end
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

val (input, tm) = Util.getTime (fn _ => SeqBasis.tabulate 10000 (0, n) (fn i => 1))
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run ("reduce " ^ impl) (fn _ =>
  ( print "\n+---------------------------\n"
  ; reduce input
  )
)
val _ = print ("result " ^ Int.toString result ^ "\n")