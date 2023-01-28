type futCtx = MLton.Pointer.t
val futInit = _import "futInit" public : unit -> futCtx;
val futFinish = _import "futFinish" public : futCtx -> unit;
val rawFutSieve =
  _import "futSieve" public : futCtx * Int64.int * Int64.int array * Int64.int * Word8.word array -> unit;

fun doSieveOnGpu ctx n sqrtPrimes =
  let
    val numFlags = n + 1
    val flags: Word8.word array = ForkJoin.alloc numFlags
    fun isMarked i =
      Array.sub (flags, i) = 0w1
  in
    rawFutSieve (ctx, Array.length sqrtPrimes, sqrtPrimes, numFlags, flags);
    isMarked
  end


fun doSieveOnCpu _ n sqrtPrimes =
  let
    (* allocate array of flags to mark primes. *)
    val flags = ForkJoin.alloc (n + 1) : Word8.word array
    fun mark i = Array.update (flags, i, 0w1)
    fun unmark i = Array.update (flags, i, 0w0)
    fun isMarked i =
      Array.sub (flags, i) = 0w1

    (* initially, mark every number *)
    val _ = ForkJoin.parfor 10000 (0, n + 1) mark

    (* unmark every multiple of every prime in sqrtPrimes *)
    val _ = ForkJoin.parfor 1 (0, Array.length sqrtPrimes) (fn i =>
      let
        val p = Array.sub (sqrtPrimes, i)
        val numMultiples = n div p - 1
      in
        ForkJoin.parfor 4096 (0, numMultiples) (fn j => unmark ((j + 2) * p))
      end)
  in
    isMarked
  end


fun doSieveOnGpuIfBiggerThan threshold ctx n sqrtPrimes =
  if n >= threshold then doSieveOnGpu ctx n sqrtPrimes
  else doSieveOnCpu ctx n sqrtPrimes

functor Primes
  (type ctx
   val impl: string
   val init: unit -> ctx
   val siever: ctx -> int -> int array -> (int -> bool)
   val finish: ctx -> unit):
sig
  val runBenchmark: int -> int array
end =
struct
  fun primes ctx n =
    let
      fun loop n =
        if n < 2 then
          ForkJoin.alloc 0
        else
          let
            (* all primes up to sqrt(n) *)
            val sqrtPrimes = loop (Real.floor (Real.Math.sqrt (Real.fromInt n)))
            val isMarked = siever ctx n sqrtPrimes
            val result =
              (* for every i in 2 <= i <= n, filter those that are still marked *)
              SeqBasis.filter 4096 (2, n + 1) (fn i => i) isMarked
          in
            result
          end

      val result = loop n
    in
      result
    end


  fun runBenchmark n =
    let
      val ctx = init ()
      val result = Benchmark.run ("primes " ^ impl) (fn _ => primes ctx n)
      val _ = finish ctx
    in
      result
    end

end

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"
val gpuThresh = CommandLineArgs.parseInt "gpu-thresh" 1000000

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

structure PrimesCPU =
  Primes
    (val impl = "cpu"
     type ctx = unit
     fun init () = ()
     val siever = doSieveOnCpu
     fun finish () = ())
structure PrimesGPU =
  Primes
    (val impl = "gpu"
     type ctx = futCtx
     val init = futInit
     val siever = doSieveOnGpu
     val finish = futFinish)
structure PrimesHybrid =
  Primes
    (val impl = "hybrid"
     type ctx = futCtx
     val init = futInit
     val siever = doSieveOnGpuIfBiggerThan gpuThresh
     val finish = futFinish)

val bench =
  case impl of
    "cpu" => PrimesCPU.runBenchmark
  | "gpu" => PrimesGPU.runBenchmark
  | "hybrid" => PrimesHybrid.runBenchmark
  | _ => Util.die ("unknown -impl " ^ impl)

val result = bench n
val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result ^ "\n")
