structure CLA = CommandLineArgs

val ctx = FutharkPrimes.ctx_new FutharkPrimes.default_cfg

(* ==========================================================================
 * primes on gpu
 *)

fun primes_gpu n : Int64.int array =
  let
    val farr = FutharkPrimes.Entry.primes ctx n
    val _ = FutharkPrimes.ctx_sync ctx
    val output = FutharkPrimes.Int64Array1.values farr
    val _ = FutharkPrimes.Int64Array1.free farr
  in
    output
  end


(* ==========================================================================
 * primes on cpu
 *)

fun primes_cpu n : Int64.int array =
  if n < 2 then
    ForkJoin.alloc 0
  else
    let
      val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
      val sqrtPrimes = primes_cpu sqrtN

      val flags = ForkJoin.alloc (n + 1) : Word8.word array
      fun mark i = Array.update (flags, i, 0w0)
      fun unmark i = Array.update (flags, i, 0w1)
      fun isMarked i =
        Array.sub (flags, i) = 0w0
      val _ = ForkJoin.parfor 10000 (0, n + 1) mark

      val blockSize = Int.max (sqrtN, 1000)
      val numBlocks = Util.ceilDiv (n + 1) blockSize

      val _ = ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n + 1)

          fun loop i =
            if i >= Array.length sqrtPrimes then
              ()
            else if 2 * Int64.toInt (Array.sub (sqrtPrimes, i)) >= hi then
              ()
            else
              let
                val p = Int64.toInt (Array.sub (sqrtPrimes, i))
                val lom = Int.max (2, Util.ceilDiv lo p)
                val him = Util.ceilDiv hi p
              in
                Util.for (lom, him) (fn m => unmark (m * p));
                loop (i + 1)
              end
        in
          loop 0
        end)
    in
      SeqBasis.filter 4096 (2, n + 1) (fn i => Int64.fromInt i) isMarked
    end


(* ===========================================================================
 * primes hybrid cpu+gpu
 *)

val hybrid_gpu_split = CLA.parseReal "hybrid-gpu-split" 0.2
val _ = print
  ("hybrid-gpu-split " ^ Real.toString hybrid_gpu_split
   ^ " (fraction of segments given to gpu choice points)\n")

fun calculateMid lo hi =
  lo + Real.ceil (Real.fromInt (hi - lo) * hybrid_gpu_split)


fun primes_hybrid n : Int64.int array =
  if n <= 1000 then
    primes_cpu n
  (* ForkJoin.choice
    {prefer_cpu = fn _ => primes_cpu n, prefer_gpu = fn _ => primes_gpu n} *)
  else
    let
      val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
      val sqrtPrimes = primes_hybrid sqrtN

      val flags = ForkJoin.alloc (n + 1) : Word8.word array
      fun mark i = Array.update (flags, i, 0w1)
      fun unmark i = Array.update (flags, i, 0w0)
      fun isMarked i =
        Array.sub (flags, i) = 0w1
      val _ = ForkJoin.parfor 10000 (0, n + 1) mark

      val blockSize = Int.max (sqrtN, 1000)
      val numBlocks = Util.ceilDiv (n + 1) blockSize

      fun doBlock b =
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n + 1)

          fun loop i =
            if i >= Array.length sqrtPrimes then
              ()
            else if 2 * Int64.toInt (Array.sub (sqrtPrimes, i)) >= hi then
              ()
            else
              let
                val p = Int64.toInt (Array.sub (sqrtPrimes, i))
                val lom = Int.max (2, Util.ceilDiv lo p)
                val him = Util.ceilDiv hi p
              in
                Util.for (lom, him) (fn m => unmark (m * p));
                loop (i + 1)
              end
        in
          loop 0
        end

      val sqrtPrimesOnGpu =
        FutharkPrimes.Int64Array1.new ctx sqrtPrimes (Array.length sqrtPrimes)

      fun blockRangeSize lob hib =
        Int.min (n + 1, hib * blockSize) - lob * blockSize

      fun doBlocksOnGpu lob hib =
        let
          val lo = lob * blockSize
          val hi = Int.min (n + 1, hib * blockSize)

          val gpuFlags =
            FutharkPrimes.Entry.sieve_segment ctx (sqrtPrimesOnGpu, lo, hi)
          val _ = FutharkPrimes.ctx_sync ctx
          val target = ArraySlice.slice (flags, lo, SOME (hi - lo))
        in
          FutharkPrimes.Word8Array1.values_into gpuFlags target;
          FutharkPrimes.Word8Array1.free gpuFlags
        end

      fun loop lob hib =
        if hib - lob = 0 then
          ()
        else if hib - lob = 1 then
          doBlock lob
        else
          let
            val midb = calculateMid lob hib
          in
            ForkJoin.par (fn _ => loopChoose lob midb, fn _ => loop midb hib);
            ()
          end

      and loopChoose lob hib =
        if blockRangeSize lob hib < 10000 then
          loop lob hib
        else
          ForkJoin.choice
            { prefer_cpu = fn _ => loop lob hib
            , prefer_gpu = fn _ => doBlocksOnGpu lob hib
            }
    in
      loop 0 numBlocks;
      FutharkPrimes.Int64Array1.free sqrtPrimesOnGpu;
      SeqBasis.filter 5000 (2, n + 1) (fn i => Int64.fromInt i) isMarked
    end


(* ===========================================================================
 * main
 *)

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val doit =
  case impl of
    "cpu" => primes_cpu
  | "gpu" => primes_gpu
  | "hybrid" => primes_hybrid
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("primes " ^ impl) (fn () => doit n)
val numPrimes = Array.length result
(* case result of
  Mono x => Int64Array.length x
| Poly x => Array.length x *)
val _ = print ("num primes " ^ Int.toString numPrimes ^ "\n")
