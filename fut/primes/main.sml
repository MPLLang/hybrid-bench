structure CLA = CommandLineArgs

val ctx = FutharkPrimes.ctx_new FutharkPrimes.default_cfg

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

(* ==========================================================================
 * primes on gpu
 *)


fun primes_gpu n : Int64Array.array =
  let
    val (farr, count) = FutharkPrimes.Entry.primes ctx n
    val _ = FutharkPrimes.ctx_sync ctx
    val output = FutharkPrimes.Int64Array1.values farr
    val _ = FutharkPrimes.Int64Array1.free farr
  in
    output
  end


(* ===========================================================================
 * main
 *)

datatype i64_mono_poly =
  Mono of Int64Array.array
| Poly of Int64.int array

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val doit =
  case impl of
    "cpu" => Poly o primes_cpu
  | "gpu" => Mono o primes_gpu
  (* | "hybrid" => primesHybridBenchmark gpuThresh *)
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("primes " ^ impl) (fn () => doit n)
val numPrimes =
  case result of
    Mono x => Int64Array.length x
  | Poly x => Array.length x
val _ = print ("num primes " ^ Int.toString numPrimes ^ "\n")
