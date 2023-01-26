val rawFutSieve =
  _import "futSieve" public : Int64.int * Int64.int array * Int64.int * bool array -> unit;

fun doSieveOnGpu n sqrtPrimes =
  let
    val numFlags = n + 1
    val flags: bool array = ForkJoin.alloc numFlags
    fun isMarked i = Array.sub (flags, i)
  in
    rawFutSieve (Array.length sqrtPrimes, sqrtPrimes, numFlags, flags);
    isMarked
  end


fun doSieveOnCpu n sqrtPrimes =
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


fun primes siever n =
  if n < 2 then
    ForkJoin.alloc 0
  else
    let
      (* all primes up to sqrt(n) *)
      val sqrtPrimes = primes siever (Real.floor
        (Real.Math.sqrt (Real.fromInt n)))
      val isMarked = siever n sqrtPrimes
      val result =
        (* for every i in 2 <= i <= n, filter those that are still marked *)
        SeqBasis.filter 4096 (2, n + 1) (fn i => i) isMarked
    in
      result
    end

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val siever =
  case impl of
    "cpu" => doSieveOnCpu
  | "gpu" => doSieveOnGpu
  (* | "hybrid" =>  *)
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("primes " ^ impl) (fn _ => primes siever n)
val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result ^ "\n")
