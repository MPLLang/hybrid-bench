structure CLA = CommandLineArgs

val ctx = FutharkPrimes.Context.new FutharkPrimes.default_cfg

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

fun singleton x = Array.fromList [x]

val doit =
  case impl of
    "cpu" => SegmentedPrimes.primes_cpu
  | "cpu-alternate" => singleton o SegmentedFlagsPrimes.primes_cpu
  | "gpu" => singleton o SegmentedFlagsPrimes.primes_gpu ctx
  | "hybrid" => singleton o SegmentedFlagsPrimes.primes_hybrid ctx
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("primes " ^ impl) (fn () => doit n)

val numPrimes = SeqBasis.reduce 1000 (op+) 0 (0, Array.length result) (fn i =>
  Array.length (Array.sub (result, i)))
val _ = print ("num primes " ^ Int.toString numPrimes ^ "\n")
