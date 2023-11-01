structure CLA = CommandLineArgs

val () = print "Initialising Futhark context... "
val ctx = FutharkPrimes.Context.new
  (FutharkPrimes.Config.cache (SOME "futhark.cache")
     FutharkPrimes.Config.default)
val () = print "Done!\n"

val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "cpu"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

fun singleton x = Array.fromList [x]

val doit =
  case impl of
    "cpu" => SegmentedPrimes.primes_cpu
  | "gpu" => singleton o SegmentedPrimes.primes_gpu ctx
  | "hybrid" => SegmentedPrimes.primes_hybrid ctx
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("primes " ^ impl) (fn () => doit n)

val numPrimes = SeqBasis.reduce 1000 (op+) 0 (0, Array.length result) (fn i =>
  Array.length (Array.sub (result, i)))
val _ = print ("num primes " ^ Int.toString numPrimes ^ "\n")

val _ = FutharkPrimes.Context.free ctx
