structure CLA = CommandLineArgs

val n = CLA.parseInt "n" (100 * 1000 * 1000)
val impl = CLA.parseString "impl" "hybrid"
val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val () = print "Initialising Futhark context... "
val ctx = FutharkSort.Context.new
  (FutharkSort.Config.cache (SOME "futhark.cache") FutharkSort.Config.default)
val () = print "Done!\n"

val _ = print ("generating " ^ Int.toString n ^ " random integers\n")
fun elem i =
  Int32.fromLarge (Word64.toLargeInt (Word64.mod
    (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))
val input = Seq.tabulate elem n

val bench =
  case impl of
    "hybrid" => (fn () => HybridSort.sort ctx input)
  | "cpu" => (fn () => HybridSort.sort_cpu input)
  | "gpu" => (fn () => HybridSort.sort_gpu ctx input)
  | _ => Util.die ("unknown impl: " ^ impl)

val result = Benchmark.run ("sort " ^ impl) bench

val _ = print
  ("input " ^ Util.summarizeArraySlice 8 Int32.toString input ^ "\n")
val _ = print
  ("result " ^ Util.summarizeArraySlice 8 Int32.toString result ^ "\n")

val _ = FutharkSort.Context.free ctx
