structure CLA = CommandLineArgs

val n = CLA.parseInt "n" (100 * 1000 * 1000)
val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("generating " ^ Int.toString n ^ " random integers\n")
fun elem i =
  Int64.fromLarge (Word64.toLargeInt (Word64.mod
    (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))
val input = Seq.tabulate elem n

val ctx = FutSort.init ()

val result = Benchmark.run "hybrid mergesort" (fn _ =>
  HybridSort.sort ctx input)

val _ = FutSort.cleanup ctx

val _ = print ("input " ^ Util.summarizeArraySlice 8 Int.toString input ^ "\n")
val _ = print
  ("result " ^ Util.summarizeArraySlice 8 Int.toString result ^ "\n")
