val n = CommandLineArgs.parseInt "n" (10 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "hybrid"

val input1 = HybridTreeMatrix.tabulate n (fn _ => 1.0)
val input2 = HybridTreeMatrix.tabulate n (fn _ => 2.0)

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val bench =
  case impl of
    "cpu" => HybridTreeMatrix.cpu_multiply
  | "gpu" => HybridTreeMatrix.gpu_multiply
  | "hybrid" => HybridTreeMatrix.multiply
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "dmm" (fn _ => bench (input1, input2))
val arr = HybridTreeMatrix.flatten result
val _ = print (Real32.toString (Array.sub (arr, 0)) ^ "\n")
