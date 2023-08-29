val n = CommandLineArgs.parseInt "n" 8192
val impl = CommandLineArgs.parseString "impl" "hybrid"

val input1 = MatReal32.tabulate {width = n, height = n} (fn _ => 1.0)
val input2 = MatReal32.tabulate {width = n, height = n} (fn _ => 3.0)

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val bench =
  case impl of
    "cpu" => MatReal32.cpu_multiply
  | "gpu" => MatReal32.gpu_multiply
  | "hybrid" => MatReal32.hybrid_multiply
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "dmm" (fn _ => bench (input1, input2))
val arr = MatReal32.data result
val _ = print (Real32.toString (Array.sub (arr, 0)) ^ "\n")

val allCorrect =
  SeqBasis.reduce 10000 (fn (a, b) => a andalso b) true (0, n * n) (fn i =>
    Real32.== (Array.sub (arr, i), Real32.fromInt n * 3.0))

val _ = print ("correct? " ^ (if allCorrect then "yes" else "no") ^ "\n")
