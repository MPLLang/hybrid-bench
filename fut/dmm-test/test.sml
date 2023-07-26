fun trace n prefix f =
  if true (*n < 10000*) then
    f ()
  else
    let
      val (result, tm) = Util.getTime f
    in
      print (prefix ^ " " ^ Time.fmt 4 tm ^ " (n = " ^ Int.toString n ^ ")\n");
      result
    end

val simultaneous = CommandLineArgs.parseInt "simultaneous" 20
val n = CommandLineArgs.parseInt "n" (10 * 1000 * 1000)
val seed = CommandLineArgs.parseInt "seed" (12342)
val impl = CommandLineArgs.parseString "impl" "hybrid"
val gpuThresh = CommandLineArgs.parseInt "gpu-thresh" 1000000

val input1 = HybridTreeMatrix.tabulate n (fn _ => 1.0)
val input2 = HybridTreeMatrix.tabulate n (fn _ => 2.0)

val _ = print ("simultaneous " ^ Int.toString simultaneous ^ "\n")
val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")
val _ = print ("gpu-thresh " ^ Int.toString gpuThresh ^ "\n")

val bench =
  case impl of
  (* "cpu" => TreeMatrix.dMMOnCpuBenchmark *)
  (* | "gpu" => primesOnGpuBenchmark *)
    "hybrid" => HybridTreeMatrix.multiply
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "dmm" (fn _ => bench (input1, input2))
val arr = HybridTreeMatrix.flatten result
val _ = print
  (Real32.toString (Array.sub (arr, 0)) ^ "\n") (* val _ = print ("result " ^ Util.summarizeArraySlice 8 Word8.toString result ^ "\n")  *) (* val _ = print ("result length " ^ Int.toString (Seq.length result) ^ "\n") *)
    (* val result = bench {simultaneous = simultaneous, n = n}
    val result0 = Array.sub (result, 0)
    val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result0 ^ "\n") *) (* val _ = print ("num primes " ^ Int.toString (Array.length result0) ^ "\n") *)
