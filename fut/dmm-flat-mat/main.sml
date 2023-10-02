val n = CommandLineArgs.parseInt "n" 8192
val impl = CommandLineArgs.parseString "impl" "hybrid"

val input1 = MatReal32.tabulate {width = n, height = n} (fn _ => 1.0)
val input2 = MatReal32.tabulate {width = n, height = n} (fn _ => 3.0)

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val bench =
  case impl of
    "cpu" => MatReal32.cpu_multiply_nonsquare
  (* | "cpu-nonsquare" => MatReal32.cpu_multiply_nonsquare *)
  | "gpu" => MatReal32.gpu_multiply
  | "hybrid" => MatReal32.hybrid_multiply_nonsquare
  (* | "hybrid-nonsquare" => MatReal32.hybrid_multiply_nonsquare *)
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "dmm" (fn _ => bench (input1, input2))
val arr = MatReal32.data result
val _ = print (Real32.toString (Array.sub (arr, 0)) ^ "\n")


fun closeEnough (a, b) =
  Real32.abs (a - b) <= 0.0000001


val error = FindFirst.findFirst 1000 (0, n * n) (fn i =>
  not (closeEnough (Array.sub (arr, i), Real32.fromInt n * 3.0)))

val _ =
  case error of
    NONE => print ("correct? yes\n")
  | SOME i =>
      print
        ("correct? NO!\nfound error: output[" ^ Int.toString i ^ "] = "
         ^ Real32.toString (Array.sub (arr, i)) ^ "\n")

val _ =
  if n > 20 then
    ()
  else
    Util.for (0, n) (fn i =>
      ( Util.for (0, n) (fn j =>
          (print (Real32.toString (Array.sub (arr, i * n + j))); print " "))
      ; print "\n"
      ))
