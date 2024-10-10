val n = CommandLineArgs.parseInt "n" 8192
val impl = CommandLineArgs.parseString "impl" "hybrid"
val devices = ArraySlice.full (Array.fromList (String.fields (fn c => c = #",")
  (CommandLineArgs.parseString "devices" "")))

val input1 = MatReal32.tabulate {width = n, height = n} (fn _ => 1.0)
val input2 = MatReal32.tabulate {width = n, height = n} (fn _ => 3.0)

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

fun initial_run () =
  let
    val _ = print
      "============== INITIAL WARMUP ==============\n\
      \(At least one full CPU run is needed to warm up the OpenBLAS\n\
      \memory allocator. Subsequent normal benchmarking warmup runs\n\
      \should then guarantee that everything is sufficiently warm...)\n"
    val (result, tm) = Util.getTime (fn _ =>
      MatReal32.cpu_multiply_nonsquare (input1, input2))
    val arr = MatReal32.data result
    val _ = print (Real32.toString (Array.sub (arr, 0)) ^ "\n")
    val _ = print ("warmup_time " ^ Time.fmt 4 tm ^ "s\n")
    val _ = print ("============================================\n")
  in
    ()
  end

val bench =
  case impl of
    "cpu" => MatReal32.cpu_multiply_nonsquare
  (* | "cpu-pow2" => MatReal32.cpu_multiply *)
  | "gpu" => MatReal32.gpu_multiply (Seq.first devices)
  | "hybrid" => MatReal32.hybrid_multiply_nonsquare devices
  (* | "hybrid-pow2" => MatReal32.hybrid_multiply devices *)
  | _ => Util.die ("unknown -impl " ^ impl)

val () = initial_run ()
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
