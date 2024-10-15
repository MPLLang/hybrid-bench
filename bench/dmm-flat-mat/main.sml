val n = CommandLineArgs.parseInt "n" 8192
val impl = CommandLineArgs.parseString "impl" "hybrid"
val doPreallocate = CommandLineArgs.parseFlag "preallocate"
val doPrepopulate = CommandLineArgs.parseFlag "prepopulate"
val devices = ArraySlice.full (Array.fromList (String.fields (fn c => c = #",")
  (CommandLineArgs.parseString "devices" "")))

val () = Seq.foreach devices (fn (dev_id, gpu_id) =>
  let
    val init = _import "initialize_stream": Int64.int * string * Int64.int -> unit;
  in
    init(dev_id, gpu_id, String.size gpu_id)
  end)

val input1 = MatReal32.tabulate {width = n, height = n} (fn _ => 1.0)
val input2 = MatReal32.tabulate {width = n, height = n} (fn _ => 3.0)

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")
val _ = print ("preallocate? " ^ (if doPreallocate then "yes" else "no") ^ "\n")
val _ = print ("prepopulate? " ^ (if doPrepopulate then "yes" else "no") ^ "\n")

val _ =
  if doPreallocate andalso doPrepopulate then
    Util.die ("pass either --preallocate or --prepopulate but not both.\n\
              \(preallocate: exclude measurement of cudaMalloc for input matrices)\n\
              \(prepopulate: exclude measurement of cudaMalloc AND cudaMemcpy for input matrices)")
  else ()

(* val ptrs = ArraySlice.full (SeqBasis.tabulate 1 (0, Seq.length devices) (fn i =>
  let
    val gpu_id = Seq.nth devices i
    val len1 = Array.length (MatReal32.data input1)
    val len2 = Array.length (MatReal32.data input2)
    val d_input1 = allocDeviceMemory (gpu_id, String.size gpu_id, len1)
    val d_input2 = allocDeviceMemory (gpu_id, String.size gpu_id, len2)
  in
    (d_input1, d_input2)
  end)) *)

val gpu_input1 = 
  if doPreallocate then MatReal32.preallocate devices input1
  else if doPrepopulate then MatReal32.prepopulate devices input1
  else MatReal32.GpuNone

val gpu_input2 = 
  if doPreallocate then MatReal32.preallocate devices input2
  else if doPrepopulate then MatReal32.prepopulate devices input2
  else MatReal32.GpuNone


(* This is an attempt to avoid the performance instability of the initial
 * cudaMalloc+cudaMemcpy in the benchmark. It doesn't seem to be working,
 * though.
 *)
val tmp = MatReal32.tabulate {width = 1000, height = 1000} (fn _ => 1.0)
fun prep () =
  let
    val stuff = MatReal32.prepopulate devices tmp
    val stuff = MatReal32.ensurePopulated devices stuff tmp
  in
    MatReal32.freeFloatsGpuData stuff;
    MatReal32.syncGpus devices
  end

val bench =
  case impl of
    "cpu" => MatReal32.cpu_multiply_nonsquare
  (* | "cpu-pow2" => MatReal32.cpu_multiply *)
  | "gpu" => MatReal32.gpu_multiply devices (gpu_input1, gpu_input2)
  | "hybrid" => MatReal32.hybrid_multiply_nonsquare devices (gpu_input1, gpu_input2)
  (* | "hybrid-pow2" => MatReal32.hybrid_multiply devices *)
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run_with_prep "dmm" {prep = prep, bench = fn () => bench (input1, input2)}
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
