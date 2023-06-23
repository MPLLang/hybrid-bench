val cublasSGEMM =
  _import "cublasSGEMM" public : MLton.Pointer.t * MLton.Pointer.t * MLton.Pointer.t * int * int * int -> unit;

(* generate n random floating point numbers (where the matrix is sqrt n * sqrt n) *)
fun generate n seed =
    let
      fun hash seed = Util.hash32_2 (Word32.fromInt seed)
      fun w32to8 w = Word8.fromInt (Word32.toInt (Word32.andb (w, 0wx7F)))
      fun genByte seed = w32to8 (hash seed)
      fun genNonZeroByte seed =
        w32to8 (Word32.+ (0w1, Word32.mod (hash seed, 0w127)))
    in
      Seq.tabulate (fn i =>
        if i < n-1 then
          genByte (seed+i)
        else
          genNonZeroByte (seed+i))
      n
  end

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

val input1 = generate n seed
val input2 = generate n seed

val _ = print ("simultaneous " ^ Int.toString simultaneous ^ "\n")
val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")
val _ = print ("gpu-thresh " ^ Int.toString gpuThresh ^ "\n")

val bench =
  case impl of
    "cpu" => dMMOnCpuBenchmark
  (* | "gpu" => primesOnGpuBenchmark *)
  | "hybrid" => dMMHybridBenchmark 
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "dmm" (fn () => SeqBasis.tabulate 1 (0, simultaneous ) (fn _ => bench input1 input2))
val result = Array.sub (result, 0)
val _ = print ("result " ^ Util.summarizeArraySlice 8 Word8.toString result ^ "\n") 
val _ = print ("result length " ^ Int.toString (Seq.length result) ^ "\n")
(* val result = bench {simultaneous = simultaneous, n = n}
val result0 = Array.sub (result, 0)
val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result0 ^ "\n") *)
(* val _ = print ("num primes " ^ Int.toString (Array.length result0) ^ "\n") *)
