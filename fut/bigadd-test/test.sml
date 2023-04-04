type fut_ctx = MLton.Pointer.t
type fut_u8_1d = MLton.Pointer.t
val futInit = _import "futInit" public : unit -> fut_ctx;
val futFinish = _import "futFinish" public : fut_ctx -> unit;
(* val rawFutBigAdd =
  _import "futBigAdd" public : fut_ctx * Word8.word array * Word8.word array * Int64.int * Word8.word array -> unit;
val rawFutReadValuesAndFree =
  _import "futReadValuesAndFree" public : fut_ctx * fut_u8_1d * Word8.word array array -> unit; *)

type bigadd_package = MLton.Pointer.t

val rawFutBigAddSpawn =
  _import "futBigAddSpawn" public : fut_ctx * Word8.word array * Word8.word array * Int64.int -> bigadd_package;

val rawFutBigAddPoll =
  _import "futBigAddPoll" public : bigadd_package -> Word8.word;

val rawFutBigAddOutputSize =
  _import "futBigAddOutputSize" public : bigadd_package -> Int64.int;

val rawFutBigAddFinish =
  _import "futBigAddFinish" public : bigadd_package * Word8.word array -> unit;



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

val ctx = futInit ()

fun makeBigAddOnGpuTask input1 input2 =
  let
    (* val input1 = generate n seed *)
    val (a1, i1, n) = ArraySlice.base input1
    val _ = if i1 = 0 then () else raise Fail "uh oh"
    (* val input2 = generate n seed *)
    val (a2, i2, _) = ArraySlice.base input2
    val _ = if i2 = 0 then () else raise Fail "uh oh"

    fun spawn () = rawFutBigAddSpawn(ctx, a1, a2, n)

    fun poll pack = (0w1 = rawFutBigAddPoll pack)

    fun finish pack = 
      let
        val outputSize = rawFutBigAddOutputSize pack
        val output = ForkJoin.alloc outputSize
      in
        rawFutBigAddFinish (pack, output);
        Seq.take (ArraySlice.full output) outputSize
      end
  in
    ForkJoin.gpu {spawn = spawn, poll = poll, finish = finish}
  end

fun bigAddOnCpuBenchmark a b = 
  let
    val res = Add.add (a, b) 
  in
    res
  end

fun bigAddHybridBenchmark a b = 
  ForkJoin.choice {gpu = makeBigAddOnGpuTask a b, cpu = (fn () => bigAddOnCpuBenchmark a b) }

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
    "cpu" => bigAddOnCpuBenchmark
  (* | "gpu" => primesOnGpuBenchmark *)
  | "hybrid" => bigAddHybridBenchmark 
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "bignum" (fn () => SeqBasis.tabulate 1 (0, simultaneous ) (fn _ => bench input1 input2))
val result = Array.sub (result, 0)
val _ = print ("result " ^ Util.summarizeArraySlice 8 Word8.toString result ^ "\n") 
val _ = print ("result length " ^ Int.toString (Seq.length result) ^ "\n")
(* val result = bench {simultaneous = simultaneous, n = n}
val result0 = Array.sub (result, 0)
val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result0 ^ "\n") *)
(* val _ = print ("num primes " ^ Int.toString (Array.length result0) ^ "\n") *)
