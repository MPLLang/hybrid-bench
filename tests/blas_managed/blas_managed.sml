(* same array exists in both CPU and GPU memory *)
structure DuplicatedInt32Array =
struct
  datatype t = A of {onCpu: Int32.int array, onGpu: MLton.Pointer.t}

  fun tabulate grain f n =
    let
      val onCpu = SeqBasis.tabulate grain (0, n) f
      val onGpu = GPUKernels.allocCudaManagedMemory (4 * Int64.fromInt n)
    in
      ForkJoin.parfor 10000 (0, n) (fn i =>
        MLton.Pointer.setInt32 (onGpu, i, Array.sub (onCpu, i))
      );

      A {onCpu=onCpu, onGpu=onGpu}
    end

  fun length (A {onCpu, ...}) = Array.length onCpu

  fun free (A {onGpu, ...}) =
    GPUKernels.freeCudaMemory onGpu

  fun onCpu (A {onCpu=c, ...}) = c
  fun onGpu (A {onGpu=g, ...}) = g
end


fun reduceOnGPU (arr, lo, hi) =
  let
    val arr = DuplicatedInt32Array.onGpu arr
    val result = GPUKernels.reductionManaged (arr, lo, hi)
  in
    (* print ("|onGPU size " ^ Int.toString (hi-lo) ^ "\n"); *)
    result
  end


fun plusReduceOnlyCPU arr =
  let
    val arr = DuplicatedInt32Array.onCpu arr
  in
    SeqBasis.reduce 10000 op+ 0 (0, Array.length arr) (fn i =>
      Int32.toInt (Array.sub (arr, i)))
  end


fun plusReduceOnlyGPU arr =
  reduceOnGPU (arr, 0, DuplicatedInt32Array.length arr)

val splitFraction = CommandLineArgs.parseReal "split" 0.2
val minGPU = CommandLineArgs.parseInt "min-gpu" (1000 * 1000)
val _ = print ("split " ^ Real64.toString splitFraction ^ "\n")
val _ = print ("min-gpu " ^ Int.toString minGPU ^ "\n")

fun plusReduceHybrid arr =
  let
    val lock = SpinLock.new ()
    fun maybeGPU (lo, hi) = 
      if (hi-lo) >= minGPU andalso SpinLock.trylock lock then 
        let
          val result = reduceOnGPU(arr, lo, hi)

          val _ = SpinLock.unlock lock
        in
          result
        end
      else
        wtr (lo, hi)

     and wtr (lo, hi) =
        if (hi - lo) = 0 then
          0
        else if hi - lo < 10000 then
          let
            val arr = DuplicatedInt32Array.onCpu arr
          in
            SeqBasis.foldl op+ 0 (lo, hi) (fn i => Int32.toInt (Array.sub (arr, i)))
          end
        else
          let 
            val mid = lo + Real64.round (splitFraction * Real64.fromInt (hi-lo))
            val (left, right) =
              ForkJoin.par (fn() => wtr (lo, mid),
                            fn() => maybeGPU (mid, hi))
          in
            left + right
          end 
  in
    wtr(0, DuplicatedInt32Array.length arr)
  end


val n = CommandLineArgs.parseInt "n" (100 * 1000 * 1000)
val impl = CommandLineArgs.parseString "impl" "hybrid"

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

val reduce =
  case impl of
    "cpu" => plusReduceOnlyCPU
  | "gpu" => plusReduceOnlyGPU
  | "hybrid" => plusReduceHybrid
  | _ => Util.die ("unknown -impl " ^ impl)

fun genInput () =
  let
    (* need 4n bytes to store n 32-bit integers *)
    val arr = GPUKernels.allocCudaManagedMemory (4 * Int64.fromInt n)
  in
    ForkJoin.parfor 10000 (0, n) (fn i =>
      MLton.Pointer.setInt32 (arr, i, Int32.fromInt 1)
    );
    
    (arr, n)
  end

val (input, tm) = Util.getTime (fn _ => DuplicatedInt32Array.tabulate 1000 (fn i => 1) n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run ("reduce " ^ impl) (fn _ => reduce input)
val _ = DuplicatedInt32Array.free input
val _ = print ("result " ^ Int.toString result ^ "\n")