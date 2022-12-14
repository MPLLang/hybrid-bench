(*structure CPUMandel = 
struct
  
  fun mandel c_re c_im count = 
    let
      fun mandel' i z_re z_im = 
        if z_re * z_re + z_im * z_im > 4.0 then i else
        if i = count then i else
        let
          val new_re = z_re * z_re - z_im * z_im
          val new_im = 2.0 * z_re * z_im
          val z_re' = c_re + new_re
          val z_im' = c_im + new_im
        in
          mandel' (i+1) z_re' z_im'
        end
    in
      mandel' 0 c_re c_im
    end

  fun runMandel () = 
    let
      val (x0, x1, y0, y1) = (~2.0, 1.0, ~1.0, 1.0)
      val (width, height) = (1200.0, 800.0)
      val (dx, dy) = ((x1 - x0) / width, (y1 - y0) / height)
      val maxIter = 256
      fun loop i = 
        let
          val x = x0 + Real.fromInt(i mod Real.floor width) * dx
          val y = y0 + Real.fromInt(i div Real.floor width) * dy
        in
          mandel x y maxIter
        end
    in
      Array.tabulate(Real.floor width * Real.floor height, loop)
    end

end*)

(*structure GPUMandel = 
struct
  fun runMandel () = 
    let
      val (x0, x1, y0, y1) = (~2.0, 1.0, ~1.0, 1.0)
      val (width, height) = (1200.0, 800.0)
      val (dx, dy) = ((x1 - x0) / width, (y1 - y0) / height)
      val maxIter = 256
      val gpuarr = 
        GPUArray.init (Real.floor width * Real.floor height) (CTYPES.CINT)
      val _ = GPUKernels.mandel_gpu
              (GPUArray.getDevicePtr gpuarr, maxIter, Real.floor width, 
               Real.floor height, dx, dy, x0, y0)
    in
      GPUArray.toIntArray gpuarr
    end

end*)

structure GPUBLAS = 
struct
  fun runGEMM () = 
    let
      val n = 1024
      val gpuarr1 = 
        Array.array (n, 0)
      val gpuarr2 = 
        Array.array (n, 0)
      val gpuarr3 = 
        Array.array (n, 0)
      val _ = GPUKernels.chooseSGEMMInt
              (gpuarr1, gpuarr2, gpuarr3, n,n, n)
    in
      gpuarr3
    end
end

fun mplGEMM () =
  let
    val n = 1024
    val m1 = TreeMatrix.tabulate n (fn _ => 1.0)
    val m2 = TreeMatrix.tabulate n (fn _ => 2.0)
  in
    TreeMatrix.multiply (m1, m2)
  end

fun together () = ForkJoin.par(mplGEMM, GPUBLAS.runGEMM)


(*
fun twoWayReduction () =
  let
    val n = CommandLineArgs.parseInt "n" 10000000
    val arr = Array.array(n, 1)
    val lock = SpinLock.new ()
    fun onGPU (arr1, lo, hi) =
      GPUKernels.reduction (arr, lo, hi)
    fun maybeGPU (arr1, lo, hi) = 
      if (hi - lo) >= 100000 andalso SpinLock.trylock lock then 
        let
          val result = onGPU(arr1, lo, hi)

          val _ = SpinLock.unlock lock
        in
          result
        end
      else 
        wtr (arr1, lo, hi)
     and wtr (arr1, lo, hi) =
        if (hi - lo) = 0 then
          0
        else if hi - lo < 10000 then
          let
          (* Array.sub (arr1, lo) *)
          (* val _ = print ("on cpu" ^ "\n") *)
          in
            SeqBasis.foldl op+ 0 (lo, hi) (fn i => Array.sub (arr1, i))
          (*Util.loop (lo, hi) 0 (fn(i,j) => i + Array.sub (arr1, j))*)
          end
        else
          let 
            val mid = lo + (hi-lo) div 2
            val (left, right) =
              ForkJoin.par (fn() => wtr (arr1, lo, mid), 
                            fn() => maybeGPU (arr1, mid, hi))
          in
            left + right
          end 

      (*
      val result = onGPU(arr, 0, n div 4)
      val result1 = onGPU(arr, n div 4,  n div 2)
      val result2 = onGPU(arr,  n div 2,  3 * (n div 4))
      val result3 = onGPU(arr, 3 * (n div 4), n)
      val result = result + result1 + result2 + result3
       *)
  in
    wtr(arr, 0, n)
  end
*)

fun reduceOnGPU (arr, lo, hi) =
  let
    val result = GPUKernels.reduction (arr, lo, hi)
  in
    print ("|onGPU\n"
           ^ "|  lo = " ^ Int.toString lo ^ "\n"
           ^ "|  hi-lo = " ^ Int.toString (hi-lo) ^ "\n"
           ^ "|  result = " ^ Int.toString result ^ "\n");
    result
  end

fun plusReduceOnlyCPU arr =
  SeqBasis.reduce 10000 op+ 0 (0, Array.length arr) (fn i => Array.sub (arr, i))


fun plusReduceOnlyGPU arr =
  reduceOnGPU (arr, 0, Array.length arr)

val splitFraction = CommandLineArgs.parseReal "split" 0.66
val _ = print ("split " ^ Real64.toString splitFraction ^ "\n")

fun plusReduceHybrid arr =
  let
    val n = Array.length arr
    val lock = SpinLock.new ()
    fun maybeGPU (lo, hi) = 
      if (hi - lo) >= 100000 andalso SpinLock.trylock lock then 
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
          (* Array.sub (arr1, lo) *)
          (* val _ = print ("on cpu" ^ "\n") *)
          in
            SeqBasis.foldl op+ 0 (lo, hi) (fn i => Array.sub (arr, i))
          (*Util.loop (lo, hi) 0 (fn(i,j) => i + Array.sub (arr1, j))*)
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
    wtr(0, n)
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

val (input, tm) = Util.getTime (fn _ => SeqBasis.tabulate 10000 (0, n) (fn i => 1))
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run ("reduce " ^ impl) (fn _ =>
  ( print "\n+---------------------------\n"
  ; reduce input
  )
)
val _ = print ("result " ^ Int.toString result ^ "\n")


(*
structure Main = 
struct
  fun run () = 
    let
      (*val (res1, time) = Timer.run CPUMandel.runMandel
      val _ = print("SML time " ^ time ^ "\n")*)
      (* val _ = print "============ GPU ==============\n"
      val (res2, time) = Timer.run GPUBLAS.runGEMM
     (* val _ = print("result: " ^ res2 ^ "\n")*)
      val _ = print("SMLGPU time " ^ time ^ "\n")

      val _ = print "\n============ MPL CPU ==============\n"
      val (res3, time) = Timer.run mplGEMM
      val _ = print("MPL CPU " ^ time ^ "\n")

      val _ = print "\n============ BOTH ==============\n"
      val (res4, time) = Timer.run together
      val _ = print("both in parallel " ^ time ^ "\n") *)

      val _ = print "\n============ multitask ==============\n"
      val (res4, time) = Timer.run twoWayReduction
      val _ = print("MPL result " ^ Int.toString(res4) ^ "\n")
      val _ = print("Execution Time " ^ time ^ "\n")

      (*
      val bools = List.tabulate
          (Array.length res1, fn i => if Array.sub(res1, i) = Array.sub(res2,i)
                                      then true else
                                        ((*print(Int.toString(Array.sub(res1, i))
                                        ^ " " ^ Int.toString(Array.sub(res2, i))
                                        ^ " " ^ Int.toString(i) ^"\n");*)false))
      val true = List.all (fn x => x) (bools)*)
    in
      ()
    end
end

val _ = Main.run()
*)
