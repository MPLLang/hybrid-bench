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


fun twoWayReduction () =
  let
    val n = CommandLineArgs.parseInt "n" 10000000
    val arr = Array.array(n, 1)
    val lock = SpinLock.new ()
    fun onCPU (lo, hi) =
      SeqBasis.reduce 1000 op+ 0 (lo, hi ) (fn i => Array.sub (arr, i))
    fun onGPU (arr1, size) =
      GPUKernels.test_cuda (arr1, size)
    fun wtr (lo, hi) =
      if SpinLock.trylock lock then 
        let
          val arr' = Array.tabulate ((hi-lo), (fn i => (Array.sub (arr, i+lo)))) 
          (* val _ = SpinLock.lock lock *)
          val result = onGPU(arr', hi-lo)
          (* val _ = SpinLock.unlock lock *)
        in
          result
        end
      else 
        let 
        val _ = print ("on cpu" ^ "\n")
        val result = onCPU(lo, hi)
        in
          result
        end
    
    val(left, right) = ForkJoin.par(fn() => wtr(0, Array.length arr div 2), 
                        fn() => wtr(Array.length arr div 2, Array.length arr))
     (* val(left, right) = ForkJoin.par(fn() => onCPU(0, Array.length arr div 2), 
                        fn() =>  let
          val arr' = Array.tabulate (((Array.length arr) - (Array.length arr div 2)), (fn i => (Array.sub (arr, i+(Array.length arr div 2))))) 
        in
          onGPU(arr', ((Array.length arr) - (Array.length arr div 2)))
        end) *)
    val res = left + right
  in
    res
  end


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
