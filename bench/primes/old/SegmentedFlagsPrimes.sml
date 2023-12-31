structure SegmentedFlagsPrimes =
struct


  (* ==========================================================================
   * primes on cpu
   *)

  fun primes_cpu n : Int64.int array =
    if n < 2 then
      ForkJoin.alloc 0
    else
      let
        val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
        val sqrtPrimes = primes_cpu sqrtN

        val flags = ForkJoin.alloc (n + 1) : Word8.word array
        fun mark i = Array.update (flags, i, 0w0)
        fun unmark i = Array.update (flags, i, 0w1)
        fun isMarked i =
          Array.sub (flags, i) = 0w0
        val _ = ForkJoin.parfor 10000 (0, n + 1) mark

        val blockSize = Int.max (sqrtN, 1000)
        val numBlocks = Util.ceilDiv (n + 1) blockSize

        val (_, tm) = Util.getTime (fn _ =>
          ForkJoin.parfor 1 (0, numBlocks) (fn b =>
            let
              val lo = b * blockSize
              val hi = Int.min (lo + blockSize, n + 1)

              fun loop i =
                if i >= Array.length sqrtPrimes then
                  ()
                else if 2 * Int64.toInt (Array.sub (sqrtPrimes, i)) >= hi then
                  ()
                else
                  let
                    val p = Int64.toInt (Array.sub (sqrtPrimes, i))
                    val lom = Int.max (2, Util.ceilDiv lo p)
                    val him = Util.ceilDiv hi p
                  in
                    Util.for (lom, him) (fn m => unmark (m * p));
                    loop (i + 1)
                  end
            in
              loop 0
            end))
        val _ = print
          ("sieve  (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
        val (result, tm) = Util.getTime (fn _ =>
          SeqBasis.filter 4000 (2, n + 1) (fn i => Int64.fromInt i) isMarked)
        val _ = print
          ("filter (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        result
      end


  (* ===========================================================================
   * primes hybrid cpu+gpu
   *)

  val hybrid_gpu_split = CommandLineArgs.parseReal "hybrid-gpu-split" 0.1
  val _ = print
    ("hybrid-gpu-split " ^ Real.toString hybrid_gpu_split
     ^ " (fraction of segments given to gpu choice points)\n")

  fun calculateMid lo hi =
    let val result = lo + Real.ceil (Real.fromInt (hi - lo) * hybrid_gpu_split)
    in if result = lo then lo + 1 else if result = hi then hi - 1 else result
    end


  fun primes_hybrid ctx n : Int64.int array =
    if n <= 10000 then
      primes_cpu n
    (* ForkJoin.choice
      {prefer_cpu = fn _ => primes_cpu n, prefer_gpu = fn _ => primes_gpu n} *)
    else
      let
        val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
        val sqrtPrimes = primes_hybrid ctx sqrtN

        val flags = ForkJoin.alloc (n + 1) : Word8.word array
        fun mark i = Array.update (flags, i, 0w1)
        fun unmark i = Array.update (flags, i, 0w0)
        fun isMarked i =
          Array.sub (flags, i) = 0w1
        val _ = ForkJoin.parfor 10000 (0, n + 1) mark

        val blockSize = Int.max (sqrtN, 1000)
        val numBlocks = Util.ceilDiv (n + 1) blockSize

        fun doBlock b =
          let
            val lo = b * blockSize
            val hi = Int.min (lo + blockSize, n + 1)

            fun loop i =
              if i >= Array.length sqrtPrimes then
                ()
              else if 2 * Int64.toInt (Array.sub (sqrtPrimes, i)) >= hi then
                ()
              else
                let
                  val p = Int64.toInt (Array.sub (sqrtPrimes, i))
                  val lom = Int.max (2, Util.ceilDiv lo p)
                  val him = Util.ceilDiv hi p
                in
                  Util.for (lom, him) (fn m => unmark (m * p));
                  loop (i + 1)
                end
          in
            loop 0
          end

        val (sqrtPrimesOnGpu, tm) = Util.getTime (fn _ =>
          FutharkPrimes.Int64Array1.new ctx (ArraySlice.full sqrtPrimes)
            (Array.length sqrtPrimes))
        val _ = print
          ("copy sqrtPrimes (n=" ^ Int.toString n ^ ") to gpu: " ^ Time.fmt 4 tm
           ^ "s\n")

        fun blockRangeSize lob hib =
          Int.min (n + 1, hib * blockSize) - lob * blockSize

        fun doBlocksOnGpu lob hib =
          let
            val lo = lob * blockSize
            val hi = Int.min (n + 1, hib * blockSize)

            val t0 = Time.now ()
            val gpuFlags =
              FutharkPrimes.Entry.sieve_segmented_segment ctx
                (sqrtPrimesOnGpu, blockSize, lo, hi)
            val _ = FutharkPrimes.Context.sync ctx
            val t1 = Time.now ()
            val _ = print
              ("gpu sieve (" ^ Int.toString (hi - lo) ^ "): "
               ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")
            val target = ArraySlice.slice (flags, lo, SOME (hi - lo))
          in
            let
              val (_, tm) = Util.getTime (fn _ =>
                FutharkPrimes.Word8Array1.values_into gpuFlags target)
            in
              print ("gpu copy back: " ^ Time.fmt 4 tm ^ "s\n")
            end;
            FutharkPrimes.Word8Array1.free gpuFlags
          end

        fun loop depth lob hib =
          if hib - lob = 0 then
            ()
          else if hib - lob = 1 then
            doBlock lob
          else
            let
              val midb = calculateMid lob hib
            in
              ForkJoin.par (fn _ => loopChoose (depth + 1) lob midb, fn _ =>
                loop (depth + 1) midb hib);
              ()
            end

        and loopChoose depth lob hib =
          if blockRangeSize lob hib < 100000 then
            ForkJoin.parfor 1 (lob, hib) doBlock
          else
            ForkJoin.choice
              { prefer_cpu = fn _ => loop depth lob hib
              , prefer_gpu = fn _ => doBlocksOnGpu lob hib
              }

        fun outerLoop depth lob hib =
          if depth >= 2 orelse hib - lob <= 1 then
            loop depth lob hib
          else
            let
              val midb = lob + (hib - lob) div 2
            in
              ForkJoin.par (fn _ => outerLoop (depth + 1) lob midb, fn _ =>
                outerLoop (depth + 1) midb hib);
              ()
            end

        val (_, tm) = Util.getTime (fn _ => outerLoop 0 0 numBlocks)
        val _ = print
          ("sieve (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        FutharkPrimes.Int64Array1.free sqrtPrimesOnGpu;

        let
          val (result, tm) = Util.getTime (fn _ =>
            SeqBasis.filter 5000 (2, n + 1) (fn i => Int64.fromInt i) isMarked)
        in
          print ("filter (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n");
          result
        end

      end

end
