structure SegmentedPrimes =
struct

  (* ==========================================================================
   * primes on gpu
   *)

  fun primes_gpu ctx n : Int64.int array =
    let
      val t0 = Time.now ()
      val farr = FutharkPrimes.Entry.primes ctx n
      val t1 = Time.now ()
      val output = FutharkPrimes.Int64Array1.values farr
      val _ = FutharkPrimes.Int64Array1.free farr
      val t2 = Time.now ()
    in
      print
        ("gpu primes (" ^ Int.toString n ^ "): " ^ Time.fmt 4 (Time.- (t1, t0))
         ^ "+" ^ Time.fmt 4 (Time.- (t2, t1)) ^ "s\n");
      output
    end
    handle FutharkPrimes.Error msg => Util.die ("Futhark error: " ^ msg)


  (* ==========================================================================
   * primes on cpu
   *)

  val blockSizeFactor = BenchParams.Primes.block_size_factor
  val _ = print
    ("primes-block-size-factor " ^ Real.toString blockSizeFactor ^ "\n")

  fun primes_cpu n : Int64.int array array =
    if n < 2 then
      ForkJoin.alloc 0
    else
      let
        val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
        val sqrtPrimes = primes_cpu sqrtN

        val sqrtPrimes = Seq.flatten
          (Seq.map ArraySlice.full (ArraySlice.full sqrtPrimes))

        (* Split the range [2,n+1) into blocks *)
        val blockSize = Real.ceil (Real.fromInt sqrtN * blockSizeFactor)
        val blockSize = Int.max (blockSize, 1000)
        val numBlocks = Util.ceilDiv ((n + 1) - 2) blockSize

        val (result, tm) = Util.getTime (fn _ =>
          SeqBasis.tabulate 1 (0, numBlocks) (fn b =>
            let
              val lo = 2 + b * blockSize
              val hi = Int.min (lo + blockSize, n + 1)

              val flags = Array.array (hi - lo, 0w1 : Word8.word)
              fun unmark i =
                Array.update (flags, i - lo, 0w0)

              fun loop i =
                if i >= Seq.length sqrtPrimes then
                  ()
                else if 2 * Int64.toInt (Seq.nth sqrtPrimes i) >= hi then
                  ()
                else
                  let
                    val p = Int64.toInt (Seq.nth sqrtPrimes i)
                    val lom = Int.max (2, Util.ceilDiv lo p)
                    val him = Util.ceilDiv hi p
                  in
                    Util.for (lom, him) (fn m => unmark (m * p));
                    loop (i + 1)
                  end

              val _ = loop 0

              val numPrimes = Util.loop (0, hi - lo) 0 (fn (count, i) =>
                if Array.sub (flags, i) = 0w0 then count else count + 1)

              val output = ForkJoin.alloc numPrimes

              val _ = Util.loop (lo, hi) 0 (fn (outi, i) =>
                if Array.sub (flags, i - lo) = 0w0 then outi
                else (Array.update (output, outi, Int64.fromInt i); outi + 1))
            in
              output
            end))

        val _ = print
          ("sieve (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        result
      end


  (* ==========================================================================
   * hybrid primes
   *)
  structure CtxSet = CtxSetFn (structure F = FutharkPrimes)

  val hybrid_gpu_split = BenchParams.Primes.hybrid_split
  val _ = print
    ("hybrid-gpu-split " ^ Real.toString hybrid_gpu_split
     ^ " (fraction of segments given to gpu choice points)\n")

  fun calculateMid lo hi =
    let val result = lo + Real.ceil (Real.fromInt (hi - lo) * hybrid_gpu_split)
    in if result = lo then lo + 1 else if result = hi then hi - 1 else result
    end

  structure FutharkPrimesData =
    GpuData
      (type t =
         (FutharkPrimes.Int64Array1.ctx * FutharkPrimes.Int64Array1.array))

  fun primes_hybrid ctxSet n : Int64.int array array =
    if n <= 100000 then
      primes_cpu n
    else
      let
        val sqrtN = Real.floor (Math.sqrt (Real.fromInt n))
        val sqrtPrimes = primes_hybrid ctxSet sqrtN

        val sqrtPrimes = Seq.flatten
          (Seq.map ArraySlice.full (ArraySlice.full sqrtPrimes))

        val (sqrtPrimesOnGpuSet, tm) = Util.getTime (fn _ =>
          FutharkPrimesData.initialize ctxSet (fn ctx =>
            FutharkPrimes.Int64Array1.new ctx sqrtPrimes
              (ArraySlice.length sqrtPrimes)))
        val _ = print
          ("copy sqrtPrimes to gpu (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm
           ^ "s\n")

        (* Split the range [2,n+1) into blocks *)
        val blockSize = Real.ceil (Real.fromInt sqrtN * blockSizeFactor)
        val blockSize = Int.max (blockSize, 1000)
        val total = (n + 1) - 2
        val numBlocks = Util.ceilDiv total blockSize
        fun blockRangeSize lob hib =
          Int.min (n + 1, hib * blockSize) - lob * blockSize
        val outputBlocks = SeqBasis.tabulate 1000 (0, numBlocks) (fn _ =>
          ForkJoin.alloc 0)

        val outerBlockSize = Real.ceil (hybrid_gpu_split * Real.fromInt total)

        fun doBlock b =
          let
            val lo = 2 + b * blockSize
            val hi = Int.min (lo + blockSize, n + 1)

            val flags = Array.array (hi - lo, 0w1 : Word8.word)
            fun unmark i =
              Array.update (flags, i - lo, 0w0)

            fun loop i =
              if i >= Seq.length sqrtPrimes then
                ()
              else if 2 * Int64.toInt (Seq.nth sqrtPrimes i) >= hi then
                ()
              else
                let
                  val p = Int64.toInt (Seq.nth sqrtPrimes i)
                  val lom = Int.max (2, Util.ceilDiv lo p)
                  val him = Util.ceilDiv hi p
                in
                  Util.for (lom, him) (fn m => unmark (m * p));
                  loop (i + 1)
                end

            val _ = loop 0

            val numPrimes = Util.loop (0, hi - lo) 0 (fn (count, i) =>
              if Array.sub (flags, i) = 0w0 then count else count + 1)

            val output = ForkJoin.alloc numPrimes

            val _ = Util.loop (lo, hi) 0 (fn (outi, i) =>
              if Array.sub (flags, i - lo) = 0w0 then outi
              else (Array.update (output, outi, Int64.fromInt i); outi + 1))
          in
            Array.update (outputBlocks, b, output)
          end

        fun doBlocksOnGpu device lob hib =
          let
            val lo = 2 + lob * blockSize
            val hi = Int.min (n + 1, lo + (hib - lob) * blockSize)
            val ctx = CtxSet.choose ctxSet device
            val sqrtPrimesOnGpu =
              FutharkPrimesData.choose sqrtPrimesOnGpuSet device

            val t0 = Time.now ()
            val gpuPrimes =
              FutharkPrimes.Entry.sieve_primes ctx (sqrtPrimesOnGpu, lo, hi)
            val t1 = Time.now ()
            val _ = Array.update
              (outputBlocks, lob, FutharkPrimes.Int64Array1.values gpuPrimes)
            val t2 = Time.now ()
          in
            print
              ("gpu sieve (" ^ Int.toString (hi - lo) ^ "): "
               ^ Time.fmt 4 (Time.- (t1, t0)) ^ "+"
               ^ Time.fmt 4 (Time.- (t2, t1)) ^ "s\n");
            FutharkPrimes.Int64Array1.free gpuPrimes
          end

        fun loop lob hib =
          if hib - lob = 0 then
            ()
          else if hib - lob = 1 then
            doBlock lob
          else
            let
              val midb = lob + (hib - lob) div 2
            in
              ForkJoin.par (fn _ => loopChoose lob midb, fn _ => loop midb hib);
              ()
            end

        and loopChoose lob hib =
          if
            blockRangeSize lob hib
            < BenchParams.Primes.block_range_hybrid_threshold
          then
            ForkJoin.parfor 1 (lob, hib) doBlock
          else
            ForkJoin.choice
              { prefer_cpu = fn _ => loop lob hib
              , prefer_gpu = fn device => doBlocksOnGpu device lob hib
              }

        fun outerLoop lob hib =
          if hib - lob <= 1 orelse blockRangeSize lob hib <= outerBlockSize then
            loopChoose lob hib
          else
            let
              val midb = lob + (hib - lob) div 2
            in
              ForkJoin.par (fn _ => outerLoop lob midb, fn _ =>
                outerLoop midb hib);
              ()
            end

        val (_, tm) = Util.getTime (fn _ => outerLoop 0 numBlocks)
        val _ =
          if n <= 100000 then
            ()
          else
            print ("sieve (n=" ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        FutharkPrimesData.free sqrtPrimesOnGpuSet (fn d =>
          FutharkPrimes.Int64Array1.free d);
        outputBlocks
      end

end
