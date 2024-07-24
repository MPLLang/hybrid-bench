structure QuickhullCPU:
sig
  val hull_cpu: {vectorized: bool} -> FlatPointSeq.t -> Int32.int Seq.t
end =
struct

  structure AS = ArraySlice
  structure G = Geometry2D
  structure Tree = TreeSeq

  fun startTiming () = Time.now ()

  fun par4 (f1, f2, f3, f4) =
    let
      val ((r1, r2), (r3, r4)) =
        ForkJoin.par (fn _ => ForkJoin.par (f1, f2), fn _ => ForkJoin.par (f3, f4))
    in
      (r1, r2, r3, r4)
    end

  fun tick tm msg =
    let
      val tm' = Time.now ()
    in
      print ("tick: " ^ msg ^ ": " ^ Time.fmt 4 (Time.- (tm', tm)) ^ "s\n");
      tm'
    end

  fun dump_pct msg n m =
    print
      (msg ^ " "
       ^
       Real.fmt (StringCvt.FIX (SOME 1))
         (100.0 * Real.fromInt n / Real.fromInt m) ^ "%\n")

  (* ========================================================================
   * vectorized primitives
   *)

  type f64 = Real64.real
  type i64 = Int64.int
  type i32 = Int32.int

  (* int32_t max_dist_pt_8(double * pts_data, int32_t * idxs_data, int64_t idxs_lo, double lx, double ly, double rx, double ry) *)
  val max_dist_pt_8 =
    _import "max_dist_pt_8": f64 array * i32 array * i64 * f64 * f64 * f64 * f64 -> i64;

  fun strip s =
    let
      val (arr, i, _) = ArraySlice.base s
    in
      if i = 0 then arr
      else raise Fail "QuickhullCPU.strip: strip of slice"
    end

  fun maxDistPtVectorized8 pts (idxs, lo, hi) (lp, rp) =
    let
      fun dist p q i =
        G.Point.triArea (p, q, FlatPointSeq.nth pts (Int32.toInt i))
      fun d i = dist lp rp i
      fun max ((i, di), (j, dj)) =
        if di > dj then (i, di) else (j, dj)
    in
      if hi-lo = 8 then
        let
          val i = max_dist_pt_8 (strip pts, strip idxs, lo, #1 lp, #2 lp, #1 rp, #2 rp)
          val i = Seq.nth idxs (lo + i)
        in
          (i, d i)
        end
      else
        SeqBasis.foldl max (~1, Real.negInf) (lo, hi)
          (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))
    end

  (* ========================================================================
   * cpu-only code
   *)

  fun hull_cpu {vectorized: bool} pts =
    let
      fun pt i =
        FlatPointSeq.nth pts (Int32.toInt i)
      fun dist p q i =
        G.Point.triArea (p, q, pt i)
      fun max ((i, di), (j, dj)) =
        if di > dj then (i, di) else (j, dj)
      fun x i =
        #1 (pt i)
      fun min_by f x y =
        if f x < f y then x else y
      fun max_by f x y =
        if f x > f y then x else y


      fun minmax ((l1, r1), (l2, r2)) =
        (min_by x l1 l2, max_by x r1 r2)


      fun aboveLine p q i =
        (dist p q i > 0.0)

      fun semihull (idxs: Int32.int Seq.t) l r =
        if Seq.length idxs < 2 then
          Tree.fromArraySeq idxs
        else
          let
            val lp = pt l
            val rp = pt r
            fun d i =
              dist lp rp i

            val (mid, _) =
              if not vectorized then
                SeqBasis.reduce 5000 max (~1, Real.negInf) (0, Seq.length idxs)
                  (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))
              else
                let
                  (* warning: cannot change the block size without also changing
                   * the corresponding ispc code
                   *)
                  val blockSize = 8

                  val numBlocks = Util.ceilDiv (Seq.length idxs) blockSize
                in
                  SeqBasis.reduce 200 max (~1, Real.negInf) (0, numBlocks) (fn b =>
                    let
                      val lo = b*blockSize
                      val hi = Int.min (Seq.length idxs, lo+blockSize)
                    in
                      maxDistPtVectorized8 pts (idxs, lo, hi) (lp, rp)
                    end)
                end

            val midp = pt mid

            fun flag i =
              if aboveLine lp midp i then 0w0
              else if aboveLine midp rp i then 0w1
              else 0w2

            val flags = Seq.map flag idxs

            (* TODO: STILL NEEDS VECTORIZED *)
            val (left, right) =
              ForkJoin.par
                ( fn _ =>
                    ArraySlice.full
                      (SeqBasis.filter 2000 (0, Seq.length idxs) (Seq.nth idxs)
                         (fn i => Seq.nth flags i = 0w0))
                , fn _ =>
                    ArraySlice.full
                      (SeqBasis.filter 2000 (0, Seq.length idxs) (Seq.nth idxs)
                         (fn i => Seq.nth flags i = 0w1))
                )

            fun doLeft () =
              semihull left l mid
            fun doRight () =
              semihull right mid r
            val (leftHull, rightHull) =
              if Seq.length left + Seq.length right <= 1000 then
                (doLeft (), doRight ())
              else
                ForkJoin.par (doLeft, doRight)
          in
            Tree.append (leftHull, (Tree.append (Tree.$ mid, rightHull)))
          end

      val tm = startTiming ()

      (* TODO: STILL NEEDS VECTORIZED *)
      val (l, r) =
        SeqBasis.reduce 5000 minmax (0, 0) (0, FlatPointSeq.length pts) (fn i =>
          (Int32.fromInt i, Int32.fromInt i))

      val tm = tick tm "endpoints"

      val lp = pt l
      val rp = pt r

      val flags =
        Seq.tabulate
          (fn i =>
             let
               val d = dist lp rp (Int32.fromInt i)
             in
               if d > 0.0 then 0w0 : Word8.word
               else if d < 0.0 then 0w1
               else 0w2
             end) (FlatPointSeq.length pts)

      val tm = tick tm "above/below flags"

      (* TODO: STILL NEEDS VECTORIZED *)
      val (above, below) =
        ForkJoin.par
          ( fn _ =>
              ArraySlice.full
                (SeqBasis.filter 2000 (0, FlatPointSeq.length pts)
                   (fn i => Int32.fromInt i) (fn i => Seq.nth flags i = 0w0))
          , fn _ =>
              ArraySlice.full
                (SeqBasis.filter 2000 (0, FlatPointSeq.length pts)
                   (fn i => Int32.fromInt i) (fn i => Seq.nth flags i = 0w1))
          )

      val _ = dump_pct "above" (Seq.length above) (FlatPointSeq.length pts)
      val _ = dump_pct "below" (Seq.length below) (FlatPointSeq.length pts)

      val tm = tick tm "above/below filter"

      val (above, below) = ForkJoin.par (fn _ => semihull above l r, fn _ =>
        semihull below r l)

      val tm = tick tm "quickhull"

      val hullt = Tree.append
        (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below))

      val result = Tree.toArraySeq hullt

      val tm = tick tm "flatten"
    in
      result
    end

end
