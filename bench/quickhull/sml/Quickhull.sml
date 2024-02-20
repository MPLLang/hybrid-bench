structure Quickhull:
sig

  val hull_cpu: FlatPointSeq.t -> Int32.int Seq.t

  val hull_gpu: Futhark.ctx -> Futhark.Real64Array2.array -> Int32.int Seq.t

  val hull_hybrid: Futhark.ctx
                   -> FlatPointSeq.t * Futhark.Real64Array2.array
                   -> Int32.int Seq.t
end =
struct

  structure AS = ArraySlice
  structure G = Geometry2D
  structure Tree = TreeSeq

  fun startTiming () = Time.now ()

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
   * cpu-only code
   *)

  fun hull_cpu pts =
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
              SeqBasis.reduce 5000 max (~1, Real.negInf) (0, Seq.length idxs)
                (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))

            val midp = pt mid

            fun flag i =
              if aboveLine lp midp i then 0w0
              else if aboveLine midp rp i then 0w1
              else 0w2

            val flags = Seq.map flag idxs

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


  (* ========================================================================
   * gpu code
   *)


  fun top_level_points_above_in_range_gpu ctx (points_fut, lo, hi, l, r) =
    let
      val res_fut = Futhark.Entry.top_level_points_above_in_range ctx
        (points_fut, lo, hi, l, r)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
    in
      ArraySlice.full res
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun points_above_gpu ctx (points_fut, idxs, l, r) =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val res_fut = Futhark.Entry.points_above ctx (points_fut, idxs_fut, l, r)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
    in
      ArraySlice.full res
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun top_level_filter_then_semihull_gpu ctx (points_fut, l, r) =
    let
      val res_fut =
        Futhark.Entry.top_level_filter_then_semihull ctx (points_fut, l, r)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
    in
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun filter_then_semihull_gpu ctx (points_fut, l, r, idxs) =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val res_fut =
        Futhark.Entry.filter_then_semihull ctx (points_fut, l, r, idxs_fut)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
    in
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun semihull_gpu ctx (points_fut, idxs, l, r) =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val res_fut = Futhark.Entry.semihull ctx (points_fut, l, r, idxs_fut)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
    in
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun min_max_point_in_range_gpu ctx (points_fut, lo, hi) =
    let
      val (min, max) =
        Futhark.Entry.min_max_point_in_range ctx
          (points_fut, Int64.fromInt lo, Int64.fromInt hi)
    in
      (min, max)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun point_furthest_from_line_gpu ctx (points_fut, l, r, idxs) : Int32.int =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val i =
        Futhark.Entry.point_furthest_from_line ctx (points_fut, l, r, idxs_fut)
      val () = Futhark.Int32Array1.free idxs_fut
    in
      i
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun hull_gpu ctx points_fut =
    let
      val res_fut = Futhark.Entry.quickhull ctx points_fut
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
    in
      Seq.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  (* ========================================================================
   * hybrid hull
   *)


  val semihull_par_grain = BenchParams.Quickhull.semihull_par_grain
  val semihull_hybrid_grain = BenchParams.Quickhull.semihull_hybrid_grain
  val reduce_hybrid_grain = BenchParams.Quickhull.reduce_hybrid_grain
  val reduce_hybrid_split = BenchParams.Quickhull.reduce_hybrid_split


  fun hull_hybrid ctx (pts, points_fut) =
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
              HybridBasis.reduce_hybrid reduce_hybrid_split reduce_hybrid_grain
                max (~1, Real.negInf) (0, Seq.length idxs)
                ( fn i => (Seq.nth idxs i, d (Seq.nth idxs i))
                , fn (lo, hi) =>
                    let
                      val i = point_furthest_from_line_gpu ctx
                        (points_fut, l, r, Seq.subseq idxs (lo, hi - lo))
                    in
                      (i, d i)
                    end
                )

            fun doLeft () =
              filter_then_semihull_choose idxs l mid
            fun doRight () =
              filter_then_semihull idxs mid r

            val (leftHull, rightHull) =
              if Seq.length idxs <= semihull_par_grain then
                (doLeft (), doRight ())
              else
                ForkJoin.par (doLeft, doRight)
          in
            Tree.append (leftHull, (Tree.append (Tree.$ mid, rightHull)))
          end


      and semihull_choose idxs l r =
        if Seq.length idxs < semihull_hybrid_grain then
          semihull idxs l r
        else
          ForkJoin.choice
            { prefer_cpu = fn () => semihull idxs l r
            , prefer_gpu = fn () => semihull_gpu ctx (points_fut, idxs, l, r)
            }


      and filter_then_semihull idxs l r =
        let
          val lp = pt l
          val rp = pt r

          val idxs' =
            HybridBasis.filter_hybrid reduce_hybrid_split reduce_hybrid_grain
              (0, Seq.length idxs)
              ( fn i => Seq.nth idxs i
              , fn i => aboveLine lp rp (Seq.nth idxs i)
              , fn (lo, hi) =>
                  points_above_gpu ctx
                    (points_fut, Seq.subseq idxs (lo, hi - lo), l, r)
              )
        in
          semihull_choose idxs' l r
        end


      and filter_then_semihull_choose idxs l r =
        if Seq.length idxs <= semihull_hybrid_grain then
          filter_then_semihull idxs l r
        else
          ForkJoin.choice
            { prefer_cpu = fn _ => filter_then_semihull idxs l r
            , prefer_gpu = fn _ =>
                filter_then_semihull_gpu ctx (points_fut, l, r, idxs)
            }


      fun top_level_filter_then_semihull l r =
        let
          val lp = pt l
          val rp = pt r

          val tm = startTiming ()

          val above =
            HybridBasis.filter_hybrid reduce_hybrid_split reduce_hybrid_grain
              (0, FlatPointSeq.length pts)
              ( fn i => Int32.fromInt i
              , fn i => dist lp rp (Int32.fromInt i) > 0.0
              , fn (lo, hi) =>
                  top_level_points_above_in_range_gpu ctx
                    (points_fut, lo, hi, l, r)
              )

          val tm = tick tm "top-level filter"

          val above = semihull_choose above l r

          val tm = tick tm "top-level semihull"
        in
          above
        end


      fun top_level_filter_then_semihull_choose l r =
        ForkJoin.choice
          { prefer_cpu = fn _ => top_level_filter_then_semihull l r
          , prefer_gpu = fn _ =>
              top_level_filter_then_semihull_gpu ctx (points_fut, l, r)
          }

      val tm = startTiming ()

      val (l, r) =
        HybridBasis.reduce_hybrid reduce_hybrid_split reduce_hybrid_grain minmax
          (0, 0) (0, FlatPointSeq.length pts)
          ( fn i => (Int32.fromInt i, Int32.fromInt i)
          , fn (lo, hi) => min_max_point_in_range_gpu ctx (points_fut, lo, hi)
          )

      val tm = tick tm "endpoints"

      val (above, below) =
        ForkJoin.par (fn _ => top_level_filter_then_semihull_choose l r, fn _ =>
          top_level_filter_then_semihull r l)

      val tm = tick tm "quickhull"

      val hullt = Tree.append
        (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below))

      val result = Tree.toArraySeq hullt

      val tm = tick tm "flatten"
    in
      result
    end

end
