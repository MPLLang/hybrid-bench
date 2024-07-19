functor Quickhull(CtxSet: CTX_SET where type ctx = Futhark.ctx):
sig

  val hull_cpu: FlatPointSeq.t -> Int32.int Seq.t

  val hull_gpu: Futhark.ctx -> Futhark.Real64Array2.array -> Int32.int Seq.t

  val hull_hybrid: CtxSet.t
                   -> FlatPointSeq.t * (Futhark.Real64Array2.array GpuData.t)
                   -> Int32.int Seq.t
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

  fun tt t1 t2 = Time.fmt 4 (Time.- (t2,t1))

  fun tts ts =
    case ts of
      [] => ""
    | [_] => ""
    | [t1, t2] => tt t1 t2 ^ "s\n"
    | t1 :: t2 :: rest => tt t1 t2 ^ "+" ^ tts (t2 :: rest)


  fun top_level_points_above_in_range_gpu (device, ctx) (points_fut, lo, hi, l, r) =
    let
      val t0 = Time.now ()
      val res_fut = Futhark.Entry.top_level_points_above_in_range ctx
        (points_fut, lo, hi, l, r)
      val t1 = Time.now ()
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val t2 = Time.now ()
    in
      print ("gpu " ^ device ^ " top_level_points_above_in_range("
      ^ Int.toString (hi-lo) ^ "): " ^ tts [t0, t1]);

      ArraySlice.full res
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun points_above_gpu (device, ctx) (points_fut, idxs, l, r) =
    let
      val t0 = Time.now ()
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val t1 = Time.now ()
      val res_fut = Futhark.Entry.points_above ctx (points_fut, idxs_fut, l, r)
      val t2 = Time.now ()
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
      val t3 = Time.now ()
    in
      print ("gpu " ^ device ^ " points_above("
      ^ Int.toString (Seq.length idxs) ^ "): " ^ tts [t0, t1, t2, t3]);
      ArraySlice.full res
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun top_level_filter_then_semihull_gpu (device, ctx) (points_fut, l, r) =
    let
      val t0 = Time.now ()
      val res_fut =
        Futhark.Entry.top_level_filter_then_semihull ctx (points_fut, l, r)
      val t1 = Time.now ()
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val t2 = Time.now ()
    in
      print ("gpu " ^ device ^ " top_level_filter_then_semihull():"
      ^ tts [t0, t1, t2]);
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun filter_then_semihull_gpu (device, ctx) (points_fut, l, r, idxs) =
    let
      val t0 = Time.now ()
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val t1 = Time.now ()
      val res_fut =
        Futhark.Entry.filter_then_semihull ctx (points_fut, l, r, idxs_fut)
      val t2 = Time.now ()
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
      val t3 = Time.now ()
    in
      print ("gpu " ^ device ^ " filter_then_semihull("
      ^ Int.toString (Seq.length idxs) ^ "): " ^ tts [t0, t1, t2, t3]);
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun semihull_gpu (device, ctx) (points_fut, idxs, l, r) =
    let
      val t0 = Time.now ()
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val t1 = Time.now ()
      val res_fut = Futhark.Entry.semihull ctx (points_fut, l, r, idxs_fut)
      val t2 = Time.now ()
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
      val t3 = Time.now ()
    in
      print ("gpu " ^ device ^ " semihull_gpu("
      ^ Int.toString (Seq.length idxs) ^ "): " ^ tts [t0, t1, t2, t3]);
      Tree.fromArraySeq (ArraySlice.full res)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun min_max_point_in_range_gpu (device, ctx) (points_fut, lo, hi) =
    let
      val t0 = Time.now ()
      val (l, r, b, t) =
        Futhark.Entry.min_max_point_in_range ctx
          (points_fut, Int64.fromInt lo, Int64.fromInt hi)
      val t1 = Time.now ()
    in
      print ("gpu " ^ device ^ " min_max_point_in_range("
      ^ Int.toString (hi-lo) ^ "): " ^ tts [t0, t1]);
      (l, r, b, t)
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)


  fun point_furthest_from_line_gpu (device, ctx) (points_fut, l, r, idxs) : Int32.int =
    let
      val t0 = Time.now ()
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val t1 = Time.now ()
      val i =
        Futhark.Entry.point_furthest_from_line ctx (points_fut, l, r, idxs_fut)
      val () = Futhark.Int32Array1.free idxs_fut
      val t2 = Time.now ()
    in
      print ("gpu " ^ device ^ " point_furthest_from_line("
      ^ Int.toString (Seq.length idxs) ^ "): " ^ tts [t0, t1, t2]);
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
  val reduce_hybrid_outer_split =
    BenchParams.Quickhull.reduce_hybrid_outer_split
  val reduce_hybrid_inner_split =
    BenchParams.Quickhull.reduce_hybrid_inner_split


  fun hull_hybrid ctxSet (pts, points_fut_set) =
    let
      fun pt i =
        FlatPointSeq.nth pts (Int32.toInt i)
      fun dist p q i =
        G.Point.triArea (p, q, pt i)
      fun max ((i, di), (j, dj)) =
        if di > dj then (i, di) else (j, dj)
      fun xx i =
        #1 (pt i)
      fun yy i =
        #2 (pt i)
      fun min_by f x y =
        if f x < f y then x else y
      fun max_by f x y =
        if f x > f y then x else y
      fun minmax ((l1, r1, b1, t1), (l2, r2, b2, t2)) =
        (min_by xx l1 l2, max_by xx r1 r2, min_by yy b1 b2, max_by yy t1 t2)
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
              HybridBasis.reduce_hybrid reduce_hybrid_outer_split
                reduce_hybrid_inner_split reduce_hybrid_grain max
                (~1, Real.negInf) (0, Seq.length idxs)
                ( fn i => (Seq.nth idxs i, d (Seq.nth idxs i))
                , fn device =>
                    fn (lo, hi) =>
                      let
                        val ctx = CtxSet.choose ctxSet device
                        val points_fut = GpuData.choose points_fut_set device
                        val i = point_furthest_from_line_gpu (device, ctx)
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
            , prefer_gpu = fn device =>
                let
                  val ctx = CtxSet.choose ctxSet device
                  val points_fut = GpuData.choose points_fut_set device
                in
                  semihull_gpu (device, ctx) (points_fut, idxs, l, r)
                end

            }


      and filter_then_semihull idxs l r =
        let
          val lp = pt l
          val rp = pt r

          val idxs' =
            HybridBasis.filter_hybrid reduce_hybrid_outer_split
              reduce_hybrid_inner_split reduce_hybrid_grain (0, Seq.length idxs)
              ( fn i => Seq.nth idxs i
              , fn i => aboveLine lp rp (Seq.nth idxs i)
              , fn device =>
                  fn (lo, hi) =>
                    let
                      val ctx = CtxSet.choose ctxSet device
                      val points_fut = GpuData.choose points_fut_set device
                    in
                      points_above_gpu (device, ctx)
                        (points_fut, Seq.subseq idxs (lo, hi - lo), l, r)
                    end
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
            , prefer_gpu = fn device =>
                let
                  val ctx = CtxSet.choose ctxSet device
                  val points_fut = GpuData.choose points_fut_set device
                in
                  filter_then_semihull_gpu (device, ctx) (points_fut, l, r, idxs)
                end
            }


      fun top_level_filter_then_semihull l r =
        let
          val lp = pt l
          val rp = pt r

          val tm = startTiming ()

          val above =
            HybridBasis.filter_hybrid reduce_hybrid_outer_split
              reduce_hybrid_inner_split reduce_hybrid_grain
              (0, FlatPointSeq.length pts)
              ( fn i => Int32.fromInt i
              , fn i => dist lp rp (Int32.fromInt i) > 0.0
              , fn device =>
                  fn (lo, hi) =>
                    let
                      val ctx = CtxSet.choose ctxSet device
                      val points_fut = GpuData.choose points_fut_set device
                    in
                      top_level_points_above_in_range_gpu (device, ctx)
                        (points_fut, lo, hi, l, r)
                    end
              )

          val tm = tick tm "top-level filter"

          val above = semihull above l r

          val tm = tick tm "top-level semihull"
        in
          above
        end


      fun top_level_filter_then_semihull_choose l r =
        ForkJoin.choice
          { prefer_cpu = fn _ => top_level_filter_then_semihull l r
          , prefer_gpu = fn device =>
              let
                val ctx = CtxSet.choose ctxSet device
                val points_fut = GpuData.choose points_fut_set device
              in
                top_level_filter_then_semihull_gpu (device, ctx) (points_fut, l, r)
              end
          }

      val tm = startTiming ()

      val (l, r, b, t) =
        HybridBasis.reduce_hybrid reduce_hybrid_outer_split
          reduce_hybrid_inner_split reduce_hybrid_grain minmax (0, 0, 0, 0)
          (0, FlatPointSeq.length pts)
          ( fn i =>
              let
                val ii = Int32.fromInt i
              in
                (ii, ii, ii, ii)
              end
          , fn device =>
              fn (lo, hi) =>
                let
                  val ctx = CtxSet.choose ctxSet device
                  val points_fut = GpuData.choose points_fut_set device
                in
                  min_max_point_in_range_gpu (device, ctx) (points_fut, lo, hi)
                end
          )

      val tm = tick tm "endpoints"

      val (topleft, topright, botleft, botright) = par4
        ( fn _ => top_level_filter_then_semihull_choose l t
        , fn _ => top_level_filter_then_semihull_choose t r
        , fn _ => top_level_filter_then_semihull_choose r b
        , fn _ => top_level_filter_then_semihull b l
        )

      val tm = tick tm "quickhull"

      (* val hullt = Tree.append
        (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below)) *)

      val hullt = Tree.append
        ( Tree.append
            ( Tree.append (Tree.$ l, topleft)
            , Tree.append (Tree.$ t, topright)
            )
        , Tree.append
            ( Tree.append (Tree.$ r, botleft)
            , Tree.append (Tree.$ b, botright)
            )
        )


      val result = Tree.toArraySeq hullt

      val tm = tick tm "flatten"
    in
      result
    end

end
