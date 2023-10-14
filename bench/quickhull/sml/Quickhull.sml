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

      fun parHull (idxs: Int32.int Seq.t) l r =
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
              parHull left l mid
            fun doRight () =
              parHull right mid r
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

      val _ = print
        ("above "
         ^
         Real.fmt (StringCvt.FIX (SOME 1))
           (100.0 * Real.fromInt (Seq.length above)
            / Real.fromInt (FlatPointSeq.length pts)) ^ "%\n")
      val _ = print
        ("below "
         ^
         Real.fmt (StringCvt.FIX (SOME 1))
           (100.0 * Real.fromInt (Seq.length below)
            / Real.fromInt (FlatPointSeq.length pts)) ^ "%\n")

      val tm = tick tm "above/below filter"

      val (above, below) = ForkJoin.par (fn _ => parHull above l r, fn _ =>
        parHull below r l)

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

  fun filterThenSemihullGPU ctx points_fut (l, r) =
    let
      val res_fut =
        Futhark.Entry.top_level_filter_then_semihull ctx (points_fut, l, r)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
    in
      ArraySlice.full res
    end

  fun semihullGPU ctx points_fut (idxs, l, r) =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val res_fut = Futhark.Entry.semihull ctx (points_fut, l, r, idxs_fut)
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
      val () = Futhark.Int32Array1.free idxs_fut
    in
      ArraySlice.full res
    end

  fun minMaxPointsInRange ctx points_fut (lo, hi) =
    let
      val (min, max) =
        Futhark.Entry.min_max_point_in_range ctx
          (points_fut, Int64.fromInt lo, Int64.fromInt hi)
    in
      (min, max)
    end

  fun point_furthest_from_line_gpu ctx points_fut (l, r, idxs) :
    Int32.int * Real64.real =
    let
      val idxs_fut = Futhark.Int32Array1.new ctx idxs (Seq.length idxs)
      val (i, dist) =
        Futhark.Entry.point_furthest_from_line ctx (points_fut, l, r, idxs_fut)
      val () = Futhark.Int32Array1.free idxs_fut
    in
      (i, dist)
    end

  fun hull_gpu ctx points_fut =
    let
      val res_fut = Futhark.Entry.quickhull ctx points_fut
      val res = Futhark.Int32Array1.values res_fut
      val () = Futhark.Int32Array1.free res_fut
    in
      Seq.fromArraySeq (ArraySlice.full res)
    end


  (* ========================================================================
   * hybrid
   *)

  fun reduce_hybrid grain combine z (lo, hi) (f, g) =
    let
      fun base lo hi =
        SeqBasis.reduce grain combine z (lo, hi) f

      fun loop lo hi =
        if hi - lo <= grain then
          base lo hi
        else
          let
            val mid = lo + (hi - lo) div 2
          in
            combine (ForkJoin.par (fn _ => loop_choose lo mid, fn _ =>
              loop mid hi))
          end

      and loop_choose lo hi =
        if hi - lo <= 5000 then
          base lo hi
        else
          ForkJoin.choice
            {prefer_cpu = fn _ => loop lo hi, prefer_gpu = fn _ => g (lo, hi)}
    in
      loop lo hi
    end


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


      fun parHull (idxs: Int32.int Seq.t) l r =
        if Seq.length idxs < 2 then
          Tree.fromArraySeq idxs
        else
          let
            val lp = pt l
            val rp = pt r
            fun d i =
              dist lp rp i

            val (mid, _) =
              reduce_hybrid 5000 max (~1, Real.negInf) (0, Seq.length idxs)
                ( fn i => (Seq.nth idxs i, d (Seq.nth idxs i))
                , fn (lo, hi) =>
                    let
                      val (i, _) = point_furthest_from_line_gpu ctx points_fut
                        (l, r, Seq.subseq idxs (lo, hi - lo))
                    in
                      (i, d i)
                    end
                )

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
              parHull_choose left l mid
            fun doRight () =
              parHull right mid r
            val (leftHull, rightHull) =
              if Seq.length left + Seq.length right <= 1000 then
                (doLeft (), doRight ())
              else
                ForkJoin.par (doLeft, doRight)
          in
            Tree.append (leftHull, (Tree.append (Tree.$ mid, rightHull)))
          end


      and parHull_choose idxs l r =
        if Seq.length idxs < 1000 then
          parHull idxs l r
        else
          ForkJoin.choice
            { prefer_cpu = fn () => parHull idxs l r
            , prefer_gpu = fn () =>
                Tree.fromArraySeq (semihullGPU ctx points_fut (idxs, l, r))
            }


      fun top_level_filter_then_semihull l r =
        let
          val lp = pt l
          val rp = pt r

          val tm = startTiming ()

          val flags =
            Seq.tabulate
              (fn i =>
                 if dist lp rp (Int32.fromInt i) > 0.0 then (0w1 : Word8.word)
                 else 0w0) (FlatPointSeq.length pts)

          val tm = tick tm "cpu flags"

          val above =
            ArraySlice.full
              (SeqBasis.filter 2000 (0, FlatPointSeq.length pts)
                 (fn i => Int32.fromInt i) (fn i => Seq.nth flags i = 0w1))

          val tm = tick tm "cpu filter"

          val above = parHull above l r

          val tm = tick tm "cpu semihull"
        in
          above
        end


      fun top_level_filter_then_semihull_choose l r =
        ForkJoin.choice
          { prefer_cpu = fn _ => top_level_filter_then_semihull l r
          , prefer_gpu = fn _ =>
              Tree.fromArraySeq (filterThenSemihullGPU ctx points_fut (l, r))
          }

      val tm = startTiming ()

      val (l, r) =
        reduce_hybrid 5000 minmax (0, 0) (0, FlatPointSeq.length pts)
          ( fn i => (Int32.fromInt i, Int32.fromInt i)
          , fn (lo, hi) => minMaxPointsInRange ctx points_fut (lo, hi)
          )

      val tm = tick tm "endpoints"

      val (above, below) =
        ForkJoin.par (fn _ => top_level_filter_then_semihull_choose l r, fn _ =>
          top_level_filter_then_semihull_choose r l)

      val tm = tick tm "quickhull"

      val hullt = Tree.append
        (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below))

      val result = Tree.toArraySeq hullt

      val tm = tick tm "flatten"
    in
      result
    end

end
