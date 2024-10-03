functor QuickhullCPU_Alt(G: GEOMETRY):
sig
  val hull_cpu: {vectorized: bool} -> G.R.real FlatPairSeq.t -> Int32.int Seq.t
end =
struct

  structure AS = ArraySlice
  structure Tree = TreeSeq

  structure R = struct open G.R val fromReal = fromLarge IEEEReal.TO_NEAREST end

  val zero = R.fromReal 0.0

  fun startTiming () = Time.now ()

  fun par4 (f1, f2, f3, f4) =
    let
      val ((r1, r2), (r3, r4)) =
        ForkJoin.par (fn _ => ForkJoin.par (f1, f2), fn _ =>
          ForkJoin.par (f3, f4))
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
   * quick hull
   *)

  fun hull_cpu {vectorized: bool} pts =
    if vectorized then
      Util.die ("no vectorized implementation yet, sorry")
    else
      let
        fun pt i =
          FlatPairSeq.nth pts (Int32.toInt i)
        fun dist p q i =
          G.Point.triArea (p, q, pt i)
        fun max ((i, di), (j, dj)) =
          if R.> (di, dj) then (i, di) else (j, dj)

        fun maxmax ( ((l1, dl1), (r1, dr1)), ((l2, dl2), (r2, dr2)) ) =
          ( if R.> (dl1, dl2) then (l1, dl1) else (l2, dl2)
          , if R.> (dr1, dr2) then (r1, dr1) else (r2, dr2)
          )

        fun xx i =
          #1 (pt i)
        fun yy i =
          #2 (pt i)
        fun min_by f x y =
          if R.< (f x, f y) then x else y
        fun max_by f x y =
          if R.> (f x, f y) then x else y
        fun minmax ((l1, r1, b1, t1), (l2, r2, b2, t2)) =
          (min_by xx l1 l2, max_by xx r1 r2, min_by yy b1 b2, max_by yy t1 t2)
        fun aboveLine p q i =
          R.> (dist p q i, zero)


        fun semihull (idxs: Int32.int Seq.t) l m r =
          if Seq.length idxs < 2 then
            Tree.fromArraySeq idxs
          else
            let
              val lp = pt l
              val mp = pt m
              val rp = pt r

              val flags: Word8.word array = ForkJoin.alloc (Seq.length idxs)
              val ((mid_left, _), (mid_right, _)) =
                SeqBasis.reduce 5000 maxmax ((~1, R.negInf), (~1, R.negInf)) (0, Seq.length idxs)
                  (fn i => 
                    let
                      val ii = Seq.nth idxs i
                      val p = pt ii

                      val left_dist = dist lp mp ii (* G.Point.triArea (lp, mp, p) *)
                      val right_dist = dist mp rp ii (* G.Point.triArea (mp, rp, p) *)

                      val flag =
                        if R.> (left_dist, zero) then 0w0
                        else if R.> (right_dist, zero) then 0w1
                        else 0w2
                    in
                      Array.update (flags, i, flag);
                      ((ii, left_dist), (ii, right_dist))
                    end)

              fun doLeft () =
                let
                  val above =
                    ArraySlice.full
                      (SeqBasis.filter 5000 (0, Seq.length idxs)
                        (fn i => Seq.nth idxs i)
                        (fn i => Array.sub (flags, i) = 0w0))
                in
                  semihull above l mid_left m
                end

              fun doRight () =
                let
                  val above =
                    ArraySlice.full
                      (SeqBasis.filter 5000 (0, Seq.length idxs)
                        (fn i => Seq.nth idxs i)
                        (fn i => Array.sub (flags, i) = 0w1))
                in
                  semihull above m mid_right r
                end

              val (leftHull, rightHull) =
                if Seq.length idxs <= 5000 then (doLeft (), doRight ())
                else ForkJoin.par (doLeft, doRight)
            in
              Tree.append (leftHull, (Tree.append (Tree.$ m, rightHull)))
            end

        val tm = startTiming ()


        fun minmax ((l1, r1), (l2, r2)) =
          (min_by xx l1 l2, max_by xx r1 r2)

        val (l, r) =
          SeqBasis.reduce 5000 minmax (0, 0) (0, FlatPairSeq.length pts)
            (fn i => let val ii = Int32.fromInt i in (ii, ii) end)

        val tm = tick tm "endpoints"

        val flags: Word8.word array = ForkJoin.alloc (FlatPairSeq.length pts)

        val lp = pt l
        val rp = pt r

        val ((upper_mid, _), (lower_mid, _)) =
          SeqBasis.reduce 5000 maxmax ((~1, R.negInf), (~1, R.negInf)) (0, FlatPairSeq.length pts)
            (fn i => 
              let
                val dist = G.Point.triArea (lp, rp, FlatPairSeq.nth pts i)
                val flag =
                  if R.> (dist, zero) then 0w0
                  else if R.< (dist, zero) then 0w1
                  else 0w2
              in
                Array.update (flags, i, flag);
                ((i, dist), (i, R.~ dist))
              end)
        val upper_mid = Int32.fromInt upper_mid
        val lower_mid = Int32.fromInt lower_mid

        val tm = tick tm "top-level flags"

        fun doUpper () =
          let
            val idxs =
              ArraySlice.full
                (SeqBasis.filter 5000 (0, FlatPairSeq.length pts)
                  (fn i => Int32.fromInt i)
                  (fn i => Array.sub (flags, i) = 0w0))
          in
            semihull idxs l upper_mid r
          end

        fun doLower () =
          let
            val idxs =
              ArraySlice.full
                (SeqBasis.filter 5000 (0, FlatPairSeq.length pts)
                  (fn i => Int32.fromInt i)
                  (fn i => Array.sub (flags, i) = 0w1))
          in
            semihull idxs r lower_mid l
          end

        val (top, bottom) =
          ForkJoin.par
            (doUpper
            ,doLower
            )

        val tm = tick tm "quickhull"

        val hullt = Tree.append
          (Tree.append (Tree.$ l, top), Tree.append (Tree.$ r, bottom))

        (* val hullt =
          Tree.append
            ( Tree.append
                ( Tree.append (Tree.$ l, topleft)
                , Tree.append (Tree.$ t, topright)
                )
            , Tree.append
                ( Tree.append (Tree.$ r, botright)
                , Tree.append (Tree.$ b, botleft)
                )
            ) *)


        val result = Tree.toArraySeq hullt

        val tm = tick tm "flatten"
      in
        result
      end

end
