functor QuickhullCPU(G: GEOMETRY):
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
                SeqBasis.reduce 1000 max (~1, R.negInf) (0, Seq.length idxs)
                  (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))

              fun doLeft () =
                filter_then_semihull idxs l mid
              fun doRight () =
                filter_then_semihull idxs mid r

              val (leftHull, rightHull) =
                if Seq.length idxs <= 1000 then (doLeft (), doRight ())
                else ForkJoin.par (doLeft, doRight)
            in
              Tree.append (leftHull, (Tree.append (Tree.$ mid, rightHull)))
            end


        and filter_then_semihull idxs l r =
          let
            val lp = pt l
            val rp = pt r

            val idxs' =
              ArraySlice.full
                (SeqBasis.filter 1000 (0, Seq.length idxs)
                   (fn i => Seq.nth idxs i)
                   (fn i => aboveLine lp rp (Seq.nth idxs i)))
          in
            semihull idxs' l r
          end


        fun top_level_filter_then_semihull l r =
          let
            val lp = pt l
            val rp = pt r

            val tm = startTiming ()

            val above =
              ArraySlice.full
                (SeqBasis.filter 1000 (0, FlatPairSeq.length pts)
                   (fn i => Int32.fromInt i)
                   (fn i => R.> (dist lp rp (Int32.fromInt i), zero)))

            val tm = tick tm "top-level filter"

            val above = semihull above l r

            val tm = tick tm "top-level semihull"
          in
            above
          end

        val tm = startTiming ()

        val (l, r, b, t) =
          SeqBasis.reduce 1000 minmax (0, 0, 0, 0) (0, FlatPairSeq.length pts)
            (fn i => let val ii = Int32.fromInt i in (ii, ii, ii, ii) end)

        val tm = tick tm "endpoints"

        val (topleft, topright, botleft, botright) =
          par4
            ( fn _ => top_level_filter_then_semihull l t
            , fn _ => top_level_filter_then_semihull t r
            , fn _ => top_level_filter_then_semihull r b
            , fn _ => top_level_filter_then_semihull b l
            )

        val tm = tick tm "quickhull"

        (* val hullt = Tree.append
          (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below)) *)

        val hullt =
          Tree.append
            ( Tree.append
                ( Tree.append (Tree.$ l, topleft)
                , Tree.append (Tree.$ t, topright)
                )
            , Tree.append
                ( Tree.append (Tree.$ r, botright)
                , Tree.append (Tree.$ b, botleft)
                )
            )


        val result = Tree.toArraySeq hullt

        val tm = tick tm "flatten"
      in
        result
      end

end
