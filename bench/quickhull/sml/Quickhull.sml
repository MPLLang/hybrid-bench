structure Quickhull:
sig
  val hull: bool
            -> (int Split.seq * int * int -> int Split.seq)
            -> (real * real) Seq.t
            -> int Seq.t
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

  fun hull hybrid doGpu pts =
    let
      fun pt i = Seq.nth pts i
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

      fun aboveLine p q i =
        (dist p q i > 0.0)

      fun parHull idxs l r =
        if Seq.length idxs < 2 then
          Tree.fromArraySeq idxs
        else
          let
            val lp = pt l
            val rp = pt r
            fun d i =
              dist lp rp i
            (* val idxs = DS.fromArraySeq idxs *)

            val (mid, _) =
              SeqBasis.reduce 5000 max (~1, Real.negInf) (0, Seq.length idxs)
                (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))
            (* val distances = DS.map (fn i => (i, d i)) idxs
            val (mid, _) = DS.reduce max (~1, Real.negInf) distances *)

            val midp = pt mid

            fun flag i =
              if aboveLine lp midp i then Split.Left
              else if aboveLine midp rp i then Split.Right
              else Split.Throwaway
            (* val (left, right) = Split.parSplit idxs (Seq.map flag idxs) *)
            (* (DS.force (DS.map flag idxs)) *)

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
        if not hybrid orelse Seq.length idxs < 1000 then
          parHull idxs l r
        else
          ForkJoin.choice
            { prefer_cpu = fn () => parHull idxs l r
            , prefer_gpu = fn () => Tree.fromArraySeq (doGpu (idxs, l, r))
            }

      (*
      in
        if Seq.length idxs < 2 then
          Tree.fromArraySeq idxs
        (* if DS.length idxs <= 2048 then
           seqHull idxs l r *)
        else if hybrid then
          ForkJoin.choice
            { prefer_cpu = doCpu
            , prefer_gpu = fn () => Tree.fromArraySeq (doGpu (idxs, l, r))
            }
        else
          doCpu ()
      end
      *)

      val tm = startTiming ()

      (* val allIdx = DS.tabulate (fn i => i) (Seq.length pts) *)

      (* This is faster than doing two reduces *)
      (* val (l, r) = DS.reduce
        (fn ((l1, r1), (l2, r2)) =>
          (if x l1 < x l2 then l1 else l2,
           if x r1 > x r2 then r1 else r2))
        (0, 0)
        (DS.map (fn i => (i, i)) allIdx) *)

      val (l, r) =
        SeqBasis.reduce 5000
          (fn ((l1, r1), (l2, r2)) => (min_by x l1 l2, max_by x r1 r2)) (0, 0)
          (0, Seq.length pts) (fn i => (i, i))

      val tm = tick tm "endpoints"

      val lp = pt l
      val rp = pt r

      fun flag i =
        let
          val d = dist lp rp i
        in
          if d > 0.0 then Split.Left
          else if d < 0.0 then Split.Right
          else Split.Throwaway
        end
      (* val (above, below) = *)
      (* Split.parSplit allIdx (DS.force (DS.map flag allIdx)) *)
      (* Split.parSplit (Seq.tabulate (fn i => i) (Seq.length pts))
        (Seq.tabulate flag (Seq.length pts)) *)


      val flags =
        Seq.tabulate
          (fn i =>
             let
               val d = dist lp rp i
             in
               if d > 0.0 then 0w0 : Word8.word
               else if d < 0.0 then 0w1
               else 0w2
             end) (Seq.length pts)

      val (above, below) =
        ForkJoin.par
          ( fn _ =>
              ArraySlice.full
                (SeqBasis.filter 2000 (0, Seq.length pts) (fn i => i) (fn i =>
                   Seq.nth flags i = 0w0))
          , fn _ =>
              ArraySlice.full
                (SeqBasis.filter 2000 (0, Seq.length pts) (fn i => i) (fn i =>
                   Seq.nth flags i = 0w1))
          )


      val _ = print
        ("above "
         ^
         Real.fmt (StringCvt.FIX (SOME 1))
           (100.0 * Real.fromInt (Seq.length above)
            / Real.fromInt (Seq.length pts)) ^ "%\n")
      val _ = print
        ("below "
         ^
         Real.fmt (StringCvt.FIX (SOME 1))
           (100.0 * Real.fromInt (Seq.length below)
            / Real.fromInt (Seq.length pts)) ^ "%\n")

      val tm = tick tm "above/below filter"

      val (above, below) =
        ForkJoin.par (fn _ => parHull_choose above l r, fn _ =>
          parHull below r l)

      val tm = tick tm "quickhull"

      val hullt = Tree.append
        (Tree.append (Tree.$ l, above), Tree.append (Tree.$ r, below))

      val result = Tree.toArraySeq hullt

      val tm = tick tm "flatten"
    in
      result
    end

end
