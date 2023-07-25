structure CLA = CommandLineArgs

val q = CLA.parseInt "q" 100
val n = CLA.parseInt "n" 1000000

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("q " ^ Int.toString q ^ "\n")

val max_size = 1000000000
(* val gap_size = 100 *)

fun randRange i j seed =
  i
  +
  Word64.toInt (Word64.mod
    (Util.hash64 (Word64.fromInt seed), Word64.fromInt (j - i)))

(*
fun randSeg seed =
  let
    val p = gap_size * (randRange 1 (max_size div gap_size) seed)
    val hi = Int.min (p + gap_size, max_size)
  in
    (p, randRange p hi (seed+1))
  end
*)

fun randSeg seed =
  let
    val p = randRange 1 max_size seed
    val space = max_size - p
    val hi = p + 1 + space div 100
  in
    (Int32.fromInt p, Int32.fromInt (randRange p hi (seed + 1)))
  end

(* fun query seed =
  IntervalMap.stab tree (randRange 1 max_size seed) *)

fun query tree seed =
  IntervalMap.size (IntervalMap.report_all tree (Int32.fromInt
    (randRange 1 max_size seed)))


fun makeIntervalMap (segs: IntervalMap.interval Seq.t) =
  let

    fun getSeg i =
      Seq.nth segs (Int32.toInt i)

    fun doitSorted idxs =
      if Seq.length idxs = 0 then
        IntervalMap.empty ()
      else
        let
          val half = Seq.length idxs div 2
          val (x, y) = getSeg (Seq.nth idxs half)
          val l = Seq.take idxs half
          val r = Seq.drop idxs (half + 1)

          val (l', r') =
            if Seq.length idxs <= 500 then (doitSorted l, doitSorted r)
            else ForkJoin.par (fn _ => doitSorted l, fn _ => doitSorted r)
        in
          IntervalMap.joinMid (l', (x, y), r')
        end


    fun doitUnsorted idxs =
      if Seq.length idxs = 0 then
        IntervalMap.empty ()
      else
        let
          (* pivot... just hack it, assume input is random/shuffled already *)
          val (x, y) = getSeg (Seq.nth idxs (Seq.length idxs div 2))

          val l = Seq.filter (fn i => #1 (getSeg i) < x) idxs
          val r = Seq.filter (fn i => #1 (getSeg i) > x) idxs

          val (l', r') =
            if Seq.length idxs <= 500 then (doitWithSort l, doitWithSort r)
            else ForkJoin.par (fn _ => doitUnsorted l, fn _ => doitWithSort r)
        in
          IntervalMap.joinMid (l', (x, y), r')
        end

    and doitWithSort idxs =
      let
        fun cmp (i, j) =
          Int32.compare (#1 (getSeg i), #1 (getSeg j))
        val idxs' = Mergesort.sort cmp idxs
      in
        doitSorted idxs'
      end
  in
    doitUnsorted (Seq.tabulate (fn i => Int32.fromInt i) (Seq.length segs))
  end


fun bench () =
  makeIntervalMap (Seq.tabulate (fn i => randSeg (2 * i)) n)
(* IntervalMap.interval_map (Seq.tabulate (fn i => randSeg (2 * i)) n) n *)

val tree = Benchmark.run "generating intervals..." bench
val result = ArraySlice.full (SeqBasis.tabulate 1 (0, 10) (fn i =>
  query tree (2 * n + i)))

val numHits = Seq.reduce op+ 0 result
val minHits = Seq.reduce Int.min (valOf Int.maxInt) result
val maxHits = Seq.reduce Int.max 0 result
val avgHits = Real.round (Real.fromInt numHits / Real.fromInt q)
val _ = print ("hits " ^ Int.toString numHits ^ "\n")
val _ = print ("min " ^ Int.toString minHits ^ "\n")
val _ = print ("avg " ^ Int.toString avgHits ^ "\n")
val _ = print ("max " ^ Int.toString maxHits ^ "\n")
