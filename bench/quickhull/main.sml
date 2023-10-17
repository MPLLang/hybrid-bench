structure CLA = CommandLineArgs

(* File must contain data as produced by the randPoints program in PBBS. *)
val file = CLA.parseString "points" ""

val impl = CommandLineArgs.parseString "impl" "cpu"

val points =
  if file = "" then
    raise Fail "Need -points FILE"
  else
    ( print ("Reading points from " ^ file ^ "... ")
    ; ParseFile.readSequencePoint2d file before print "Done!\n"
    )

val num_points = Seq.length points

fun rand i n =
  Int.fromLarge (Word64.toLargeInt (Word64.mod
    (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))

fun randomPoints n =
  Seq.tabulate
    (fn i =>
       ( Real64.fromInt (rand i n) / Real64.fromInt n
       , Real64.fromInt (rand (i + 1) n) / Real64.fromInt n
       )) n

val () = print "Initialising Futhark context... "
val ctx = Futhark.Context.new
  (Futhark.Config.cache (SOME "futhark.cache") Futhark.Config.default)
val () = print "Done!\n"

fun futharkPoints (points: FlatPointSeq.t) =
  Futhark.Real64Array2.new ctx (FlatPointSeq.viewData points)
    (FlatPointSeq.length points, 2)

val points = FlatPointSeq.fromArraySeq points
val (points_fut, tm) = Util.getTime (fn _ => futharkPoints points)
val _ = print ("copied points to GPU in " ^ Time.fmt 4 tm ^ "s\n")

val bench =
  case impl of
    "cpu" => (fn () => Quickhull.hull_cpu points)
  | "gpu" => (fn () => Quickhull.hull_gpu ctx points_fut)
  | "hybrid" => (fn () => Quickhull.hull_hybrid ctx (points, points_fut))
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("quickhull " ^ impl) bench

val () = Futhark.Real64Array2.free points_fut
val () = Futhark.Context.free ctx
val () = print
  ("Points in convex hull: " ^ Int.toString (Seq.length result) ^ "\n")
