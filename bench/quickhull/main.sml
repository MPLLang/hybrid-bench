structure CLA = CommandLineArgs

(* File must contain data as produced by the randPoints program in PBBS. *)
val file = CLA.parseString "points" ""

val impl = CommandLineArgs.parseString "impl" "cpu"

val devices = String.fields (fn c => c = #",")
  (CommandLineArgs.parseString "devices" "")

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

structure CtxSet = CtxSetFn (structure F = Futhark)

val () = print "Initialising Futhark context... "
val ctxSet = CtxSet.fromList devices
val (default_device, default_ctx) = Seq.first ctxSet
val () = print "Done!\n"

structure Quickhull = Quickhull(CtxSet)

fun futharkPoints (points: FlatPointSeq.t) ctx =
  Futhark.Real64Array2.new ctx (FlatPointSeq.viewData points)
    (FlatPointSeq.length points, 2)

(* structure FutharkPoints = GpuData(type t = Futhark.Real64Array2.t) *)

val points = FlatPointSeq.fromArraySeq points
val (points_fut_set, tm) = Util.getTime (fn _ =>
  (GpuData.initialize ctxSet (futharkPoints points)))
val _ = print ("copied points to GPUs in " ^ Time.fmt 4 tm ^ "s\n")

val bench =
  case impl of
    "cpu" => (fn () => Quickhull.hull_cpu points)
  | "gpu" =>
      (fn () =>
         Quickhull.hull_gpu default_ctx
           (GpuData.choose points_fut_set default_device))

  | "hybrid" => (fn () => Quickhull.hull_hybrid ctxSet (points, points_fut_set))

  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("quickhull " ^ impl) bench

val () = GpuData.free points_fut_set Futhark.Real64Array2.free
val () = CtxSet.free ctxSet
val () = print
  ("Points in convex hull: " ^ Int.toString (Seq.length result) ^ "\n")
