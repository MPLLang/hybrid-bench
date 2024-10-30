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

val points = FlatPairSeq.fromArraySeq points

val bench =
  case impl of
    "cpu" => (fn () => QuickhullCPU.hull_cpu {vectorized=false} points)
  | "cpu_vectorized" => (fn () => QuickhullCPU.hull_cpu {vectorized=true} points)

  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run ("quickhull " ^ impl) bench

val () = print
  ("Points in convex hull: " ^ Int.toString (Seq.length result) ^ "\n")
