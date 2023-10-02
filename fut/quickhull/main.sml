structure CLA = CommandLineArgs

val num_points = CLA.parseInt "n" 100000
val impl = CommandLineArgs.parseString "impl" "cpu"

fun rand i n =
  Int.fromLarge (Word64.toLargeInt (Word64.mod
    (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))

fun randomPoints n =
  Array.tabulate (n * 2, fn i => Real64.fromInt (rand i n) / Real64.fromInt n)

val ctx = Futhark.Context.new Futhark.default_cfg

fun quickhullCPU points =
    raise Fail "quickhullCPU: unimplemented"

fun quickhullHybrid points =
    raise Fail "quickhullHybrid: unimplemented"

fun quickhullGPU points =
    let val shape = (Int64.fromInt num_points,2)
        val points_fut = Futhark.Real64Array2.new ctx points shape
        val res_fut = Futhark.Entry.quickhull ctx points_fut
    in Futhark.Int32Array1.values res_fut before
       Futhark.Int32Array1.free res_fut before
       Futhark.Real64Array2.free points_fut
    end

val doit =
  case impl of
    "cpu" => quickhullCPU
  | "gpu" => quickhullGPU
  | "hybrid" => quickhullHybrid
  | _ => Util.die ("unknown -impl " ^ impl)

val () = print ("Generating " ^ Int.toString num_points ^ " random uniform points.\n")
val input = randomPoints num_points

val result = Benchmark.run ("quickhull " ^ impl) (fn () => doit input)

val () = Futhark.Context.free ctx
