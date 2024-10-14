structure CLA = CommandLineArgs

structure FutharkEntries32 =
struct
  type futhark_points = Futhark.Real32Array2.array
  type i32 = Int32.int
  type i64 = Int64.int
  type futhark_i32_arr = Futhark.Int32Array1.array
  type ctx = Futhark.ctx
  val filter_then_semihull = Futhark.Entry.filter_then_semihull_f32
  val min_max_point_in_range = Futhark.Entry.min_max_point_in_range_f32
  val point_furthest_from_line = Futhark.Entry.point_furthest_from_line_f32
  val points_above = Futhark.Entry.points_above_f32
  val quickhull = Futhark.Entry.quickhull_f32
  val semihull = Futhark.Entry.semihull_f32
  val top_level_filter_then_semihull =
    Futhark.Entry.top_level_filter_then_semihull_f32
  val top_level_points_above_in_range =
    Futhark.Entry.top_level_points_above_in_range_f32
end

structure FutharkEntries64 =
struct
  type futhark_points = Futhark.Real64Array2.array
  type i32 = Int32.int
  type i64 = Int64.int
  type futhark_i32_arr = Futhark.Int32Array1.array
  type ctx = Futhark.ctx
  val filter_then_semihull = Futhark.Entry.filter_then_semihull_f64
  val min_max_point_in_range = Futhark.Entry.min_max_point_in_range_f64
  val point_furthest_from_line = Futhark.Entry.point_furthest_from_line_f64
  val points_above = Futhark.Entry.points_above_f64
  val quickhull = Futhark.Entry.quickhull_f64
  val semihull = Futhark.Entry.semihull_f64
  val top_level_filter_then_semihull =
    Futhark.Entry.top_level_filter_then_semihull_f64
  val top_level_points_above_in_range =
    Futhark.Entry.top_level_points_above_in_range_f64
end


functor Main
  (structure G: GEOMETRY
   structure F: FUTHARK_ENTRIES

   (* either Futhark.Real64Array2.new or Futhark.Real32Array2.new *)
   val futhark_points_new: Futhark.ctx
                           -> G.R.real ArraySlice.slice
                           -> int * int
                           -> F.futhark_points

   (* either Futhark.Real64Array2.free or Futhark.Real32Array2.free *)
   val futhark_points_free: F.futhark_points -> unit) =
struct

  structure R = G.R
  structure CtxSet = CtxSetFn (structure F = Futhark)
  structure Quickhull =
    Quickhull (structure CtxSet = CtxSet structure G = G structure F = F)

  structure QuickhullCPU = QuickhullCPU(G)
  structure QuickhullCPU_Alt = QuickhullCPU_Alt(G)

  fun fromReal x = R.fromLarge IEEEReal.TO_NEAREST x

  fun doit () =
    let
      (* File must contain data as produced by the randPoints program in PBBS. *)
      val file = CLA.parseString "points" ""

      val impl = CLA.parseString "impl" "cpu"

      val devices = String.fields (fn c => c = #",")
        (CLA.parseString "devices" "")

      val log = CLA.parseBool "log" false

      val points: (R.real * R.real) Seq.t =
        if file = "" then
          raise Fail "Need -points FILE"
        else
          ( print ("Reading points from " ^ file ^ "... ")
          ; Seq.map (fn (x, y) => (fromReal x, fromReal y))
              (ParseFile.readSequencePoint2d file) before print "Done!\n"
          )

      val num_points = Seq.length points

      (* fun rand i n =
        Int.fromLarge (Word64.toLargeInt (Word64.mod
          (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))
      
      fun randomPoints n =
        Seq.tabulate
          (fn i =>
             ( R.fromInt (rand i n) / R.fromInt n
             , R.fromInt (rand (i + 1) n) / R.fromInt n
             )) n *)

      val () = print "Initialising Futhark context... "
      val ctxSet = CtxSet.fromList devices
      val default_device = 0
      val default_ctx = CtxSet.choose ctxSet default_device
      val () = print "Done!\n"

      fun futharkPoints (points: R.real FlatPairSeq.t) ctx =
        futhark_points_new ctx (FlatPairSeq.viewData points)
          (FlatPairSeq.length points, 2)

      (* structure FutharkPoints = GpuData(type t = Futhark.Real64Array2.t) *)

      val points = FlatPairSeq.fromArraySeq points
      val (points_fut_set, tm) = Util.getTime (fn _ =>
        (GpuData.initialize ctxSet (futharkPoints points)))
      val _ = print ("copied points to GPUs in " ^ Time.fmt 4 tm ^ "s\n")

      val bench =
        case impl of
          "cpu" => (fn () => QuickhullCPU.hull_cpu {vectorized = false} points)
        | "cpu_alt" => (fn () => QuickhullCPU_Alt.hull_cpu {vectorized = false} points)
        | "cpu_vectorized" =>
            (fn () => QuickhullCPU.hull_cpu {vectorized = true} points)

        | "gpu" =>
            (fn () =>
               Quickhull.hull_gpu default_ctx
                 (GpuData.choose points_fut_set default_device))

        | "hybrid" =>
            (fn () => Quickhull.hull_hybrid ctxSet (points, points_fut_set))

        | _ => Util.die ("unknown -impl " ^ impl)

      val result = Benchmark.run ("quickhull " ^ impl) bench

      val () = GpuData.free points_fut_set futhark_points_free
      val () = CtxSet.free ctxSet
      val () = print
        ("Points in convex hull: " ^ Int.toString (Seq.length result) ^ "\n")

    in
      ()
    end

end


structure Main32 =
  Main
    (structure G = Geometry2D32
     structure F = FutharkEntries32
     val futhark_points_new = Futhark.Real32Array2.new
     val futhark_points_free = Futhark.Real32Array2.free)

structure Main64 =
  Main
    (structure G = Geometry2D64
     structure F = FutharkEntries64
     val futhark_points_new = Futhark.Real64Array2.new
     val futhark_points_free = Futhark.Real64Array2.free)

val precision = CLA.parseString "precision" "64"
val _ = print ("precision " ^ precision ^ "\n")

val _ =
  case precision of
    "32" => Main32.doit ()
  | "64" => Main64.doit ()
  | _ =>
      Util.die
        ("unknown -precision " ^ precision ^ "; valid choices are: 32 64")
