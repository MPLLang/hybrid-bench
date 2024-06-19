structure CLA = CommandLineArgs
structure CtxSet = CtxSetFn (structure F = Futhark)

structure R32 = struct open MLton.Real32 open Real32 structure W = Word32 end
structure M =
  MatCOO (structure I = Int32 structure R = R32 structure CtxSet = CtxSet)

val inputFile = CLA.parseString "input" ""
val outputBinFile = CLA.parseString "output-bin" ""
val impl = CLA.parseString "impl" "cpu"

val exclude_copy_mat_onto_gpu = CLA.parseFlag "exclude-copy-mat-onto-gpu"

val (mat, tm) = Util.getTime (fn _ =>
  if inputFile = "" then Util.die ("missing -input FILE.mtx")
  else M.fromFile inputFile)
val _ = print ("parsed input in " ^ Time.fmt 4 tm ^ "s\n")

val _ =
  if outputBinFile = "" then
    ()
  else
    ( M.writeBinFile mat outputBinFile
    ; print ("wrote matrix in binary format at " ^ outputBinFile ^ "\n")
    )

val num_rows = M.height mat
val num_cols = M.width mat
val num_vals = M.nnz mat
val _ = print ("num rows " ^ M.I.toString num_rows ^ "\n")
val _ = print ("num cols " ^ M.I.toString num_cols ^ "\n")
val _ = print ("num vals " ^ Int.toString num_vals ^ "\n")

val vec = Seq.tabulate (fn _ => M.R.fromInt 1) (M.I.toInt num_rows)

val () = print "Initialising Futhark context... "
val devices = String.fields (fn c => c = #",") (CLA.parseString "devices" "")
val ctx_set = CtxSet.fromList devices
val (default_device, default_ctx) = Seq.first ctx_set
val () = print "Done!\n"


val mat_futs =
  if exclude_copy_mat_onto_gpu andalso impl <> "cpu" then
    SOME (GpuData.initialize ctx_set (fn ctx => M.mat_on_gpu ctx mat))
  else
    NONE

val bench =
  case impl of
    "cpu" => (fn () => M.mxv mat vec)
  | "gpu" => (fn () => M.mxv_gpu ctx_set (mat, mat_futs) vec)
  | "multi-gpu" => (fn () => M.mxv_multi_gpu ctx_set (mat, mat_futs) vec)
  | "hybrid" => (fn () => M.mxv_hybrid ctx_set (mat, mat_futs) vec)
  | _ => Util.die ("unknown -impl " ^ impl)

val result = Benchmark.run "sparse-mxv" bench
val _ = print
  ("result "
   ^ Util.summarizeArraySlice 20 (M.R.fmt (StringCvt.FIX (SOME 1))) result
   ^ "\n")

val () = Option.app (fn datas => GpuData.free datas M.free_mat_on_gpu) mat_futs
(* val () = Futhark.Context.free ctx *)
val () = CtxSet.free ctx_set
