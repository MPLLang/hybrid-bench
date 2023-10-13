structure CLA = CommandLineArgs

structure R32 = struct open MLton.Real32 open Real32 structure W = Word32 end
structure M = MatCOO (structure I = Int64 structure R = R32)

val inputFile = CLA.parseString "input" ""
val outputBinFile = CLA.parseString "output-bin" ""

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
val _ = print ("num rows " ^ Int.toString num_rows ^ "\n")
val _ = print ("num cols " ^ Int.toString num_cols ^ "\n")
val _ = print ("num vals " ^ Int.toString num_vals ^ "\n")

val vec = Seq.tabulate (fn _ => M.R.fromInt 1) num_rows

val result = Benchmark.run "sparse-mxv" (fn _ => M.mxv mat vec)
val _ = print
  ("result "
   ^ Util.summarizeArraySlice 20 (M.R.fmt (StringCvt.FIX (SOME 1))) result
   ^ "\n")
