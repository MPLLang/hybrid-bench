structure CLA = CommandLineArgs
structure M = MatCOO (structure I = Int64 structure R = Real32)

val inputFile = CLA.parseString "input" ""

val (mat, tm) = Util.getTime (fn _ =>
  if inputFile = "" then Util.die ("missing -input FILE.mtx")
  else M.fromFile inputFile)
val _ = print ("parsed input in " ^ Time.fmt 4 tm ^ "s\n")

val num_rows = M.height mat
val vec = Seq.tabulate (fn _ => M.R.fromInt 1) num_rows

val result = Benchmark.run "sparse-mxv" (fn _ => M.mxv mat vec)
val _ = print
  ("result "
   ^ Util.summarizeArraySlice 8 (M.R.fmt (StringCvt.FIX (SOME 1))) result ^ "\n")
