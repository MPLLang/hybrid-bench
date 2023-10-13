structure CLA = CommandLineArgs
structure M = MatCOO (structure I = Int64 structure R = Real32)

val inputFile = CLA.parseString "input" ""

val mat =
  if inputFile = "" then Util.die ("missing -input FILE.mtx")
  else M.fromFile inputFile

(* val mat = M.example1 *)

(* can add more rows as desired here, doesn't matter *)
val extra_empty_rows_at_bottom = 2
val num_rows = extra_empty_rows_at_bottom + M.row_hi mat
val vec = Seq.tabulate (fn _ => M.R.fromInt 1) num_rows

val result = Benchmark.run "sparse-mxv" (fn _ => M.mxv mat vec)
val _ = print
  ("result "
   ^ Util.summarizeArraySlice 8 (M.R.fmt (StringCvt.FIX (SOME 1))) result ^ "\n")
