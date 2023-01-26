val rawFutScan =
  _import "futScan" public : Int32.int * Int32.int array * Int32.int array -> unit;

fun futScan (input: Int32.int array) =
  let
    val n = Array.length input
    val output = ForkJoin.alloc n
  in
    rawFutScan (Int32.fromInt n, input, output);
    output
  end

structure CLA = CommandLineArgs
val n = CLA.parseInt "n" (1000 * 1000 * 1000)

val (input, tm) = Util.getTime (fn _ =>
  SeqBasis.tabulate 10000 (0, n) (fn _ => Int32.fromInt 1))
val _ = print ("generate input: " ^ Time.fmt 3 tm ^ "\n")

val result = Benchmark.run "futScan" (fn _ => futScan input)
val _ = print ("result " ^ Util.summarizeArray 8 Int32.toString result ^ "\n")
