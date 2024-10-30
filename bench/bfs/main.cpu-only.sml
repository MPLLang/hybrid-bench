structure CLA = CommandLineArgs
structure BFS = NondetBFS_CPU
structure G = BFS.G

structure R32 = struct open MLton.Real32 open Real32 structure W = Word32 end
structure M =
  MatCOO (structure I = Int32 structure R = R32)

fun mtx_to_graph (M.Mat {width, height, row_indices, col_indices, values}) : BFS.G.graph =
  let
    val _ = if width = height then () else raise Fail "mtx_to_graph: matrix not square"
    val num_vertices = M.I.toInt width
    val nnz = Seq.length row_indices

    val starts =
      SeqBasis.filter 5000 (0, nnz) (fn i => i) (fn i =>
        i = 0 orelse Seq.nth row_indices (i-1) <> Seq.nth row_indices i)

    val row_lengths = SeqBasis.tabulate 5000 (0, num_vertices) (fn _ => 0)
    val _ =
      ForkJoin.parfor 5000 (0, Array.length starts) (fn i =>
        let
          val lo = Array.sub (starts, i)
          val hi = if i = Array.length starts - 1 then nnz else Array.sub (starts, i+1)
          val len = hi-lo
          val row_idx = Seq.nth row_indices (Array.sub (starts, i))
        in
          Array.update (row_lengths, M.I.toInt row_idx, hi-lo)
        end)

    val (offsets, _) = Seq.scan op+ 0 (ArraySlice.full row_lengths)
  in
    (offsets, ArraySlice.full row_lengths, col_indices)
  end

val dontDirOpt = CLA.parseFlag "no-dir-opt"
val diropt = not dontDirOpt

val source = CLA.parseInt "source" 0
val doCheck = CLA.parseFlag "check"

val impl = CLA.parseString "impl" "cpu"

(*
val N = CLA.parseInt "N" 10000000
val D = CLA.parseInt "D" 10

val (graph, tm) = Util.getTime (fn _ => G.randSymmGraph N D)
val _ = print ("generated graph in " ^ Time.fmt 4 tm ^ "s\n")
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")
*)

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val (graph, tm) = Util.getTime (fn _ =>
  if String.isSuffix ".mtx.bin" filename orelse String.isSuffix ".mtx" filename then
    mtx_to_graph (M.fromFile filename)
  else
    G.parseFile filename)

val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")

val _ =
  if not diropt then
    ()
  else
    let
      val (_, tm) = Util.getTime (fn _ =>
        if G.parityCheck graph then
          ()
        else
          TextIO.output
            ( TextIO.stdErr
            , "WARNING: parity check failed; graph might not be symmetric "
              ^ "or might have duplicate- or self-edges\n"
            ))
      val _ = print ("parity check in " ^ Time.fmt 4 tm ^ "s\n")
    in
      ()
    end


val do_bfs =
  let
    val s = G.Vertex.fromInt source
  in
    case impl of
      "cpu" => (fn () => BFS.bfs_cpu {diropt = diropt} graph s)
    | _ => Util.die ("unknown impl: " ^ impl ^ "\n")
  end


val P: G.vertex Seq.t = Benchmark.run "running bfs" (fn _ => do_bfs ())

val numVisited = SeqBasis.reduce 10000 op+ 0 (0, Seq.length P) (fn i =>
  if Seq.nth P i >= 0 then 1 else 0)
val _ = print ("visited " ^ Int.toString numVisited ^ "\n")

fun numHops P hops v =
  let
    val vv = G.Vertex.toInt v
  in
    if hops > Seq.length P then ~2
    else if Seq.nth P vv = ~1 then ~1
    else if Seq.nth P vv = v then hops
    else numHops P (hops + 1) (Seq.nth P vv)
  end

val maxHops = SeqBasis.reduce 100 Int.max ~3 (0, G.numVertices graph) (fn i =>
  numHops P 0 (G.Vertex.fromInt i))
val _ = print ("max dist " ^ Int.toString maxHops ^ "\n")

fun check () =
  let
    val (P', serialTime) = Util.getTime (fn _ =>
      SerialBFS.bfs graph (G.Vertex.fromInt source))

    val correct =
      Seq.length P = Seq.length P'
      andalso
      SeqBasis.reduce 10000 (fn (a, b) => a andalso b) true (0, Seq.length P)
        (fn i =>
           numHops P 0 (G.Vertex.fromInt i) = numHops P' 0 (G.Vertex.fromInt i))
  in
    print ("serial finished in " ^ Time.fmt 4 serialTime ^ "s\n");
    print ("correct? " ^ (if correct then "yes" else "no") ^ "\n")
  end

val _ = if doCheck then check () else ()
