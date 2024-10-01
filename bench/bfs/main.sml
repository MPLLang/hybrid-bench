structure CLA = CommandLineArgs
structure CtxSet = CtxSetFn (structure F = Futhark)

val () = print "Initialising Futhark contexts... "
val devices = String.fields (fn c => c = #",") (CLA.parseString "devices" "")
val ctxSet = CtxSet.fromList devices
val () = print "Done!\n"

val _ = print ("devices: " ^ String.concatWith "," devices ^ "\n")

structure BFS = NondetBFS(CtxSet)
structure G = BFS.G

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

val (graph, tm) = Util.getTime (fn _ => G.parseFile filename)
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")

(* val ((graph_fut, free_graph_fut), tm) = Util.getTime (fn _ => *)
val (graphs_fut, tm) = Util.getTime (fn _ =>
  GpuData.initialize ctxSet (fn ctx =>
    let
      val (offsets, _, edges) = graph
      val n = G.numVertices graph
      val m = G.numEdges graph

      val offsets_i32 = Seq.map Int32.fromInt offsets
      val offsets_fut = Futhark.Int32Array1.new ctx offsets_i32 n
      val edges_fut = Futhark.Int32Array1.new ctx edges m
      val graph_fut =
        Futhark.Opaque.graph.new ctx {offsets = offsets_fut, edges = edges_fut}

      fun free () =
        ( Futhark.Int32Array1.free offsets_fut
        ; Futhark.Int32Array1.free edges_fut
        ; Futhark.Opaque.graph.free graph_fut
        )
    in
      (graph_fut, free)
    end))
val _ = print ("futhark loaded graph in " ^ Time.fmt 4 tm ^ "s\n")


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
    | "gpu" =>
        (fn () =>
           let
             val dev = 0
             val ctx = CtxSet.choose ctxSet dev
             val (graph_fut, _) = GpuData.choose graphs_fut dev
           in
             BFS.bfs_gpu ctx {diropt = diropt}
               (G.numVertices graph, G.numEdges graph, graph_fut) s
           end)
    | "hybrid" =>
        let
          val graphs_fut' = Seq.map (fn (dev, (g, f)) => (dev, g)) graphs_fut
        in
          fn () =>
            BFS.bfs_hybrid ctxSet {diropt = diropt} (graph, graphs_fut') s
        end
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

val _ = GpuData.free graphs_fut (fn (_, free_fn) => free_fn ())
val _ = CtxSet.free ctxSet
