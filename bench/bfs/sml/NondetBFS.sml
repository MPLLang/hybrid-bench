(* nondeterministic direction-optimized BFS, using CAS on outneighbors to
 * construct next frontier. *)
functor NondetBFS(CtxSet: CTX_SET where type ctx = Futhark.ctx) =
struct
  type 'a seq = 'a Seq.t

  (* structure DS = DelayedSeq *)
  structure G = AdjacencyGraph(Int32)
  structure V = G.Vertex

  type vertex = G.vertex

  val sub = Array.sub
  val upd = Array.update

  (* fun sub' msg (a, i) =
    Array.sub (a, i)
    handle Subscript =>
      Util.die
        ("sub error: " ^ msg ^ ": length " ^ Int.toString (Array.length a)
         ^ " index " ^ Int.toString i)
  
  fun upd' msg (a, i, x) =
    Array.update (a, i, x)
    handle Subscript =>
      Util.die
        ("update error: " ^ msg ^ ": length " ^ Int.toString (Array.length a)
         ^ " index " ^ Int.toString i) *)

  val vtoi = V.toInt
  val itov = V.fromInt

  (* fun ASsub s =
    let val (a, i, _) = ArraySlice.base s
    in sub (a, i+s)
    end *)

  val GRAIN = 10000

  fun strip s =
    let val (s', start, _) = ArraySlice.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun tt a b =
    Time.fmt 4 (Time.- (b, a))

  (* =========================================================================
   * gpu-only
   *)

  fun bfs_gpu ctx {diropt} (n, m, graph_fut: Futhark.Opaque.graph.t) (s: vertex) =
    let
      val t1 = Time.now ()

      val parents_fut = Futhark.Entry.bfs ctx
        (if diropt then 0w1 else 0w0, graph_fut, s)

      val t2 = Time.now ()

      val parents = Futhark.Int32Array1.values parents_fut
      val _ = Futhark.Int32Array1.free parents_fut

      val t3 = Time.now ()
    in
      print
        ("gpu bfs (n=" ^ Int.toString n ^ ", m=" ^ Int.toString m ^ "): "
         ^ tt t1 t2 ^ "+" ^ tt t2 t3 ^ "s\n");
      ArraySlice.full parents
    end
    handle Futhark.Error msg => Util.die ("Futhark error: " ^ msg)

  (* =========================================================================
   * cpu-only
   *)

  val bfs_dense_threshold = CommandLineArgs.parseReal "bfs-dense-threshold" 0.2
  val _ = print
    ("bfs-dense-threshold " ^ Real.toString bfs_dense_threshold ^ "\n")

  fun bfs_cpu {diropt: bool} (g: G.graph) (s: vertex) =
    let
      val n = G.numVertices g
      val parent = strip (Seq.tabulate (fn _ => ~1) n)

      (* Choose method of filtering the frontier: either frontier always
       * only consists of valid vertex ids, or it allows invalid vertices and
       * pretends that these vertices are isolated. *)
      fun degree v = G.degree g v
      fun filterFrontier s =
        Seq.filter (fn x => x <> itov (~1)) s
      (*
      fun degree v = if v < 0 then 0 else Graph.degree g v
      fun filterFrontier s = s
      *)

      val denseThreshold = Real.floor
        (Real.fromInt (G.numEdges g) * bfs_dense_threshold)

      fun sumOfOutDegrees frontier =
        SeqBasis.reduce 10000 op+ 0 (0, Seq.length frontier)
          (degree o Seq.nth frontier)
      (* DS.reduce op+ 0 (DS.map degree (DS.fromArraySeq frontier)) *)

      fun shouldProcessDense frontier =
        diropt
        andalso
        let
          val n = Seq.length frontier
          val m = sumOfOutDegrees frontier
        in
          n + m > denseThreshold
        end

      fun bottomUp (frontier: vertex Seq.t) =
        let
          val flags = Seq.tabulate (fn _ => false) n
          val _ = Seq.foreach frontier (fn (_, v) =>
            ArraySlice.update (flags, vtoi v, true))
          fun inFrontier v =
            Seq.nth flags (vtoi v)

          fun processVertex v =
            if sub (parent, v) <> ~1 then
              NONE
            else
              let
                val nbrs = G.neighbors g (itov v)
                val deg = ArraySlice.length nbrs
                fun loop i =
                  if i >= deg then
                    NONE
                  else
                    let
                      val u = Seq.nth nbrs i
                    in
                      if inFrontier u then (upd (parent, v, u); SOME (itov v))
                      else loop (i + 1)
                    end
              in
                loop 0
              end
        in
          ArraySlice.full (SeqBasis.tabFilter 1000 (0, n) processVertex)
        end


      fun topDown (frontier: vertex Seq.t) =
        let
          val nf = Seq.length frontier
          val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf)
            (degree o Seq.nth frontier)
          val mf = sub (offsets, nf)
          val outNbrs: vertex array = ForkJoin.alloc mf

          (* attempt to claim parent of u as v *)
          fun claim (u, v) =
            sub (parent, vtoi u) = ~1
            andalso ~1 = Concurrency.casArray (parent, vtoi u) (~1, v)

          fun visitNeighbors offset v nghs =
            Util.for (0, Seq.length nghs) (fn i =>
              let
                val u = Seq.nth nghs i
              in
                if not (claim (u, v)) then upd (outNbrs, offset + i, itov (~1))
                else upd (outNbrs, offset + i, u)
              end)

          fun visitMany offlo lo hi =
            if lo = hi then
              ()
            else
              let
                val v = Seq.nth frontier offlo
                val voffset = sub (offsets, offlo)
                val k = Int.min (hi - lo, sub (offsets, offlo + 1) - lo)
              in
                if k = 0 then
                  visitMany (offlo + 1) lo hi
                else
                  ( visitNeighbors lo v
                      (Seq.subseq (G.neighbors g v) (lo - voffset, k))
                  ; visitMany (offlo + 1) (lo + k) hi
                  )
              end

          fun parVisitMany (offlo, offhi) (lo, hi) =
            if hi - lo <= GRAIN then
              visitMany offlo lo hi
            else
              let
                val mid = lo + (hi - lo) div 2
                val (i, j) = OffsetSearch.search mid offsets (offlo, offhi)
                val _ =
                  ForkJoin.par
                    ( fn _ => parVisitMany (offlo, i) (lo, mid)
                    , fn _ => parVisitMany (j - 1, offhi) (mid, hi)
                    )
              in
                ()
              end

          (* Either one of the following is correct, but the second one has
           * significantly better granularity control for graphs that have a
           * small number of vertices with huge degree. *)

          val _ = ForkJoin.parfor 100 (0, nf) (fn i =>
            visitMany i (sub (offsets, i)) (sub (offsets, i + 1)))

        (* val _ = parVisitMany (0, nf + 1) (0, mf) *)
        in
          filterFrontier (ArraySlice.full outNbrs)
        end

      fun search (frontier: vertex Seq.t) =
        if Seq.length frontier = 0 then
          ()
        else if shouldProcessDense frontier then
          let val (nextFrontier, tm) = Util.getTime (fn _ => bottomUp frontier)
          in print ("dense  " ^ Time.fmt 4 tm ^ "\n"); search nextFrontier
          end
        else
          let val (nextFrontier, tm) = Util.getTime (fn _ => topDown frontier)
          in print ("sparse " ^ Time.fmt 4 tm ^ "\n"); search nextFrontier
          end

      val _ = upd (parent, vtoi s, s)
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end


  (* =========================================================================
   * hybrid
   *)

  val bfs_sparse_hybrid_threshold = BenchParams.Bfs.sparse_hybrid_threshold
  val bfs_dense_hybrid_split = BenchParams.Bfs.dense_hybrid_split
  val bfs_sparse_hybrid_split = BenchParams.Bfs.sparse_hybrid_split
  val _ = print
    ("bfs-sparse-hybrid-threshold " ^ Real.toString bfs_sparse_hybrid_threshold
     ^ "\n")
  val _ = print
    ("bfs-dense-hybrid-split " ^ Real.toString bfs_dense_hybrid_split ^ "\n")
  val _ = print
    ("bfs-sparse-hybrid-split " ^ Real.toString bfs_sparse_hybrid_split ^ "\n")


  fun bfs_hybrid ctxSet {diropt: bool}
    (g: G.graph, gs_fut: Futhark.Opaque.graph.t GpuData.t) (s: vertex) =
    let
      val n = G.numVertices g
      val parent = strip (Seq.tabulate (fn _ => ~1) n)

      (* Choose method of filtering the frontier: either frontier always
       * only consists of valid vertex ids, or it allows invalid vertices and
       * pretends that these vertices are isolated. *)
      fun degree v = G.degree g v
      fun filterFrontier s =
        Seq.filter (fn x => x <> itov (~1)) s
      (*
      fun degree v = if v < 0 then 0 else Graph.degree g v
      fun filterFrontier s = s
      *)

      val denseThreshold = Real.floor
        (Real.fromInt (G.numEdges g) * bfs_dense_threshold)

      fun sumOfOutDegrees frontier =
        SeqBasis.reduce 10000 op+ 0 (0, Seq.length frontier)
          (degree o Seq.nth frontier)
      (* DS.reduce op+ 0 (DS.map degree (DS.fromArraySeq frontier)) *)

      fun shouldProcessDense frontier =
        diropt
        andalso
        let
          val n = Seq.length frontier
          val m = sumOfOutDegrees frontier
        in
          n + m > denseThreshold
        end

      fun bottomUp (frontier: vertex Seq.t) =
        let
          val t0 = Time.now ()

          val state =
            Seq.map (fn v => if v = ~1 then 0w0 else 0w1 : Word8.word)
              (ArraySlice.full parent)
          val _ = Seq.foreach frontier (fn (_, v) =>
            ArraySlice.update (state, vtoi v, 0w2))
          fun inFrontier v =
            Seq.nth state (vtoi v) = 0w2
          fun isVisited v =
            Seq.nth state v <> 0w0

          val t1 = Time.now ()

          val state_futs = GpuData.initialize ctxSet (fn ctx =>
            Futhark.Word8Array1.new ctx state (Seq.length state))

          val t2 = Time.now ()

          val _ = print
            ("prep dense state: " ^ tt t0 t1 ^ "+" ^ tt t1 t2 ^ "s\n")

          fun keep_vertex (v: int) =
            if isVisited v then
              false
            else
              let
                val nbrs = G.neighbors g (itov v)
                val deg = ArraySlice.length nbrs
                fun loop i =
                  if i >= deg then
                    false
                  else
                    let
                      val u = Seq.nth nbrs i
                    in
                      if inFrontier u then (upd (parent, v, u); true)
                      else loop (i + 1)
                    end
              in
                loop 0
              end

          val nextFrontier =
            HybridBasis.filter_hybrid_with_cleanup bfs_dense_hybrid_split 10000
              (0, n)
              ( fn v => itov v

              , fn v => keep_vertex v

              , fn device =>
                  fn (vlo, vhi) =>
                    let
                      val t0 = Time.now ()
                      val ctx = CtxSet.choose ctxSet device
                      val g_fut = GpuData.choose gs_fut device
                      val state_fut = GpuData.choose state_futs device
                      val (vertices_fut, parents_fut) =
                        Futhark.Entry.bfs_round_dense_kernel ctx
                          (g_fut, state_fut, itov vlo, itov vhi)
                      val t1 = Time.now ()
                      val vertices =
                        ArraySlice.full
                          (Futhark.Int32Array1.values vertices_fut)
                      val parents =
                        ArraySlice.full (Futhark.Int32Array1.values parents_fut)
                      val _ = Futhark.Int32Array1.free vertices_fut
                      val _ = Futhark.Int32Array1.free parents_fut
                    in
                      (vlo, vhi, vertices, parents, t0, t1)
                    end

              , fn (vlo, vhi, vertices, parents, t0, t1) =>
                  let
                    val t2 = Time.now ()
                    val _ =
                      ForkJoin.parfor 1000 (0, Seq.length vertices) (fn i =>
                        let
                          val v = Seq.nth vertices i
                          val p = Seq.nth parents i
                        in
                          upd (parent, vtoi v, p)
                        end)
                    val t3 = Time.now ()
                  in
                    print
                      ("gpu dense (n=" ^ Int.toString (vhi - vlo) ^ "): "
                       ^ tt t0 t1 ^ "+" ^ tt t1 t2 ^ "+" ^ tt t2 t3 ^ "s\n");
                    vertices
                  end
              )
        in
          GpuData.free state_futs Futhark.Word8Array1.free;
          nextFrontier
        end


      fun topDown (frontier: vertex Seq.t) =
        let
          val nf = Seq.length frontier
          val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf)
            (degree o Seq.nth frontier)
          val mf = sub (offsets, nf)
          val outNbrs: vertex array = ForkJoin.alloc mf


          val visited_futs =
            if
              nf + mf
              < Real.floor (Real.fromInt n * bfs_sparse_hybrid_threshold)
            then
              NONE
            else
              let
                val t0 = Time.now ()
                val visited =
                  Seq.map (fn v => if v = ~1 then 0w0 else 0w1 : Word8.word)
                    (ArraySlice.full parent)
                val t1 = Time.now ()
                val _ = print ("prep sparse state: " ^ tt t0 t1 ^ "s\n")
                val visited_futs = GpuData.initialize ctxSet (fn ctx =>
                  Futhark.Word8Array1.new ctx visited n)
                val t2 = Time.now ()
                val _ = print ("copy over sparse state: " ^ tt t1 t2 ^ "s\n")
              in
                SOME visited_futs
              end


          (* attempt to claim parent of u as v *)
          fun claim (u, v) =
            sub (parent, vtoi u) = ~1
            andalso ~1 = Concurrency.casArray (parent, vtoi u) (~1, v)

          fun visitNeighbors offset v nghs =
            Util.for (0, Seq.length nghs) (fn i =>
              let
                val u = Seq.nth nghs i
              in
                if not (claim (u, v)) then upd (outNbrs, offset + i, itov (~1))
                else upd (outNbrs, offset + i, u)
              end)

          fun visitMany offlo lo hi =
            if lo = hi then
              ()
            else
              let
                val v = Seq.nth frontier offlo
                val voffset = sub (offsets, offlo)
                val k = Int.min (hi - lo, sub (offsets, offlo + 1) - lo)
              in
                if k = 0 then
                  visitMany (offlo + 1) lo hi
                else
                  ( visitNeighbors lo v
                      (Seq.subseq (G.neighbors g v) (lo - voffset, k))
                  ; visitMany (offlo + 1) (lo + k) hi
                  )
              end


          fun loop i j =
            if
              j - i = 0
            then
              ()

            else if
              j - i = 1
            then
              visitMany i (sub (offsets, i)) (sub (offsets, i + 1))

            else if
              (j - i < 10000
               andalso sub (offsets, j) - sub (offsets, i) < 100000)
            then
              ForkJoin.parfor 100 (i, j) (fn k =>
                visitMany k (sub (offsets, k)) (sub (offsets, k + 1)))

            else
              let
                val mid =
                  i
                  + Real.floor (Real.fromInt (j - i) * bfs_sparse_hybrid_split)
              in
                ForkJoin.par (fn _ => loop_choose i mid, fn _ => loop mid j);
                ()
              end

          and loop_choose i j =
            let
              val choice_result =
                if
                  (j - i < 10000
                   andalso sub (offsets, j) - sub (offsets, i) < 100000)
                then
                  (loop i j; NONE)
                else
                  ForkJoin.choice
                    { prefer_cpu = fn _ => (loop i j; NONE)
                    , prefer_gpu = fn (device: string) =>
                        let
                          val t0 = Time.now ()
                          val ctx = CtxSet.choose ctxSet device
                          val g_fut = GpuData.choose gs_fut device
                          val visited_fut =
                            GpuData.choose (valOf visited_futs) device
                          val frontier_piece = Seq.subseq frontier (i, j - i)
                          val frontier_piece_fut =
                            Futhark.Int32Array1.new ctx frontier_piece (j - i)
                          val t1 = Time.now ()
                          val (vertices_fut, parents_fut) =
                            Futhark.Entry.bfs_round_sparse_kernel ctx
                              (g_fut, visited_fut, frontier_piece_fut)
                          val t2 = Time.now ()
                          val vertices = Futhark.Int32Array1.values vertices_fut
                          val parents = Futhark.Int32Array1.values parents_fut
                          val _ = Futhark.Int32Array1.free vertices_fut
                          val _ = Futhark.Int32Array1.free parents_fut
                          val _ = Futhark.Int32Array1.free frontier_piece_fut
                        in
                          SOME
                            ( ArraySlice.full vertices
                            , ArraySlice.full parents
                            , t0
                            , t1
                            , t2
                            )
                        end
                    }
            in
              case choice_result of
                NONE => ()
              | SOME (vertices, parents, t0, t1, t2) =>
                  let
                    val t3 = Time.now ()

                    val lo = sub (offsets, i)
                    val hi = sub (offsets, j)

                    val _ =
                      ForkJoin.parfor 1000 (0, Seq.length vertices) (fn k =>
                        let
                          val v = Seq.nth vertices k
                          val p = Seq.nth parents k
                        in
                          if not (claim (v, p)) then
                            upd (outNbrs, lo + k, itov (~1))
                          else
                            upd (outNbrs, lo + k, v)
                        end)

                    val _ =
                      ForkJoin.parfor 5000 (Seq.length vertices, hi - lo)
                        (fn k => upd (outNbrs, lo + k, itov (~1)))

                    val t4 = Time.now ()
                  in
                    print
                      ("gpu sparse (n=" ^ Int.toString (j - i) ^ ", m="
                       ^ Int.toString (hi - lo) ^ "): " ^ tt t0 t1 ^ "+"
                       ^ tt t1 t2 ^ "+" ^ tt t2 t3 ^ "+" ^ tt t3 t4 ^ "s\n");
                    ()
                  end
            end

          val _ =
            case visited_futs of
              NONE =>
                ForkJoin.parfor 100 (0, nf) (fn i =>
                  visitMany i (sub (offsets, i)) (sub (offsets, i + 1)))
            | SOME _ => loop 0 nf

          val _ =
            Option.app (fn datas => GpuData.free datas Futhark.Word8Array1.free)
              visited_futs
        in
          filterFrontier (ArraySlice.full outNbrs)
        end


      fun search (frontier: vertex Seq.t) =
        if Seq.length frontier = 0 then
          ()
        else if shouldProcessDense frontier then
          let val (nextFrontier, tm) = Util.getTime (fn _ => bottomUp frontier)
          in print ("dense  " ^ Time.fmt 4 tm ^ "\n"); search nextFrontier
          end
        else
          let val (nextFrontier, tm) = Util.getTime (fn _ => topDown frontier)
          in print ("sparse " ^ Time.fmt 4 tm ^ "\n"); search nextFrontier
          end

      val _ = upd (parent, vtoi s, s)
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end

end
