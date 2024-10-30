structure NondetBFS_CPU =
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

  val GRAIN = 1000

  fun strip s =
    let val (s', start, _) = ArraySlice.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun tt a b =
    Time.fmt 4 (Time.- (b, a))

  (* ====================================================================== *)

  val bfs_dense_threshold = CommandLineArgs.parseReal "bfs-dense-threshold" 0.2
  val _ = print
    ("bfs-dense-threshold " ^ Real.toString bfs_dense_threshold ^ "\n")

  (* structure G =
  struct
    open G

    fun degree ((offsets, _, compact_nbrs): G.graph) v =
      let
        val v = vtoi v
        val lo = Seq.nth offsets v
        val hi = if v = Seq.length offsets - 1 then Seq.length compact_nbrs else Seq.nth offsets (v+1)
      in
        hi-lo
      end
  end *)

  fun bfs_cpu {diropt: bool} (g: G.graph) (s: vertex) =
    let
      val n = G.numVertices g
      val parent = strip (Seq.tabulate (fn _ => itov ~1) n)

      (* Choose method of filtering the frontier: either frontier always
       * only consists of valid vertex ids, or it allows invalid vertices and
       * pretends that these vertices are isolated. *)

      fun degree v = G.degree g v
      fun filterFrontier s =
        Seq.filter (fn x => x <> itov (~1)) s
      
      (* fun degree v = if v < 0 then 0 else G.degree g v
      fun filterFrontier s = s *)
     

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

          (* val _ = print (Int.toString nf ^ " " ^ Int.toString mf ^ "\n") *)

          (* attempt to claim parent of u as v *)
          fun claim (u, v) =
            sub (parent, vtoi u) = itov ~1
            andalso itov ~1 = Concurrency.casArray (parent, vtoi u) (itov ~1, v)

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
          in (*print ("dense  " ^ Time.fmt 4 tm ^ "\n");*) search nextFrontier
          end
        else
          let val (nextFrontier, tm) = Util.getTime (fn _ => topDown frontier)
          in (*print ("sparse " ^ Time.fmt 4 tm ^ "\n");*) search nextFrontier
          end

      val _ = upd (parent, vtoi s, s)
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end

end