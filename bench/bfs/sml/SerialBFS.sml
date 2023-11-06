structure SerialBFS =
struct

  structure G = AdjacencyGraph(Int32)
  structure V = G.Vertex
  type vertex = G.vertex

  val sub = Array.sub
  val upd = Array.update

  val vtoi = V.toInt
  val itov = V.fromInt

  fun bfs g s =
    let
      fun neighbors v = G.neighbors g v
      fun degree v = G.degree g v

      val n = G.numVertices g
      val m = G.numEdges g

      val queue: G.vertex array = ForkJoin.alloc (m + 1)
      val parents: G.vertex array = Array.array (n, ~1)

      fun search (lo, hi) =
        if lo >= hi then
          lo
        else
          let
            val v = sub (queue, lo)
            fun visit (hi', u) =
              if sub (parents, vtoi u) >= 0 then hi'
              else (upd (parents, vtoi u, v); upd (queue, hi', u); hi' + 1)
          in
            search (lo + 1, Seq.iterate visit hi (neighbors v))
          end

      val _ = upd (parents, vtoi s, s)
      val _ = upd (queue, 0, s)
      val numVisited = search (0, 1)
    in
      ArraySlice.full parents
    end

end
