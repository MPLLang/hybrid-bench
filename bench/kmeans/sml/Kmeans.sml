(*

K-means clustering is based on maintaining two data structures:

The points: an array of n d-dimensional points. Constant throughout
the algorithm.

The centroids: an array of k d-dimensional points. Changes
repeatedly.

The algorithm proceeds as follows:

  1. Assign initial arbitrary centroids (the first 'k' points is a
     good start).

  2. Compute new centroids:

    2a. For each point, determine the index of the closest centroid.

    2b. For each centroid index, find the points that are closest to
        that, and compute their mean. This is essentially a kind of
        histogram, and produces a new array of centroids.

  3. Repeat steps 2-3 until some convergence criterion is reached.

As 'k' is usually much less than 'n', the trick that allows efficient
hybridization is to keep the points on both the CPU and GPU at all
times, and only pass the much smaller centroids array around.
Specifically, in step 2 we divide the points into an arbitrary number
of chunks, compute the centroids for each chunk separately, then
combine them at the end. (This works because the centroids are just
averages, and computing an average is a near-homomorphism.)

The algorithm has some reasonable assumptions: there are no duplicate
points, and no clusters are ever empty. (The last is guaranteed by
initialising the centroids to be specific points.)

*)

structure Kmeans:
sig
  val centroidsChunkCPU: Points.t -> int * int * Points.t -> Points.t
  val kmeansHybrid: (int * int * Points.t -> Points.t)
                    -> int
                    -> int
                    -> Points.t
                    -> int * Points.t
  val kmeans: int -> int -> Points.t -> int * Points.t
end =
struct
  fun distance x y =
    let
      val d = Seq.length x
      fun loop acc i =
        if i = d then
          acc
        else
          let val diff = Seq.nth x i - Seq.nth y i
          in loop (acc + diff * diff) (i + 1)
          end
    in
      loop 0.0 0
    end

  (* Our convergence criterion is that the centroids don't change too
  much anymore. *)
  val tolerance = 0.001
  fun closeEnough (x, y) =
    Real.abs (x - y) < tolerance

  local
    fun findNearestPoint points centroids point_i =
      let
        val k = Points.length centroids
        val point = Points.nth points point_i
        fun loop i (min_i, min_dist) =
          if i = k then
            min_i
          else
            let
              val dist = distance point (Points.nth centroids i)
            in
              if dist < min_dist then loop (i + 1) (i, dist)
              else loop (i + 1) (min_i, min_dist)
            end
      in
        loop 1 (0, distance point (Points.nth centroids 0))
      end

    fun centroidsOf points k membership =
      let
        val d = Points.dims points
        val n = Points.length points

        val cluster_sizes = Array.array (k, 0)

        fun countPoint c =
          Array.update (cluster_sizes, c, 1 + Array.sub (cluster_sizes, c))

        fun countPoints i =
          if i = n then
            ()
          else
            let val c = Seq.nth membership i
            in countPoint c; countPoints (i + 1)
            end

        val () = countPoints 0

        val cluster_means = Array.array (k * d, 0.0)

        fun addPoint c p =
          let
            val m = real (Array.sub (cluster_sizes, c))
            fun loop i =
              if i = d then
                ()
              else
                ( Array.update
                    ( cluster_means
                    , c * d + i
                    , (Seq.nth p i / m + Array.sub (cluster_means, c * d + i))
                    )
                ; loop (i + 1)
                )
          in
            loop 0
          end

        fun addPoints i =
          if i = n then
            ()
          else
            ( addPoint (Seq.nth membership i) (Points.nth points i)
            ; addPoints (i + 1)
            )

        val () = addPoints 0
      in
        Points.fromSeq d (Seq.fromArraySeq (ArraySlice.full cluster_means))
      end
  in
    (* This function is completely sequential, but you can apply it to
    different chunks of the input and combine the partial results, as
    long as you remember to weigh the results appropriately.*)
    fun newCentroids points centroids =
      let
        val k = Points.length centroids
        val num_points = Points.length points
        val new_membership =
          Seq.tabulate (findNearestPoint points centroids) num_points
      in
        centroidsOf points k new_membership
      end
  end

  fun newCentroidsChunked (centroidsChunk: (int * int * Points.t -> Points.t)) n
    centroids =
    let
      val chunk_size = 100000
      val d = Points.dims centroids
      val k = Points.length centroids
      val num_chunks = (n + chunk_size - 1) div chunk_size
      val chunk_means: Points.t array = ForkJoin.alloc num_chunks
      fun onChunk i =
        let
          val start = i * chunk_size
          val len = Int.min (n - start, chunk_size)
        in
          Array.update (chunk_means, i, centroidsChunk (start, len, centroids))
        end
    in
      ForkJoin.parfor 1 (0, num_chunks) onChunk;
      (Seq.reduce Points.add (Points.zero (d, k))
         (Seq.fromArraySeq (ArraySlice.full chunk_means)))
    end

  fun converge (centroidsChunk: (int * int * Points.t -> Points.t)) n k
    max_iterations iterations centroids =
    if iterations >= max_iterations then
      (iterations, centroids)
    else
      let
        val new_centroids = newCentroidsChunked centroidsChunk n centroids
      in
        if
          Seq.equal closeEnough
            (Points.toSeq centroids, Points.toSeq new_centroids)
        then
          (iterations + 1, new_centroids)
        else
          converge centroidsChunk n k max_iterations (iterations + 1)
            new_centroids
      end

  fun kmeansHybrid centroidsChunk k max_iterations points =
    let val n = Points.length points
    in converge centroidsChunk n k max_iterations 0 (Points.take points k)
    end

  fun centroidsChunkCPU points (start, len, centroids) =
    let
      val weight = real len / real (Points.length points)
    in
      Points.scale weight
        (newCentroids (Points.slice points (start, len)) centroids)
    end

  fun kmeans k max_iterations points =
    kmeansHybrid (centroidsChunkCPU points) k max_iterations points
end
