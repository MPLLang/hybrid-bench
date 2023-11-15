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
of chunks (this can be done by communicating just a start index and
length), compute the centroids for each chunk separately, then combine
them at the end. (This works because the centroids are just averages,
and computing an average is a near-homomorphism.)

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
  val kmeans': int -> int -> Points.t -> int * Points.t
  val kmeans'': int -> int -> Points.t -> int * Points.t
  val kmeansNewHybrid:
    (Points.t -> {kernel: (int * int) -> real array Seq.t, after: unit -> unit})
    -> int
    -> int
    -> Points.t
    -> int * Points.t
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
    Real.abs (x - y) <= Real.abs (x * tolerance)

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


    fun centroidsOf' points k membership =
      let
        val d = Points.dims points
        val n = Points.length points

        val cluster_counts =
          Hist.hist 5000 {combine = op+, neutral = 0, num_bins = k}
            {lo = 0, hi = n, get_bin = Seq.nth membership, get_elem = fn _ => 1}

        val cluster_sums: (real Seq.t) Seq.t =
          Hist.hist 100
            { combine = Seq.zipWith Real.+
            , neutral = Seq.tabulate (fn _ => 0.0) d
            , num_bins = k
            }
            { lo = 0
            , hi = n
            , get_bin = Seq.nth membership
            , get_elem = Points.nth points
            }

        val cluster_means =
          Seq.zipWith (fn (count, sum) => Seq.map (fn r => r / real count) sum)
            (cluster_counts, cluster_sums)
      in
        Points.fromSeq d (Seq.flatten cluster_means)
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

    fun newCentroids' points centroids =
      let
        val k = Points.length centroids
        val num_points = Points.length points
        val new_membership =
          Seq.tabulate (findNearestPoint points centroids) num_points
      in
        centroidsOf' points k new_membership
      end


    val hist_cpu_grain = CommandLineArgs.parseInt "hist-cpu-grain" 1000
    val _ = print ("hist-cpu-grain " ^ Int.toString hist_cpu_grain ^ "\n")

    fun newCentroids'' points centroids =
      let
        val k = Points.length centroids
        val n = Points.length points
        val d = Points.dims points

        (* Use elements of dimension d+1, and store the count in the extra
         * dimension (specifically index 0). We can then compute the means by
         * replacing each
         *   [s0, s1, s2, ..., s(d)]
         * with
         *   [s1/s0, s2/s0, ..., s(d)/s0]
         *)
        val cluster_results =
          Hist.inplace_hist hist_cpu_grain
            { combine = Seq.zipWith Real.+
            , fresh_neutral = fn () => Seq.tabulate (fn _ => 0.0) (d + 1)
            , num_bins = k
            }
            { lo = 0
            , hi = n
            , get_bin = findNearestPoint points centroids
            , modify_bin = fn i =>
                fn binval =>
                  let
                    val pt = Points.nth points i
                  in
                    ArraySlice.update
                      (binval, 0, 1.0 + ArraySlice.sub (binval, 0));

                    Util.for (0, d) (fn j =>
                      ArraySlice.update
                        ( binval
                        , j + 1
                        , ArraySlice.sub (binval, j + 1) + Seq.nth pt j
                        ))
                  end
            }

        val means =
          Seq.map (fn x => Seq.map (fn r => r / Seq.nth x 0) (Seq.drop x 1))
            cluster_results
      in
        Points.fromSeq d (Seq.flatten means)
      end

    val hist_gpu_grain = CommandLineArgs.parseInt "hist-gpu-grain" 100000
    val hist_gpu_split = CommandLineArgs.parseReal "hist-gpu-split" 0.75

    val _ = print ("hist-gpu-grain " ^ Int.toString hist_gpu_grain ^ "\n")
    val _ = print ("hist-gpu-split " ^ Real.toString hist_gpu_split ^ "\n")

    fun newCentroidsHybrid gpu points centroids =
      let
        val k = Points.length centroids
        val n = Points.length points
        val d = Points.dims points

        val {kernel, after} = gpu centroids

        (* Use elements of dimension d+1, and store the count in the extra
         * dimension (specifically index 0). We can then compute the means by
         * replacing each
         *   [s0, s1, s2, ..., s(d)]
         * with
         *   [s1/s0, s2/s0, ..., s(d)/s0]
         *)
        val cluster_results =
          Hist.inplace_hist_hybrid_two_level hist_cpu_grain hist_gpu_grain
            hist_gpu_split
            { combine_inplace = fn (a, b) =>
                Util.for (0, d + 1) (fn i =>
                  Array.update (a, i, Array.sub (a, i) + Array.sub (b, i)))
            , fresh_neutral = fn () => Array.tabulate (d + 1, fn _ => 0.0)
            , num_bins = k
            }
            { lo = 0
            , hi = n
            , get_bin = findNearestPoint points centroids
            , gpu = kernel
            , modify_bin = fn i =>
                fn binval =>
                  let
                    val pt = Points.nth points i
                  in
                    Array.update (binval, 0, 1.0 + Array.sub (binval, 0));
                    Util.for (0, d) (fn j =>
                      Array.update
                        ( binval
                        , j + 1
                        , Array.sub (binval, j + 1) + Seq.nth pt j
                        ))
                  end
            }

        val _ = after ()

        val means =
          Seq.map
            (fn x => let val x = ArraySlice.full x
                     in Seq.map (fn r => r / Seq.nth x 0) (Seq.drop x 1)
                     end) cluster_results
      in
        Points.fromSeq d (Seq.flatten means)
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

  fun loop f max_iterations points centroids i =
    if i >= max_iterations then
      (i, centroids)
    else
      let
        val centroids' = f points centroids
      in
        if
          Seq.equal closeEnough
            (Points.toSeq centroids, Points.toSeq centroids')
        then (i + 1, centroids')
        else loop f max_iterations points centroids' (i + 1)
      end

  (* uses Hist.hist *)
  fun kmeans' k max_iterations points =
    loop newCentroids' max_iterations points (Points.take points k) 0

  (* uses Hist.inplace_hist_hybrid *)
  fun kmeans'' k max_iterations points =
    loop newCentroids'' max_iterations points (Points.take points k) 0

  (* uses Hist.inplace_hist_hybrid *)
  fun kmeansNewHybrid gpu k max_iterations points =
    loop (newCentroidsHybrid gpu) max_iterations points (Points.take points k) 0
end
