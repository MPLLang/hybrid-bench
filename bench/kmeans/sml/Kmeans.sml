structure Kmeans:
sig
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

  (* This function is completely sequential, but you can apply it to
  different chunks of the input and combine the partial results, as
  long as you remember to weigh the results appropriately.*)
  fun centroidsOf k points membership =
    let
      val d = Points.dims points

      val cluster_sizes = Array.array (k, 0)
      val () = Seq.applyIdx membership (fn (_, c) =>
        Array.update (cluster_sizes, c, 1 + Array.sub (cluster_sizes, c)))

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
        if i = Points.length points then
          ()
        else
          ( addPoint (Seq.nth membership i) (Points.nth points i)
          ; addPoints (i + 1)
          )

      val () = addPoints 0
    in
      Points.fromSeq d (Seq.fromArraySeq (ArraySlice.full cluster_means))
    end

  fun converge k points max_iterations iterations centroids membership =
    if iterations >= max_iterations then
      (iterations, centroids)
    else
      let
        val num_points = Points.length points
        val new_membership =
          Seq.tabulate (findNearestPoint points centroids) num_points
        val new_centroids = centroidsOf k points new_membership
      in
        if Seq.equal op= (membership, new_membership) then
          (iterations + 1, new_centroids)
        else
          converge k points max_iterations (iterations + 1) new_centroids
            new_membership
      end

  fun kmeans k max_iterations points =
    let
      val num_points = Points.length points
      (* Take the first k points as cluster centroids. *)
      val centroids = Points.take points k
      (* Assign points arbitrarily to clusters. *)
      val membership = Seq.tabulate (fn i => i mod k) num_points
    in
      converge k points max_iterations 0 centroids membership
    end
end
