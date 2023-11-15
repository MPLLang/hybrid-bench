-- Our convergence criterion is that the centroids don't change too
-- much anymore.
def tolerance : f64 = 0.001
def close x y =
  f64.abs (x - y) <= f64.abs (x * tolerance)

def euclid_dist_2 [d] (pt1: [d]f64) (pt2: [d]f64): f64 =
  f64.sum (map (\x->x*x) (map2 (-) pt1 pt2))

def closest_point (p1: (i32,f64)) (p2: (i32,f64)): (i32,f64) =
  if p1.1 < p2.1 then p1 else p2

def find_nearest_point [k][d] (pts: [k][d]f64) (pt: [d]f64): i32 =
  let (i, _) = foldl (\acc (i, p) -> closest_point acc (i32.i64 i, euclid_dist_2 pt p))
                     (0, f64.inf)
                     (zip (indices pts) pts)
  in i

def centroids_of [n][d] (k: i64) (points: [n][d]f64) (membership: [n]i32): [k][d]f64 =
  let points_in_clusters =
    hist (+) 0 k (map i64.i32 membership) (replicate n 1)

  let cluster_sums =
    hist (map2 (+)) (replicate d 0) k
         (map i64.i32 membership)
         points

  in map2 (\point n -> map (/f64.i32 (if n == 0 then 1 else n)) point)
          cluster_sums points_in_clusters

entry centroids_chunk [k][n][d] (start: i64) (len: i64) (points: [n][d]f64) (centroids: [k][d]f64) =
  let points' = points[start:start+len]
  let new_membership = map (find_nearest_point centroids) points'
  let new_centroids = centroids_of k points' new_membership
  let weight = f64.i64 len / f64.i64 n
  in map (map (*weight)) new_centroids

entry kmeans [n][d]
        (k: i64) (max_iterations: i32)
        (points: [n][d]f64): (i32, [][]f64) =
  -- Assign arbitrary initial cluster centroids.
  let centroids = take k points
  let changed = true
  let i = 0
  let (centroids,_,i) =
    loop (centroids, changed, i)
    while changed && i < max_iterations do
      -- For each point, find the cluster with the closest centroid.
      let membership = map (find_nearest_point centroids) points
      -- Then, find the new centroids of the clusters.
      let new_centroids = centroids_of k points membership
      let changed = not (and (map2 close (flatten centroids) (flatten new_centroids)))
      in (new_centroids, changed, i+1)
  in (i, centroids)

entry histogram [k][n][d] (all_points: [n][d]f64) (centroids: [k][d]f64) (start: i64) (len: i64) : ([k]i64, [k][d]f64) =
  let points: [len][d]f64 = sized len (all_points[start:start+len])
  let membership = map (find_nearest_point centroids) points

  let points_in_clusters: [k]i64 =
    hist (+) 0 k (map i64.i32 membership) (replicate len 1)

  let cluster_sums: [k][d]f64 =
    hist (map2 (+)) (replicate d 0) k
         (map i64.i32 membership)
         points

  --in map2 (\count sum -> [f64.i64 count] ++ sum) points_in_clusters cluster_sums
  in (points_in_clusters, cluster_sums)

------------------------------------------------------------------------------

def kmeans_bench [n][d] (points: [n][d]f64) =
  kmeans 5 10 points |> (.1)

-- ==
-- entry: kmeans_bench
-- random input { [1000000][10]f64 }
