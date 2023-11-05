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
  let new_centres = centroids_of k points' new_membership
  let weight = f64.i64 len / f64.i64 n
  in map (map (*weight)) new_centres

entry kmeans [n][d]
        (k: i32) (max_iterations: i32)
        (points: [n][d]f64): (i32, [][]f64) =
  let k = i64.i32 k

  -- Assign arbitrary initial cluster centres.
  let cluster_centres = take k points
  -- Also assign points arbitrarily to clusters.
  let membership = map i32.i64 (map (%k) (iota n))
  let changed = true
  let i = 0
  let (_,cluster_centres,_,i) =
    loop (membership, cluster_centres, changed, i)
    while changed && i < max_iterations do
      -- For each point, find the cluster with the closest centroid.
      let new_membership = map (find_nearest_point cluster_centres) points
      -- Then, find the new centres of the clusters.
      let new_centres = centroids_of k points new_membership
      let delta = i32.sum (map (\b -> if b then 0 else 1)
                               (map2 (==) membership new_membership))
      in (new_membership, new_centres, delta != 0, i+1)
  in (i, cluster_centres)
