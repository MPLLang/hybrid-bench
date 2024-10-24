-- Based on work by Frederik Berthelsen, Kasper Erik
-- Schmidt-Christensen, Niels Hansen, and Mikkel Kragh Mathiesen.
--
-- Uses single precision floats.
--
-- It is a bit inefficient that we have to sort at the end to get the
-- right ordering, but the flattened quickhull does not otherwise
-- preserve it.

module type euclidean_space = {
  type dist
  type point

  -- val dist_to_f64 : dist -> f64
  val zero_dist : dist
  val dist_less : dist -> dist -> bool

  val point_eq : point -> point -> bool
  val point_less : point -> point -> bool

  val less_by_x: point -> point -> bool
  val less_by_y: point -> point -> bool

  val signed_dist_to_line : point -> point -> point -> dist
}

module type convex_hull = {
  module space : euclidean_space

  type point = space.point

  val compute [n] : [n]point -> ([]point,[]point)
}

module naive_space_f64 : euclidean_space with point = {x:f64, y:f64} = {
  type dist = f64
  type point = {x:f64, y:f64}

  def zero_dist = 0f64
  def dist_less (x : dist) (y : dist) = x < y

  -- def dist_to_f64 x = x

  def point_eq (p : point) (q : point) =
    p.x == q.x && p.y == q.y
  def point_less (p : point) (q : point) =
    p.x < q.x || (p.x == q.x && p.y < q.y)

  def less_by_x (p: point) (q: point) = p.x < q.x
  def less_by_y (p: point) (q: point) = p.y < q.y

  def sqr (x : f64) = x * x
  def ssqr (x : f64) = f64.abs x * x

  def signed_dist_to_line (p : point) (q : point) (r : point) =
    let ax = q.x - p.x
    let ay = q.y - p.y
    let bx = r.x - p.x
    let by = r.y - p.y
    in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)
}


-- SAM_NOTE: naive_space would be better as a module-level function for DRY
module naive_space_f32 : euclidean_space with point = {x:f32, y:f32} = {
  type dist = f32
  type point = {x:f32, y:f32}

  def zero_dist = 0f32
  def dist_less (x : dist) (y : dist) = x < y

  -- def dist_to_f32 x = x

  def point_eq (p : point) (q : point) =
    p.x == q.x && p.y == q.y
  def point_less (p : point) (q : point) =
    p.x < q.x || (p.x == q.x && p.y < q.y)

  def less_by_x (p: point) (q: point) = p.x < q.x
  def less_by_y (p: point) (q: point) = p.y < q.y

  def sqr (x : f32) = x * x
  def ssqr (x : f32) = f32.abs x * x

  def signed_dist_to_line (p : point) (q : point) (r : point) =
    let ax = q.x - p.x
    let ay = q.y - p.y
    let bx = r.x - p.x
    let by = r.y - p.y
    in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)
}


module indexed_space (S: euclidean_space)
       : euclidean_space with point = (S.point, i32) = {
  type dist = S.dist
  type point = (S.point, i32)

  def zero_dist = S.zero_dist
  def dist_less = S.dist_less

  -- def dist_to_f64 = S.dist_to_f64

  def point_eq (a: point) (b: point) = S.point_eq a.0 b.0
  def point_less (a: point) (b: point) = S.point_less a.0 b.0

  def less_by_x (a: point) (b: point) = S.less_by_x a.0 b.0
  def less_by_y (a: point) (b: point) = S.less_by_y a.0 b.0

  def signed_dist_to_line (p: point) (q: point) (r: point) =
    S.signed_dist_to_line p.0 q.0 r.0
}

module mk_quickhull (S : euclidean_space) = {
  module space = S
  open space

  def expand_hull [num_segs] [num_points]
                  (segs : [num_segs](point, point))
                  (points : [num_points](i32, point))
    : ([](point, point), [](i32, point)) =
    let dists = map
                (\(seg_ix, p) ->
                   signed_dist_to_line segs[seg_ix].0 segs[seg_ix].1 p)
                points
    let max (i,id) (j,jd) =
      if dist_less jd id then (i,id) else (j,jd)
    let extrema_ix = reduce_by_index
                     (replicate num_segs (-1,zero_dist)) max (-1,zero_dist)
                     (map (i64.i32 <-< (.0)) points) (zip (iota num_points) dists)
    let segs' = tabulate num_segs
                         (\i -> [(segs[i].0, points[extrema_ix[i].0].1),
                                 (points[extrema_ix[i].0].1, segs[i].1)])
                |> flatten
    let eval_point (ix, (seg_ix, p)) =
      if extrema_ix[seg_ix].0 == ix then (-1, p) else
      let (a, b) = segs[seg_ix]
      let q = points[extrema_ix[seg_ix].0].1
      let daq = signed_dist_to_line a q p
      let dqb = signed_dist_to_line q b p
      in if dist_less zero_dist daq then (seg_ix * 2, p)
         else if dist_less zero_dist dqb then (seg_ix * 2 + 1, p)
         else (-1, p)
    let points' =
      filter ((>= 0) <-< (.0))
      <| map eval_point (zip (iota num_points) points)
    in (segs', points')

  def extract_empty_segments [num_segs] [num_points]
                             (hull : [](point))
                             (segs : [num_segs](point, point))
                             (points : [num_points](i32, point))
      : ([](point), [](point, point), [](i32, point)) =
    let point_ixs = map (i64.i32 <-< (.0)) points
    let segs_inhabited =
      reduce_by_index
      (replicate num_segs 0i32) (+) 0 point_ixs (replicate num_points 1)
      |> map (> 0)
    let (segs_true, segs_false) = partition (.1) (zip segs segs_inhabited)
    let segs_indicator = map i32.bool segs_inhabited
    let new_segs_ix =
      scan (+) 0 segs_indicator |> map2 (\i n -> n - i) segs_indicator
    let hull' = hull ++ map (.0.0) segs_false
    let segs' = map (.0) segs_true
    let points' = map (\(seg_ix, p) -> (new_segs_ix[seg_ix], p)) points
    in (hull', segs', points')

  def semihull (start : point) (end : point) (points : []point) =
    if null points then [start]
    else
      (loop (hull, segs, points) =
         ([], [(start, end)], map (\p -> (0, p)) points)
       while !(null points) do
       let (segs', points') = expand_hull segs points
       in extract_empty_segments hull segs' points')
      |> (.0)

  def filter_then_semihull (start : point) (end : point) (points : []point) =
    semihull start end (filter (\p -> dist_less zero_dist (signed_dist_to_line start end p)) points)

  def min_by_x p q = if less_by_x p q then p else q
  def max_by_x p q = if less_by_x p q then q else p
  def min_by_y p q = if less_by_y p q then p else q
  def max_by_y p q = if less_by_y p q then q else p

  def min_max_point ps =
    ( reduce min_by_x ps[0] ps
    , reduce max_by_x ps[0] ps
    , reduce min_by_y ps[0] ps
    , reduce max_by_y ps[0] ps
    )

  def point_furthest_from_line l r pts =
    let with_dist = map (\p -> (p, signed_dist_to_line l r p)) pts
    in reduce (\a b -> if dist_less a.1 b.1 then b else a) with_dist[0] with_dist

  def compute (ps : []point) =
    if length ps <= 3 then (ps, []) else
    let leftmost = reduce min_by_x ps[0] ps
    let rightmost = reduce max_by_x ps[0] ps
    let (_, upper_points, lower_points) =
      partition2
      (\p -> point_eq p leftmost || point_eq p rightmost)
      (\p -> dist_less zero_dist (signed_dist_to_line leftmost rightmost p))
      ps
    let upper_hull = semihull leftmost rightmost upper_points
    let lower_hull = semihull rightmost leftmost lower_points
    in (upper_hull, lower_hull)
}


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- f64 entrypoints
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

module space_f64 = (indexed_space naive_space_f64)
module naive_quickhull_f64 = mk_quickhull space_f64
type point_f64 = space_f64.point


entry semihull_f64 points l r idxs =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let start = p l
  let end = p r
  in naive_quickhull_f64.semihull start end (map p idxs)
     |> map (.1)
     |> filter (!=l) -- Remove starting point.


entry filter_then_semihull_f64 points l r idxs =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let start = p l
  let end = p r
  in naive_quickhull_f64.filter_then_semihull start end (map p idxs)
     |> map (.1)
     |> filter (!=l)


entry point_furthest_from_line_f64 [k] (points: [k][2]f64) (l: i32) (r: i32) (idxs: []i32) : i32 =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let ((_, i), _) = naive_quickhull_f64.point_furthest_from_line (p l) (p r) (map p idxs)
  in i


entry min_max_point_in_range_f64 [k] (points: [k][2]f64) lo hi : (i32, i32, i32, i32) =
  let ps = take (hi-lo) (drop lo points)
  let ps = map2 (\i p -> ({x=f64.f64 p[0], y=f64.f64 p[1]}, i32.i64 (lo + i))) (indices ps) ps
  let (l, r, b, t) = naive_quickhull_f64.min_max_point ps
  in (l.1, r.1, b.1, t.1)


-- select points above the line (l, r) and then compute the semihull of these
entry top_level_filter_then_semihull_f64 points (l: i32) (r : i32) : []i32 =
  let p i = ({x=points[i,0], y=points[i,1]}, i32.i64 i)
  let start = p (i64.i32 l)
  let end = p (i64.i32 r)
  in naive_quickhull_f64.filter_then_semihull start end (map p (indices points))
     |> map (.1)
     |> filter (!=l)


entry top_level_points_above_in_range_f64 [k] (points: [k][2]f64) (lo:i64) (hi:i64) (l:i32) (r:i32) : []i32 =
  let p i = {x=points[i,0], y=points[i,1]}
  let keep (i: i32) =
    naive_space_f64.dist_less
      naive_space_f64.zero_dist
      (naive_space_f64.signed_dist_to_line (p l) (p r) (p i))
  let lo = i32.i64 lo
  let hi = i32.i64 hi
  in filter keep (lo...(hi-1))


entry points_above_f64 [k] (points: [k][2]f64) (idxs: []i32) (l:i32) (r:i32) : []i32 =
  let p i = {x=points[i,0], y=points[i,1]}
  let keep (i: i32) =
    naive_space_f64.dist_less
      naive_space_f64.zero_dist
      (naive_space_f64.signed_dist_to_line (p l) (p r) (p i))
  in filter keep idxs


entry quickhull_f64 [k] (ps : [k][2]f64) : []i32 =
  let ps' = map2 (\i p -> ({x=f64.f64 p[0], y=f64.f64 p[1]}, i32.i64 i))
                 (indices ps) ps
  let (convex_upper, convex_lower) = naive_quickhull_f64.compute ps'
  in map (.1) (convex_upper ++ convex_lower)



------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- f32 entrypoints
--
-- SAM_NOTE: should probably just use module-level functions for DRY... 
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


module space_f32 = (indexed_space naive_space_f32)
module naive_quickhull_f32 = mk_quickhull space_f32
type point_f32 = space_f32.point


entry semihull_f32 points l r idxs =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let start = p l
  let end = p r
  in naive_quickhull_f32.semihull start end (map p idxs)
     |> map (.1)
     |> filter (!=l) -- Remove starting point.


entry filter_then_semihull_f32 points l r idxs =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let start = p l
  let end = p r
  in naive_quickhull_f32.filter_then_semihull start end (map p idxs)
     |> map (.1)
     |> filter (!=l)


entry point_furthest_from_line_f32 [k] (points: [k][2]f32) (l: i32) (r: i32) (idxs: []i32) : i32 =
  let p i = ({x=points[i,0], y=points[i,1]}, i)
  let ((_, i), _) = naive_quickhull_f32.point_furthest_from_line (p l) (p r) (map p idxs)
  in i


entry min_max_point_in_range_f32 [k] (points: [k][2]f32) lo hi : (i32, i32, i32, i32) =
  let ps = take (hi-lo) (drop lo points)
  let ps = map2 (\i p -> ({x=f32.f32 p[0], y=f32.f32 p[1]}, i32.i64 (lo + i))) (indices ps) ps
  let (l, r, b, t) = naive_quickhull_f32.min_max_point ps
  in (l.1, r.1, b.1, t.1)


-- select points above the line (l, r) and then compute the semihull of these
entry top_level_filter_then_semihull_f32 points (l: i32) (r : i32) : []i32 =
  let p i = ({x=points[i,0], y=points[i,1]}, i32.i64 i)
  let start = p (i64.i32 l)
  let end = p (i64.i32 r)
  in naive_quickhull_f32.filter_then_semihull start end (map p (indices points))
     |> map (.1)
     |> filter (!=l)


entry top_level_points_above_in_range_f32 [k] (points: [k][2]f32) (lo:i64) (hi:i64) (l:i32) (r:i32) : []i32 =
  let p i = {x=points[i,0], y=points[i,1]}
  let keep (i: i32) =
    naive_space_f32.dist_less
      naive_space_f32.zero_dist
      (naive_space_f32.signed_dist_to_line (p l) (p r) (p i))
  let lo = i32.i64 lo
  let hi = i32.i64 hi
  in filter keep (lo...(hi-1))


entry points_above_f32 [k] (points: [k][2]f32) (idxs: []i32) (l:i32) (r:i32) : []i32 =
  let p i = {x=points[i,0], y=points[i,1]}
  let keep (i: i32) =
    naive_space_f32.dist_less
      naive_space_f32.zero_dist
      (naive_space_f32.signed_dist_to_line (p l) (p r) (p i))
  in filter keep idxs


entry quickhull_f32 [k] (ps : [k][2]f32) : []i32 =
  let ps' = map2 (\i p -> ({x=f32.f32 p[0], y=f32.f32 p[1]}, i32.i64 i))
                 (indices ps) ps
  let (convex_upper, convex_lower) = naive_quickhull_f32.compute ps'
  in map (.1) (convex_upper ++ convex_lower)