-- Adapted from https://github.com/diku-dk/futhark-benchmarks/tree/master/pbbs/breadthFirstSearch


-- function `expand` from: 
import "lib/github.com/diku-dk/segmented/segmented"

type vertex = i32
type graph [n] [m] = {offsets: [n]i32, edges: [m]vertex}
type tree_edge = {vertex: vertex, parent: vertex}


def remove_duplicates [k] n (queue: [k]tree_edge): []tree_edge = 
  let verts = map (\q -> i64.i32 q.vertex) queue
  let indexes = iota k 
  let H = hist i64.min k n verts indexes
  in map2 (\i j -> H[i] == j) verts indexes 
    |> zip queue
    |> filter (.1)
    |> map (.0)


def update_parents [n] [k] (parents: *[n]vertex) (selected_edges: [k]tree_edge): *[n]vertex =
  let qVerts = map (\q -> i64.i32 q.vertex) selected_edges
  let qParents = map (\q -> q.parent) selected_edges
  in scatter parents qVerts qParents


def vertex_degree [n] [m] (g: graph[n][m]) (v: vertex): i64 =
  let extended = g.offsets ++ [i32.i64 m]
  in i64.i32 (extended[v + 1] - extended[v])


def vertex_neighbors [n] [m] (g: graph[n][m]) (v: vertex): []vertex =
  let lo = i64.i32 g.offsets[v]
  let hi = lo + vertex_degree g v
  in g.edges[lo:hi]


def get_ith_neighbor_cancel_visited [n] [m] (g: graph[n][m]) is_visited (v: vertex) (i: i64) : tree_edge =
  let neighbor = g.edges[i64.i32 g.offsets[v] + i]
  in if !(is_visited neighbor)
  then {vertex = neighbor, parent = v}
  else {vertex = -1, parent = -1}


def bfs_round_sparse [n] [m]
    (g: graph[n][m])
    (is_visited: vertex -> bool)
    (frontier: []vertex) : *[]tree_edge =
  let new_frontier = expand
    (\v -> vertex_degree g v)
    (\v i -> get_ith_neighbor_cancel_visited g is_visited v i)
    frontier
  let filtered_new_frontier = filter (\q -> q.parent != -1) new_frontier
  let deduplicated_new_frontier = remove_duplicates n filtered_new_frontier
  in deduplicated_new_frontier


-- NOTE: this only works if the graph is undirected and symmetrized. In general,
-- we would need to use the in-neighbors of every vertex. In a symmetrized
-- graph, the in-neighbors and out-neighbors of every vertex are the same.
def bfs_round_dense [n] [m]
    (g: graph [n][m])
    (is_visited: vertex -> bool)
    (frontier: []vertex) : *[]tree_edge =
  let in_frontier =
    spread n false (map i64.i32 frontier) (map (\_ -> true) frontier)
  let select_edge_for_vertex v : tree_edge =
    if is_visited v then {vertex = v, parent = -1} else
    let best_in_neighbor =
      vertex_neighbors g v
      |> map (\u -> if in_frontier[u] then u else -1i32)
      |> reduce i32.max (-1i32)
    in
    {vertex = v, parent = best_in_neighbor}
  in
  tabulate n (\v -> select_edge_for_vertex (i32.i64 v))
  |> filter (\e -> e.parent != -1)



-- -- this works, but is slow...
-- def bfs_round_dense [n] [m]
--     (g: graph[n][m])
--     (is_visited: vertex -> bool)
--     (frontier: []vertex) : *[]tree_edge =
--   let in_frontier =
--     spread n false (map i64.i32 frontier) (map (\_ -> true) frontier)
--   let unvisited_nonisolated =
--     filter (\v -> vertex_degree g v > 0 && !(is_visited v))
--       (map i32.i64 (iota n))
--   let count = length unvisited_nonisolated
--   let unvisited_nonisolated = sized count unvisited_nonisolated
--   let parents =
--     expand_reduce
--       (\v -> vertex_degree g v)
--       (\v i ->
--         let neighbor = g.edges[i64.i32 g.offsets[v] + i]
--         in
--         if in_frontier[neighbor] then neighbor else -1i32)
--       i32.max
--       (-1i32)
--       unvisited_nonisolated
--   let parents = sized count parents
--   in
--   zip unvisited_nonisolated parents
--   |> map (\(v,p) -> {vertex = v, parent = p})
--   |> filter (\e -> e.parent != -1)


def bfs_round [n] [m]
    (do_diropt: bool)
    (g: graph[n][m])
    (is_visited: vertex -> bool)
    (frontier: []vertex) : *[]tree_edge =
  let above_threshold () =
    let threshold = m / 4
    let edges_count = reduce (+) 0 (map (vertex_degree g) frontier)
    in 
    n + edges_count > threshold
  in
  if do_diropt && above_threshold () then
    bfs_round_dense g is_visited frontier
  else
    bfs_round_sparse g is_visited frontier


def bfs_loop [n] [m] (do_diropt: bool) (g: graph[n][m]) (parents: *[n]vertex) (frontier: []vertex): [n]vertex =
  let (parents, _) =
    loop (parents: *[n]vertex, frontier: []vertex)
    while length frontier > 0 do
      let selected_edges =
        bfs_round do_diropt g (\v -> parents[v] != -1) frontier
      let parents = update_parents parents selected_edges
      let frontier' = map (.vertex) selected_edges
      in (parents, frontier')
  in parents


------------------------------------------------------------------------------



-- do_diropt is 1/0 for do/don't direction-optimize
entry bfs [n] [m] (do_diropt: u8) (g: graph[n][m]) start =
  let parents = replicate n (-1)
  let parents = parents with [start] = start
  in bfs_loop (do_diropt == 1) g parents [start]


-- visited[v] = 0 if unvisited, 1 if already visited
-- frontier = [v1, v2, ...]: vertices to expand now

entry bfs_round_sparse_kernel [n] [m] (g: graph[n][m]) (visited: [n]u8) (frontier: []vertex) : ([]vertex, []vertex) =
  let selected_edges = bfs_round_sparse g (\v -> visited[v] == 1) frontier
  in (map (.vertex) selected_edges, map (.parent) selected_edges)

-- state[v] = 0 if unvisited
--            1 if visited in a previous round
--            2 if in the current frontier
entry bfs_round_dense_kernel [n] [m] (g: graph[n][m]) (state: [n]u8) (vlo: i32) (vhi: i32) : ([]vertex, []vertex) =
  let is_visited v = state[v] != 0
  let in_frontier v = state[v] == 2

  let select_edge_for_vertex (v: vertex) : tree_edge =
    if is_visited v then {vertex = v, parent = -1} else
    let best_in_neighbor =
      vertex_neighbors g v
      |> map (\u -> if in_frontier u then u else -1i32)
      |> reduce i32.max (-1i32)
    in
    {vertex = v, parent = best_in_neighbor}
  
  let selected_edges =
    map select_edge_for_vertex (vlo...(vhi-1))
    |> filter (\e -> e.parent != -1)

  in (map (.vertex) selected_edges, map (.parent) selected_edges)