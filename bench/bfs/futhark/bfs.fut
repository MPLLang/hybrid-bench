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


def update_parents [n] (parents: *[n]vertex) (selected_edges: []tree_edge): *[n]vertex =
  let qVerts = map (\q -> i64.i32 q.vertex) selected_edges
  let qParents = map (\q -> q.parent) selected_edges
  in scatter parents qVerts qParents


def edges_of_vertex [n] [m] (g: graph[n][m]) (edge: tree_edge): i64 =
  let extended = g.offsets ++ [i32.i64 m]
  in i64.i32 (extended[edge.vertex + 1] - extended[edge.vertex])


def get_ith_edge_from_vert [n] [m] (g: graph[n][m]) is_visited (q: tree_edge) (i: i64) : tree_edge =
  let currentVert = g.edges[i64.i32 g.offsets[q.vertex] + i]
  in if !(is_visited currentVert)
  then {vertex = currentVert, parent = q.vertex}
  else {vertex = -1, parent = -1}


def bfs_round [n] [m]
    (g: graph[n][m])
    (is_visited: vertex -> bool)
    (frontier: *[]tree_edge) : (*[]tree_edge) =

  let new_frontier = expand
    (\edge -> edges_of_vertex g edge)
    (\i -> get_ith_edge_from_vert g is_visited i)
    frontier

  let filtered_new_frontier = filter (\q -> q.parent != -1) new_frontier
  let deduplicated_new_frontier = remove_duplicates n filtered_new_frontier

  in deduplicated_new_frontier


def bfs_loop [n] [m]
  (g: graph[n][m])
  (parents: *[n]vertex)
  (queue: *[]tree_edge): [n]vertex
  =
  -- Loop until we get an empty queue
  let (parents, _) =
    loop (parents: *[n]vertex, queue)
    while length queue > 0 do
      let queue = bfs_round g (\v -> parents[v] != -1) queue
      let parents = update_parents parents queue
      in (parents, queue)
  in parents


------------------------------------------------------------------------------



entry bfs [n] [m] (g: graph[n][m]) start =
  let parents = replicate n (-1)
  let queue = [{vertex = start, parent = start}]
  let parents = update_parents parents queue

  in bfs_loop g parents queue


-- visited[v] = 0 if unvisited, 1 if already visited
-- frontier = [v1, v2, ...]: vertices to expand now
-- entry bfs_round_kernel [n] [m]
--   (offsets: [n]i32) (edges: [m]vertex)
--   (visited: [n]u8) (frontier: []vertex) : []tree_edge
--   =
--   let 