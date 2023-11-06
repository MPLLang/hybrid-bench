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


def vertex_degree [n] [m] (g: graph[n][m]) (v: vertex): i64 =
  let extended = g.offsets ++ [i32.i64 m]
  in i64.i32 (extended[v + 1] - extended[v])


def get_ith_neighbor_cancel_visited [n] [m] (g: graph[n][m]) is_visited (v: vertex) (i: i64) : tree_edge =
  let neighbor = g.edges[i64.i32 g.offsets[v] + i]
  in if !(is_visited neighbor)
  then {vertex = neighbor, parent = v}
  else {vertex = -1, parent = -1}


def bfs_round [n] [m]
    (g: graph[n][m])
    (is_visited: vertex -> bool)
    (frontier: []vertex) : (*[]tree_edge) =
  let new_frontier = expand
    (\v -> vertex_degree g v)
    (\v i -> get_ith_neighbor_cancel_visited g is_visited v i)
    frontier
  let filtered_new_frontier = filter (\q -> q.parent != -1) new_frontier
  let deduplicated_new_frontier = remove_duplicates n filtered_new_frontier
  in deduplicated_new_frontier


def bfs_loop [n] [m] (g: graph[n][m]) (parents: *[n]vertex) (frontier: *[]vertex): [n]vertex =
  let (parents, _) =
    loop (parents: *[n]vertex, frontier)
    while length frontier > 0 do
      let selected_edges = bfs_round g (\v -> parents[v] != -1) frontier
      let parents = update_parents parents selected_edges
      in (parents, map (.vertex) selected_edges)
  in parents


------------------------------------------------------------------------------



entry bfs [n] [m] (g: graph[n][m]) start =
  let parents = replicate n (-1)
  let parents = parents with [start] = start
  in bfs_loop g parents [start]


-- visited[v] = 0 if unvisited, 1 if already visited
-- frontier = [v1, v2, ...]: vertices to expand now
entry bfs_round_kernel [n] [m] (g: graph[n][m]) (visited: [n]u8) (frontier: []vertex) : []tree_edge = 
  bfs_round g (\v -> visited[v] == 1) frontier