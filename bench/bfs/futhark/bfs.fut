-- Adapted from https://github.com/diku-dk/futhark-benchmarks/tree/master/pbbs/breadthFirstSearch


-- function `expand` from: 
import "lib/github.com/diku-dk/segmented/segmented"

type queuePair = {vertex: i32, parent: i32}

def remove_duplicates [nQueue]  (nVerts) (queue: [nQueue]queuePair): []queuePair = 
  let verts = map (\q -> i64.i32 q.vertex) queue
  let indexes = iota nQueue 
  let H = hist i64.min nQueue nVerts verts indexes
  in map2 (\i j -> H[i] == j) verts indexes 
    |> zip queue
    |> filter (.1)
    |> map (.0)

-- Set the parent of each vertex in the queue. The queue must not contain duplicates
def update_parents [nVerts] (parents: *[nVerts]i32) (queue: []queuePair): *[nVerts]i32 =
  let qVerts = map (\q -> i64.i32 q.vertex) queue
  let qParents = map (\q -> q.parent) queue
  in scatter parents qVerts qParents

def edges_of_vertex (verts: []i32) (nEdges: i32) (edge: queuePair): i64 =
  let extended = verts ++ [nEdges]
  in i64.i32 (extended[edge.vertex + 1] - extended[edge.vertex])

def get_ith_edge_from_vert (verts: []i32) (edges: []i32) (parents: []i32) (q: queuePair) (i: i64) : queuePair =
  -- Get the i'th vertex of the edge
  let currentVert = edges[i64.i32 verts[q.vertex] + i]
  -- If it's an unseen vertex, add it to the queue with the current vertex being the parent
  -- Else return a placeholder that we filter later
  in if (parents[currentVert] == -1)
  then {vertex = currentVert, parent = q.vertex}
  else {vertex = -1, parent = -1}


def bfs_round [n] [m]
    (vert_offsets: [n]i32) (edges: [m]i32)
    (parents: *[n]i32, frontier: *[]queuePair) : (*[n]i32, *[]queuePair) =

  -- Setup a function that takes a queuePair, and returns how many vertexes goes out of it
  let get_edges_of_vert_fun = edges_of_vertex vert_offsets (i32.i64 m)
  -- Setup a function that takes a queuePair and an int i, and returns the vertex pointed to by the i'th edge
  let get_ith_edge_from_vert_fun = get_ith_edge_from_vert vert_offsets edges parents

  -- Get the vertexes in the next layer
  let new_frontier = expand (get_edges_of_vert_fun) (get_ith_edge_from_vert_fun) frontier
  -- Remove empty placeholders ({-1, -1} queuePairs)
  let filtered_new_frontier = filter (\q -> q.parent != -1) new_frontier
  -- Remove duplicates from the queue
  let deduplicated_new_frontier = remove_duplicates n filtered_new_frontier

  in (update_parents parents deduplicated_new_frontier, deduplicated_new_frontier)


def BFS [nVerts] [nEdges] (verts: [nVerts]i32) (edges: [nEdges]i32) (parents: *[nVerts]i32) (queue: *[]queuePair): [nVerts]i32 =
  -- Loop until we get an empty queue
  let (parents, _) =
    loop (parents, queue)
    while length queue > 0 do
    bfs_round verts edges (parents, queue)
  in parents


------------------------------------------------------------------------------



def main [nVerts] [nEdges] (vertexes_enc: [nVerts]i32) (edges_enc: [nEdges]i32) start =
  let parents = replicate nVerts (-1)
  let queue = [{vertex = start, parent = start}]
  let parents = update_parents parents queue

  in BFS vertexes_enc edges_enc parents queue