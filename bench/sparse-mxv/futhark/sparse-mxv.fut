
entry sparse_mxv [n]
  (row_indices: [n]i32)
  (col_indices: [n]i32)
  (vals: [n]f32) 
  (vec: []f32)
  : []f32
  =
  let row_lo = head row_indices
  let row_hi = 1 + last row_indices
  let num_bins = row_hi - row_lo
  in
  hist (+) 0f32 (i64.i32 num_bins)
    (map (\r -> i64.i32 (r - row_lo)) row_indices)
    (map2 (\col x -> x * vec[col]) col_indices vals)