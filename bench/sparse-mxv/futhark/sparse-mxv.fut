



def do_sparse_mxv [n] (rows: [n]i32) (cols: [n]i32) (vals: [n]f32) (vec: []f32) =
  let row_lo = head rows
  let row_hi = 1 + last rows
  let num_bins = row_hi - row_lo
  in
  hist (+) 0f32 (i64.i32 num_bins)
    (map (\r -> i64.i32 (r - row_lo)) rows)
    (map2 (\col x -> x * vec[col]) cols vals)


def do_sparse_mxv' [n] (rows: [n]i32) (cols: [n]i32) (vals: [n]f32) (vec: []f32) block_size =
  let row_lo = head rows
  let row_hi = 1 + last rows
  let num_rows = i64.i32 (row_hi - row_lo)

  let num_blocks = 1 + (n-1) / block_size

  let acc =
    loop acc: *[num_rows]f32 = replicate num_rows 0f32
    for b in 0...(num_blocks-1) do
      let blo = b * block_size
      let bhi = i64.min n (blo + block_size)

      let brow_lo = i64.i32 (rows[blo] - row_lo)
      let brow_hi = 1 + i64.i32 (rows[bhi-1] - row_lo)

      let old_brow_lo_val = acc[brow_lo]
      let old_brow_hi_val =
        if brow_hi-1 > brow_lo then acc[brow_hi-1] else 0.0

      let acc = 
        acc with [brow_lo:brow_hi] =
          do_sparse_mxv rows[blo:bhi] cols[blo:bhi] vals[blo:bhi] vec

      let acc =
        acc with [brow_lo] = acc[brow_lo] + old_brow_lo_val
      
      let acc =
        acc with [brow_hi-1] = acc[brow_hi-1] + old_brow_hi_val

      in
      acc
  
  in
  acc


entry sparse_mxv [n] (rows: [n]i32) (cols: [n]i32) (vals: [n]f32) (vec: []f32) (start: i64) (stop: i64) (block_size: i64) : (u8, f32, []f32, f32) =
  let output_vec =
    do_sparse_mxv' rows[start:stop] cols[start:stop] vals[start:stop] vec block_size
    -- do_sparse_mxv rows[start:stop] cols[start:stop] vals[start:stop] vec
  let sz = length output_vec
  in
  if sz == 1 then
    (1, output_vec[0], [], 0)
  else
    (0, output_vec[0], output_vec[1 : sz - 1], output_vec[sz - 1])