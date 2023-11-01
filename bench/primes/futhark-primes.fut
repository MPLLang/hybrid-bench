-- ==
-- entry: primes
-- input { 100000000i64 }

def ceil_div (n: i64) k = 1 + (n-1)/k

-- compute all primes in the range [lo, hi), except multiples of 2 and 3
--   (this is a classic trick: special case the small primes for performance)
-- output: array of flags, length count (requires count >= hi-lo)
--   for 0 <= i < hi-lo:
--     output[i] = 1 means that lo+i is either prime or a multiple of {2,3}
--     output[i] = 0 means that lo+i is composite
--   for hi-lo <= i < count: output is garbage
-- requires: count >= hi-lo
-- requires: seed_primes contains all primes p for 5 <= p <= sqrt(hi)
--   Note: it's okay for seed_primes to be larger than this, but we need it
--   to _at least_ go up to sqrt(lo+count). It's also okay for 2 and 3 to be in
--   seed_primes, but this is not necessary for correctness and will only slow
--   us down.
entry sieve_segment_except_multiples_of_2_3
  (seed_primes: []i64) (lo: i64) (hi: i64) (count: i64) : [count]u8 =

  -- how many multiples p of fall into the range [lo, hi) ?
  let num_multiples_in_range p =
    let lom = i64.max 2 (ceil_div lo p)
    let him = ceil_div hi p
    in i64.max 0 (him-lom)
  in

  let offsets = scan (+) 0 (map num_multiples_in_range seed_primes)
  let total = if length offsets == 0 then 0 else last offsets
  let offset pi = if pi == 0 then 0 else offsets[pi-1]

  let spread_prime_indices =
    scan (+) 0 (spread total 0 offsets (map (\_ -> 1) offsets))

  let update_position i pi =
    let p = seed_primes[pi]
    let j = i - offset pi   -- this update is for the j^th multiple of p
    let lom = i64.max 2 (ceil_div lo p)
    let x = (lom+j)*p
    in if x%2 == 0 || x%3 == 0 then -1 else x-lo

  let update_positions =
    map2 update_position (indices spread_prime_indices) spread_prime_indices

  in spread count (1: u8) update_positions (map (\_ -> 0) update_positions)


-- num_flags should be >= hi-lo
-- the extra flags will just be a bunch of 1s
-- entry sieve_segment' (seed_primes: []i64) (lo: i64) (hi: i64) (num_flags: i64) : *[num_flags]u8 =
--   let init_flags = tabulate num_flags (\_ -> 1: u8)
--   let (flags, _) =
--     loop (flags: *[num_flags]u8, i) = (init_flags, 0)
--     while i < length seed_primes && 2*seed_primes[i] < hi do
--       let p = seed_primes[i]
--       let lom = i64.max 2 (ceil_div lo p)
--       let him = ceil_div hi p
--       let num_multiples = i64.max 0 (him-lom)
--       let indices = tabulate num_multiples (\k -> (lom+k)*p - lo)
--       let flags' = scatter flags indices (replicate num_multiples (0: u8))
--       in (flags', i+1)
--   in flags


-- entry sieve_segmented_segment (seed_primes: []i64) block_size (lo: i64) (hi: i64) : []u8 =
--   let num_flags = hi-lo
--   let num_blocks = ceil_div num_flags block_size
--   let do_block b =
--     let lo' = lo + b*block_size
--     let hi' = i64.min (lo' + block_size) hi
--     in sieve_segment' seed_primes lo' hi' block_size
--   in
--   take num_flags (flatten (tabulate num_blocks do_block))


entry sieve_primes (seed_primes: []i64) (lo: i64) (hi: i64) : []i64 =
  let seed_primes_except_2_3 =
    if length seed_primes < 2 then [] else seed_primes[2:]
  let flags = sieve_segment_except_multiples_of_2_3 seed_primes_except_2_3 lo hi (hi-lo)
  let ps = filter (\i -> flags[i-lo] == 1 && i%2 != 0 && i%3 != 0) (lo...(hi-1))
  in
  if lo <= 2 && 3 < hi then
    [2,3] ++ ps
  else if lo <= 2 && 2 < hi then
    [2] ++ ps
  else
    ps



entry sieve_primes_segmented (seed_primes: []i64) block_size (lo: i64) (hi: i64) : []i64 =
  let seed_primes_except_2_3 =
    if length seed_primes < 2 then [] else seed_primes[2:]

  let num_flags = hi-lo
  let num_blocks = ceil_div num_flags block_size
  let do_block b : [block_size]u8 =
    let lo' = lo + b*block_size
    let hi' = i64.min (lo' + block_size) hi
    in sieve_segment_except_multiples_of_2_3 seed_primes_except_2_3 lo' hi' block_size

  let flags =
    -- This is equivalent to
    --   tabulate num_blocks do_block
    -- However, doing it this way, with a sequential loop, comes out to be
    -- **significantly** faster... about 36x faster on my machine.
    loop flags : *[num_blocks][block_size]u8 =
      replicate num_blocks (replicate block_size 0u8)
    for b in 0...(num_blocks-1) do
      flags with [b] = do_block b

  let flags = flatten flags

  let ps = filter (\i -> flags[i-lo] == 1 && i%2 != 0 && i%3 != 0) (lo...(hi-1))
  in
  if lo <= 2 && 3 < hi then
    [2,3] ++ ps
  else if lo <= 2 && 2 < hi then
    [2] ++ ps
  else
    ps

------------------------------------------------------------------------------
-- This next bit was an attempt at optimizing the bounds-squaring loop by
-- ensuring that the bounds we choose are exactly:
--   [..., sqrt(sqrt(n)), sqrt(n), n]
-- this would match the normal recursive algorithm, where primes(n) is
-- computed recursively in terms of primes(sqrt(n))
--
-- however, I wasn't able to get any performance improvement from this...
------------------------------------------------------------------------------

-- local
-- def desired_sizes n =
--   let (smallest_size, count) =
--     loop (n, count) = (n, 1) while n > 3 do
--       let n' = i64.f64 (f64.floor (f64.sqrt (f64.i64 n)))
--       in
--       (n', count+1)
--   let (_, _, sizes) =
--     loop (n, i, sizes: *[count]i64) = (n, count, replicate count smallest_size)
--     while n > 3 do
--       let n' = i64.f64 (f64.floor (f64.sqrt (f64.i64 n)))
--       let sizes' = sizes with [i-1] = n
--       in
--       (n', i-1, sizes')
--   in
--   sizes


-- entry primes (n: i64) : []i64 =
--   if n < 2 then [] else
--   if n == 2 then [2] else
--   if n < 5 then [2,3] else
  
--   let sizes = desired_sizes n
--   let sizes = assert (head sizes <= 3) sizes
--   let primes = [2,3]
--   in
--   loop primes for i in 1...(length sizes - 1) do
--     let bound = sizes[i-1]
--     let bound' = sizes[i]
--     in
--     sieve_primes_segmented primes (bound*4) 2 (bound'+1)

------------------------------------------------------------------------------


entry primes (n: i64) : []i64 =

  -- no need to recompute the smallest primes, we can just hardcode these
  let small_primes: [8]i64 = [2,3,5,7,11,13,17,19]

  -- the next small prime would be 23, so a valid initial bound is 22,
  -- i.e., small_primes contains all primes <= 22.
  let init_bound = i64.min n 22
  let init_seeds = filter (\p -> p <= init_bound) small_primes

  let (ps, _) =
    loop (seeds, bound) = (init_seeds, init_bound) while bound < n do
      let bound' = i64.min (bound*bound) n
      let seeds' = sieve_primes_segmented seeds (bound*4) 2 (bound'+1)
      in (seeds', bound')
  in ps