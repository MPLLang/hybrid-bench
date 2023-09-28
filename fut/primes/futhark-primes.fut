
def ceil_div (n: i64) k = 1 + (n-1)/k

-- compute all primes in the range [lo, hi)
-- requires that seed_primes contains all primes p <= sqrt(hi)
--   Note: it's okay for seed_primes to be larger than this, but we need it
--   to _at least_ go up to sqrt(hi)
-- output: array of flags, length hi-lo
--   output[lo+i] = 1 means that i is prime
--   output[lo+i] = 0 means that i is composite
entry sieve_segment (seed_primes: []i64) (lo: i64) (hi: i64) : []u8 =

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
    in (lom+j)*p - lo

  let update_positions =
    map2 update_position (indices spread_prime_indices) spread_prime_indices

  in spread (hi - lo) (1: u8) update_positions (map (\_ -> 0) update_positions)


entry primes (n: i64) : []i64 =
  if n < 2 then
    []
  else
  let (ps, _) =
    loop (primesSoFar, bound) = ([2], 2) while bound < n do
      let newBound = i64.min (bound*bound) n
      let flags = sieve_segment primesSoFar 2 (newBound+1)
      let newPrimes = filter (\i -> flags[i-2] == 1) (2...newBound)
      in (newPrimes, newBound)
  in ps