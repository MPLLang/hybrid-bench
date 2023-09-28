
entry sieve (primes: []i64) (numFlags: i64) : []bool =
  -- Each prime p has to perform `numMultiples p` updates across the output
  -- flags. So, first step is to compute, for each prime, its "starting
  -- position" in the update sequence. This is its offset.
  let numMultiples p = (numFlags-1) / p - 1
  let offsets = scan (+) 0 (map numMultiples primes)
  let total = if length offsets == 0 then 0 else last offsets
  let offset pi = if pi == 0 then 0 else offsets[pi-1]

  -- Next, for each update-to-be-performed, we compute the index of the
  -- prime that contributes that update.
  let spreadPrimeIndices =
    scan (+) 0 (spread total 0 offsets (map (\_ -> 1) offsets))

  -- compute the position of the ith overall update. The prime that contributes
  -- this update is primes[pi]
  let updatePosition i pi =
    let p = primes[pi]
    let j = i - offset pi
    in (j+2)*p

  let updatePositions =
    map2 updatePosition (indices spreadPrimeIndices) spreadPrimeIndices

  in spread numFlags true updatePositions (map (\_ -> false) updatePositions)


entry primes (n: i64) : ([]i64, i64) =
  if n < 2 then
    ([], 0)
  else
  let (ps, _) =
    loop (primesSoFar, bound) = ([2], 2) while bound < n do
      let newBound = i64.min (bound*bound) n
      let flags = sieve primesSoFar (newBound+1)
      let newPrimes = filter (\i -> flags[i]) (2...newBound)
      in (newPrimes, newBound)
  in (ps, length ps)