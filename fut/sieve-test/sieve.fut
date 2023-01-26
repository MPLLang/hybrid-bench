
def segmented_scan 't [n] (g:t->t->t) (ne: t) (flags: [n]bool) (vals: [n]t): [n]t =
  let pairs =
    scan ( \ (v1,f1) (v2,f2) ->
             let f = f1 || f2
             let v = if f2 then v2 else g v1 v2
             in (v,f) )
      (ne, false)
      (zip vals flags)

  let (res,_) = unzip pairs
  in res


def replicated_iota [n] (reps:[n]i64) : []i64 =
  let s1 = scan (+) 0 reps
  let s2 = map (\i -> if i==0 then 0 else s1[i-1]) (iota n)
  let tmp = scatter (replicate (reduce (+) 0 reps) 0) s2 (iota n)
  let flags = map (>0) tmp
  in segmented_scan (+) 0 flags tmp


def main (primes: []i64) (numFlags: i64) : []bool =
  let numMultiples p = (numFlags-1) / p - 1
  let offsets = scan (+) 0 (map numMultiples primes)
  let offset pi = if pi == 0 then 0 else offsets[pi-1]

  let spreadPrimeIndices =
    map (\pi -> primes[pi]) (replicated_iota (map numMultiples primes))

  let updatePosition i pi =
    let p = primes[pi]
    let j = i - offset pi
    in (j+2)*p

  let indices =
    map2 updatePosition (indices spreadPrimeIndices) spreadPrimeIndices

  in spread numFlags true indices (map (\_ -> false) indices)