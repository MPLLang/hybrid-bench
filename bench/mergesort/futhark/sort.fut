-- import "lib/github.com/diku-dk/sorts/merge_sort"
import "lib/github.com/diku-dk/sorts/radix_sort"

def radix_sort_int [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                          (xs: [n]t): [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits-1 then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-----------------------------------------------------------------------------


def split_count 'a (leq: a -> a -> bool) (s: []a) (t: []a) k : (i64, i64) =
  let normalize ((slo, shi), (tlo, thi), count) =
    let slo_orig = slo
    let tlo_orig = tlo

    -- maybe count is small
    let shi = i64.min shi (slo + count)
    let thi = i64.min thi (tlo + count)

    -- maybe count is large
    let slack = (shi-slo) + (thi-tlo) - count
    let slack = i64.min slack (shi-slo)
    let slack = i64.min slack (thi-tlo)

    let slo = i64.max slo (shi - slack)
    let tlo = i64.max tlo (thi - slack)

    let count = count - (slo - slo_orig) - (tlo - tlo_orig)
    in
    ((slo, shi), (tlo, thi), count)

  let step ((slo, shi), (tlo, thi), count) =
    if shi-slo <= 0 then
      ((slo, shi), (tlo+count, thi), 0)
    else if thi-tlo <= 0 then
      ((slo+count, shi), (tlo, thi), 0)
    else if count == 1 then
      if leq t[tlo] s[slo] then
        ((slo, shi), (tlo+1, thi), 0)
      else
        ((slo+1, shi), (tlo, thi), 0)
    else
    let m = count / 2
    let n = count - m

    --  |------|x|-------|
    --  ^      ^         ^
    -- slo   slo+m      shi
    --
    --  |------|y|-------|
    --  ^        ^       ^
    -- tlo     tlo+n    thi
    --

    let leq_y_x =
      n == 0 ||
      slo+m >= shi ||
      leq t[tlo+n-1] s[slo+m]
    in
    if leq_y_x then
      ((slo, shi), (tlo+n, thi), count-n)
    else
      ((slo, shi), (tlo, tlo+n), count)

  let ((slo, _), (tlo, _), _) =
    loop (ss, tt, count) = normalize ((0, length s), (0, length t), k)
    while count > 0 do
      normalize (step (ss, tt, count))
  
  in
  (slo, tlo)


------------------------------------------------------------------------------

def merge_sequential 'a (leq: a -> a -> bool) (s: []a) (t: []a) n : [n]a =
  let dummy = if length s > 0 then head s else head t
  let (_, data) =
    loop (i, data) = (0, replicate n dummy) for k < n do
      let j = k-i
      let (i, x) =
        if j == length t || (i < length s && leq s[i] t[j]) then
          (i+1, s[i])
        else
          (i, t[j])
      in
      (i, data with [k] = x)
  in
  data


def merge 'a (leq: a -> a -> bool) (s: []a) (t: []a) : []a =
  if length s == 0 then
    t
  else if length t == 0 then
    s
  else
  let n = length s + length t
  let block_size = i64.max 100 (i64.f64 (f64.ceil (f64.sqrt (f64.i64 n))) / 2)
  let num_blocks = 1 + (n-1) / block_size

  let padding = (block_size * (num_blocks-1) - n) % block_size
  let dummy = if leq (last s) (last t) then last t else last s
  let t = t ++ replicate padding dummy

  let splitters =
    tabulate (1+num_blocks) (\i -> split_count leq s t (i * block_size))
  let block b : [block_size]a =
    let (slo, tlo) = splitters[b]
    let (shi, thi) = splitters[b+1]
    in merge_sequential leq s[slo:shi] t[tlo:thi] block_size
  in
  take n (flatten (tabulate num_blocks block))

-----------------------------------------------------------------------------


-- def merge_sort 'a (leq: a -> a -> bool) (s: [n]a) : [n]a =
   


-----------------------------------------------------------------------------

entry sort [n] (xs: [n]i32) : [n]i32 = radix_sort_int 32 i32.get_bit xs

entry merge_i32 [n] [m] (xs: [n]i32) (ys: [m]i32) : []i32 =
  merge (<=) xs ys