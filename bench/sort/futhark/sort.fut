import "lib/github.com/diku-dk/sorts/merge_sort"
import "lib/github.com/diku-dk/sorts/radix_sort"

def radix_sort_int [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                          (xs: [n]t): [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits-1 then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- entry sort [n] (xs: [n]i32) : [n]i32 = merge_sort (\a b -> a <= b) xs

entry sort [n] (xs: [n]i32) : [n]i32 = radix_sort_int 32 i32.get_bit xs
