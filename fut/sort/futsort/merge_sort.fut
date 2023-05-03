import "lib/github.com/diku-dk/sorts/merge_sort"

let ms = merge_sort

entry merge_sort [n] (xs: [n]i64) : [n]i64 = ms (\a b -> a <= b) xs
