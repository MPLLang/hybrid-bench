def nth_ (x: [n]i8) (i: i64) : i8 =
let (ps)
    length [n] 'a (xs: [n]a) = n
    if i < length x then
        x[i]
    else
        (0w0: i8)
in
    ps

def propagate (x: i8) (y: i8) =
let(carries)
    if y = 0w127 then
        x
    else   
        y
in
    carries

def f (carry: i8) (sum: i8) =
    let res = ((carry >> 0w7) + sum) & 0w7F
    in 
        res

def add (x: [n]i8) (y: [m]i8) =
    let length [n] 'a (xs: [n]a) = n
    let maxlen = if n > m then n else m
    let sums = tabulate maxlen+1 (\i -> nth_ x i + nth_ y i)
    let carries = scan propagate 0w0 sums

    --not sure how to do force and pass a function to zip
    let res = zip f (carries, sums)
    -- res should be an array here
    in
        if null res or last res > 0w0 then
            res
        else
            init res 