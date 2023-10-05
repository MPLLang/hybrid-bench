def nth_ (x: []u8) (i: i64) : u8 =
	if i < length x then
		x[i]
	else
		0u8

def propagate (x: u8) (y: u8) =
    if y == 127u8 then
        x
    else   
        y

def f (carry: u8) (sum: u8) =
    let res = ((carry >> 7u8) + sum) & 0x7Fu8
    in 
        res

def exscan [n] 'a f zero (x: [n]a) : [n]a =
	let o = scan f zero x
	in 
		tabulate (n) (\i -> if i == 0 then zero else o[i-1]) 


entry add (x: []u8) (y: []u8) : ([]u8, i64) =
    let maxlen = i64.max (length x) (length y)
    let sums = tabulate (maxlen+1) (\i -> nth_ x i + nth_ y i)
    let carries = exscan propagate 0u8 sums

    let res = map2 f carries sums
    in
        if (null res) || (last res) > 0u8 then
            (res, length res)
        else
            (init res, length res - 1) 