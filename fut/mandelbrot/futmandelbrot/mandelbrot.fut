
-- Config needs to match the same constants in the SML code
let top = 1.0f64
let bot = -1.0f64
let left = -1.5f64
let right = 0.5f64
let maxIter = 50i32
let divergeThresh = 4f64
let resolution = 5000f64


let w = i64.f64 (f64.ceil (resolution * (right - left)))
let h = i64.f64 (f64.ceil (resolution * (top - bot)))
let dx = (right - left) / f64.i64 w
let dy = (top - bot) / f64.i64 h


let xyToComplex (x, y) =
  let r = left + (f64.i64 x * dx)
  let i = bot + (f64.i64 y * dy)
  in
  (r, i)


def test x y =
  let (cr, ci) = xyToComplex (x, y)
  in
  loop (zr, zi, trmti, i) = (0.0f64, 0.0f64, 0.0f64, 0)
  while i < maxIter && (zr * zr) + (zi * zi) <= divergeThresh do
    let zi = (2.0 * zr * zi) + ci
    let zr = trmti + cr
    let tr = zr * zr
    let ti = zi * zi
    in
    (zr, zi, tr - ti, i + 1)


def mark x y =
  let (zr, zi, _, _) = test x y
  in
  if (zr * zr) + (zi * zi) > divergeThresh then 0u8 else 1u8


def packByte y (xlo, xhi) =
  let (byte, _) =
    loop (byte, x) = (0u8, xlo)
    while x < xhi do
      let byte = (byte << 1) | mark x y
      in (byte, x+1)
  let byte =
    if xhi-xlo == 8 then byte
    else byte << (8 - (u8.i64 (xhi - xlo)))
  in
  byte


entry mandelbrot ylo yhi blo bhi =
  let numRows = yhi - ylo
  let numBytes = bhi - blo
  in
  flatten (tabulate_2d numRows numBytes (\y b ->
    let xlo = blo + b * 8
    let xhi = i64.min (xlo + 8) w
    in packByte (ylo + y) (xlo, xhi)))