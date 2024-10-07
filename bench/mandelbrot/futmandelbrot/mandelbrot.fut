
-- Config needs to match the same constants in the SML code
let top = 1.0f32
let bot = -1.0f32
let left = -1.5f32
let right = 0.5f32
let divergeThresh = 4f32
let resolution = 10000f32


let w = i64.f32 (f32.ceil (resolution * (right - left)))
let h = i64.f32 (f32.ceil (resolution * (top - bot)))
let dx = (right - left) / f32.i64 w
let dy = (top - bot) / f32.i64 h


let xyToComplex (x, y) =
  let r = left + (f32.i64 x * dx)
  let i = bot + (f32.i64 y * dy)
  in
  (r, i)


def test maxIter x y =
  let (cr, ci) = xyToComplex (x, y)
  in
  loop (zr, zi, trmti, i) = (0.0f32, 0.0f32, 0.0f32, 0i32)
  while i < maxIter && (zr * zr) + (zi * zi) <= divergeThresh do
    let zi = (2.0 * zr * zi) + ci
    let zr = trmti + cr
    let tr = zr * zr
    let ti = zi * zi
    in
    (zr, zi, tr - ti, i + 1)


def mark maxIter x y =
  let (zr, zi, _, _) = test maxIter x y
  in
  if (zr * zr) + (zi * zi) > divergeThresh then 0u8 else 1u8



def addPattern (y: i64) (byte: u8) =
  if y % 2 == 0 then
    0xAA | byte
  else
    0x55 | byte


def packByte maxIter y (xlo, xhi) =
  let (byte, _) =
    loop (byte, x) = (0u8, xlo)
    while x < xhi do
      let byte = (byte << 1) | mark maxIter x y
      in (byte, x+1)
  let byte =
    if xhi-xlo == 8 then byte
    else byte << (8 - (u8.i64 (xhi - xlo)))
  in
  byte


entry mandelbrot maxIter ylo yhi blo bhi =
  let numRows = yhi - ylo
  let numBytes = bhi - blo
  in
  flatten (tabulate_2d numRows numBytes (\y b ->
    let xlo = blo + b * 8
    let xhi = i64.min (xlo + 8) w
    let byte = packByte maxIter (ylo + y) (xlo, xhi)
    let byte = addPattern y byte
    in
    byte))
