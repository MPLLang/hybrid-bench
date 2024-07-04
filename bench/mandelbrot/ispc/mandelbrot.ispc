// these values must exactly match those in the SML and Futhark definitions
const uniform float top = 1.0;
const uniform float bot = -1.0;
const uniform float left = -1.5;
const uniform float right = 0.5;
const uniform float diverge_thresh = 4.0;
// const uniform int32 resolution = 10000;
const uniform int32 max_iter = 50;

static inline bool mandel(const float cr, const float ci)
{
  int32 i = 0;
  float zr = 0.0;
  float zi = 0.0;
  float tr = 0.0;
  float ti = 0.0;
  // float trmti = 0.0;
  while (i < max_iter && tr + ti <= diverge_thresh) {
    zi = (2.0 * zr * zi) + ci;
    zr = (tr - ti) + cr;
    tr = zr * zr;
    ti = zi * zi;
    // trmti = tr - ti;
    i++;
  }

  return i >= max_iter;
}

static inline uniform float fceil(uniform float x) {
  return ceil(x);
}


export uniform uint8 mandelbrot_pack_byte_full(
  const uniform int32 y,
  const uniform int32 xlo,
  const uniform float dx,
  const uniform float dy)
{
  // const uniform int32 w = round(fceil(resolution * (right - left)));
  // const uniform int32 h = round(fceil(resolution * (top - bot)));
  // const uniform float dx = (right - left) / (float)w;
  // const uniform float dy = (top - bot) / (float)h;

  uint8 result = 0;
  foreach (i = 0 ... 8) {
    int32 x = xlo+i;
    float cr = left + ((float)x * dx);
    float ci = bot + ((float)y * dy);
    bool flag = mandel(cr, ci);
    uint8 bit = flag ? 1 : 0;
    result = bit << (7 - i);
  }

  uniform uint8 byte = reduce_add(result);
  return byte;
}

// REQUIRES 0 <= xhi-xlo <= 8
export uniform uint8 mandelbrot_pack_byte(
  const uniform int32 y,
  const uniform int32 xlo,
  const uniform int32 xhi,
  const uniform float dx,
  const uniform float dy)
{
  // const uniform int32 w = round(fceil(resolution * (right - left)));
  // const uniform int32 h = round(fceil(resolution * (top - bot)));
  // const uniform float dx = (right - left) / (float)w;
  // const uniform float dy = (top - bot) / (float)h;

  uint8 result = 0;
  foreach (i = 0 ... (xhi-xlo)) {
    int32 x = xlo+i;
    float cr = left + ((float)x * dx);
    float ci = bot + ((float)y * dy);
    bool flag = mandel(cr, ci);
    uint8 bit = flag ? 1 : 0;
    result = bit << ((xhi-xlo) - i - 1);
  }

  uniform uint8 byte = reduce_add(result);
  if (xhi-xlo == 8) {
    return byte;
  }
  return byte << (8 - (xhi-xlo));
}