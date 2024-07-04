// these values must exactly match those in the SML and Futhark definitions
const uniform double top = 1.0;
const uniform double bot = -1.0;
const uniform double left = -1.5;
const uniform double right = 0.5;
const uniform double diverge_thresh = 4.0;
const uniform int64 resolution = 10000;
const uniform int64 max_iter = 50;


static inline bool mandel(const double cr, const double ci)
{
  int64 i = 0;
  double zr = 0.0;
  double zi = 0.0;
  double trmti = 0.0;
  while (i < max_iter && (zr * zr) + (zi * zi) <= diverge_thresh) {
    zi = (2.0 * zr * zi) + ci;
    zr = trmti + cr;
    double tr = zr * zr;
    double ti = zi * zi;
    trmti = tr - ti;
    i++;
  }

  return i >= max_iter;
}

// REQUIRES 0 <= xhi-xlo <= 8
export uniform uint8 mandelbrot_pack_byte(
  const uniform int64 y,
  const uniform int64 xlo,
  const uniform int64 xhi)
{
  const uniform int64 w = round(ceil(resolution * (right - left)));
  const uniform int64 h = round(ceil(resolution * (top - bot)));
  const uniform double dx = (right - left) / (double)w;
  const uniform double dy = (top - bot) / (double)h;

  uint8 result = 0;
  foreach (i = 0 ... (xhi-xlo)) {
    int64 x = xlo+i;
    double cr = left + ((double)x * dx);
    double ci = bot + ((double)y * dy);
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