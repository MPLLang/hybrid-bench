struct Pt {
  double x;
  double y;
};


inline
double
tri_area(
  struct Pt a,
  struct Pt b,
  struct Pt c)
  // double a_x, double a_y,
  // double b_x, double b_y,
  // double c_x, double c_y)
{
  // b-a
  double bma_x = b.x - a.x;
  double bma_y = b.y - a.y;

  // c-a
  double cma_x = c.x - a.x;
  double cma_y = c.y - a.y;

  // cross (bma, cma)
  return bma_x*cma_y - bma_y*cma_x;
}


// inline
// uniform Pt
// get_pt_uniform(uniform double* pts_data, int32 uniform idx) {
//   uniform double x = pts_data[2*idx];
//   uniform double y = pts_data[2*idx+1];
//   uniform Pt result = { x, y };
//   return result;
// }


inline
Pt
get_pt(uniform double * uniform pts_data, int32 idx) {
  double x = pts_data[2*idx];
  double y = pts_data[2*idx+1];
  Pt result = { x, y };
  return result;
}


export
uniform uint8
max_dist_pt_8(
  uniform double * uniform pts_data,
  uniform int32 * uniform idxs_data,
  uniform int64 idxs_lo,
  // uniform int32 idxs_hi,   // this is implicit: has to be idxs_lo+8
  uniform double lx, uniform double ly,
  uniform double rx, uniform double ry)
{
  uniform Pt left = { lx, ly };
  uniform Pt right = { rx, ry };

  double dist = 0.0;
  foreach (i = 0 ... 8) {
    int32 idx = idxs_data[idxs_lo+i];
    dist = tri_area(left, right, get_pt(pts_data, idx));
  }

  uniform double max_dist = reduce_max(dist);

  uniform uint8 result = -1;
  foreach (i = 0 ... 8) {
    if (dist == max_dist) {
      result = lanemask();
    }
  }

  return result;
}

export
uniform uint64
above_below_flags_8(
  uniform double * uniform pts_data,
  uniform int64 idxs_lo,
  // uniform int32 idxs_hi,   // this is implicit: has to be idxs_lo+8
  uniform double lx, uniform double ly,
  uniform double rx, uniform double ry)
{
  uniform Pt left = { lx, ly };
  uniform Pt right = { rx, ry };

  uniform uint8 flags[8];
  foreach (i = 0 ... 8) {
    // assert(0 <= i && i < 8);
    double dist = tri_area(left, right, get_pt(pts_data, idxs_lo+i));
    if (dist > 0.0) {
      flags[i] = 0;
    }
    else if (dist < 0.0) {
      flags[i] = 1;
    }
    else {
      flags[i] = 2;
    }
    // flag = flag << (8*i);
    // uint64 mask = ~(0xFF << (8*i)) | (0xFC << (8*i));
    // assert((mask & flag) == 0);
  }

  uniform uint64 result = 0;
  for (uniform int i = 0; i < 8; i++) {
    result |= flags[i] << (8*i);
  }

  return result;
}