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
uniform int64
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

  double dist;
  foreach (i = 0 ... 8) {
    int32 idx = idxs_data[idxs_lo+i];
    dist = tri_area(left, right, get_pt(pts_data, idx));
  }

  uniform double max_dist = reduce_max(dist);

  int32 result;
  foreach (i = 0 ... 8) {
    if (dist == max_dist) {
      result = i;
    }
    else {
      result = -1;
    }
  }

  return (int64)reduce_max(result);
}