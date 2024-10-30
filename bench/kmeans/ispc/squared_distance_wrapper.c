#include "squared_distance.h"

double squared_distance_wrapper(
  int64_t n,
  double* x, int64_t x_offset,
  double* y, int64_t y_offset
) {
  return squared_distance(n, x+x_offset, y+y_offset);
}