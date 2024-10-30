uniform double dot_product(uniform int64 n, const uniform double x[], const uniform double y[]) {
  double sum = 0.0;
  foreach (i = 0 ... n) {
    sum += x[i] * y[i];
  }
  return reduce_add(sum);
}

export
uniform double squared_distance(uniform int64 n, const uniform double x[], const uniform double y[]) {
  return dot_product(n, x, x) + dot_product(n, y, y) - 2.0 * dot_product(n, x, y);
}