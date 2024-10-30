signature FUTHARK_ENTRIES =
sig
  (* either Real32Array2.array or Real64Array2.array *)
  type futhark_points

  type i32 = Int32.int
  type i64 = Int64.int
  type futhark_i32_arr = Futhark.Int32Array1.array
  type ctx = Futhark.ctx

  val filter_then_semihull: ctx
                            -> futhark_points * i32 * i32 * futhark_i32_arr
                            -> futhark_i32_arr
  val min_max_point_in_range: ctx
                              -> futhark_points * i64 * i64
                              -> i32 * i32 * i32 * i32
  val point_furthest_from_line: ctx
                                -> futhark_points * i32 * i32 * futhark_i32_arr
                                -> i32
  val points_above: ctx
                    -> futhark_points * futhark_i32_arr * i32 * i32
                    -> futhark_i32_arr
  val quickhull: ctx -> futhark_points -> futhark_i32_arr
  val semihull: ctx
                -> futhark_points * i32 * i32 * futhark_i32_arr
                -> futhark_i32_arr
  val top_level_filter_then_semihull: ctx
                                      -> futhark_points * i32 * i32
                                      -> futhark_i32_arr
  val top_level_points_above_in_range: ctx
                                       -> futhark_points * i64 * i64 * i32 * i32
                                       -> futhark_i32_arr
end
