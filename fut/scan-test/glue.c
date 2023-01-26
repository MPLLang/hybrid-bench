#include "scan.h"

void futScan(int32_t len, int32_t* input, int32_t* output) {
  printf("futhark_context_config_new\n");
  struct futhark_context_config *cfg = futhark_context_config_new();

  printf("futhark_context_new\n");
  struct futhark_context *ctx = futhark_context_new(cfg);

  printf("futhark_new_i32_1d(input_arr)\n");
  struct futhark_i32_1d *input_arr = futhark_new_i32_1d(ctx, input, len);

  printf("futhark_new_i32_1d(output_arr)\n");
  struct futhark_i32_1d *output_arr = futhark_new_i32_1d(ctx, output, len);

  printf("futhark_entry_main\n");
  futhark_entry_main(ctx, &output_arr, input_arr);

  printf("futhark_context_sync\n");
  futhark_context_sync(ctx);

  printf("futhark_values_i32_1d\n");
  futhark_values_i32_1d(ctx, output_arr, output);

  printf("futhark_... freeing\n");
  futhark_free_i32_1d(ctx, input_arr);
  futhark_free_i32_1d(ctx, output_arr);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);

  printf("finishing futScan\n");
}