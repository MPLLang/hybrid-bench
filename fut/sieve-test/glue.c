#include "sieve.h"

void futSieve(int64_t inputLen, int64_t* input, int64_t outputLen, bool* output) {
  printf("futhark_context_config_new\n");
  struct futhark_context_config *cfg = futhark_context_config_new();

  printf("futhark_context_new\n");
  struct futhark_context *ctx = futhark_context_new(cfg);

  printf("futhark_new_i64_1d(input_arr)\n");
  struct futhark_i64_1d *input_arr = futhark_new_i64_1d(ctx, input, inputLen);

  printf("futhark_new_bool_1d(output_arr)\n");
  struct futhark_bool_1d *output_arr = futhark_new_bool_1d(ctx, output, outputLen);

  printf("futhark_entry_main\n");
  futhark_entry_main(ctx, &output_arr, input_arr, outputLen);

  printf("futhark_context_sync\n");
  futhark_context_sync(ctx);

  printf("futhark_values_bool_1d\n");
  futhark_values_bool_1d(ctx, output_arr, output);

  printf("futhark_... freeing\n");
  futhark_free_i64_1d(ctx, input_arr);
  futhark_free_bool_1d(ctx, output_arr);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);

  printf("finishing futSieve\n");
}