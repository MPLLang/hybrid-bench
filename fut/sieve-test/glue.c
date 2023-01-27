#include "sieve.h"
#include "timer.h"

struct futStuff {
  struct futhark_context_config *cfg;
  struct futhark_context *ctx;
};

void* futInit() {
  struct timer_t t;
  timer_begin(&t, "futInit");

  struct futhark_context_config *cfg = futhark_context_config_new();
  timer_report_tick(&t, "futhark_context_config_new");

  struct futhark_context *ctx = futhark_context_new(cfg);
  timer_report_tick(&t, "futhark_context_new");

  struct futStuff *result = malloc(sizeof(struct futStuff));
  result->cfg = cfg;
  result->ctx = ctx;
  return (void *)result;
}

void futFinish(struct futStuff * futStuff) {
  struct futhark_context_config *cfg = futStuff->cfg;
  struct futhark_context *ctx = futStuff->ctx;

  struct timer_t t;
  timer_begin(&t, "futFinish");

  futhark_context_free(ctx);
  timer_report_tick(&t, "futhark_context_free");

  futhark_context_config_free(cfg);
  timer_report_tick(&t, "futhark_context_config_free");

  futStuff->ctx = NULL;
  futStuff->cfg = NULL;
  free(futStuff);
}

void futSieve(
  struct futStuff * futStuff,
  int64_t inputLen,
  int64_t* input,
  int64_t outputLen,
  bool* output)
{
  struct futhark_context_config *cfg = futStuff->cfg;
  struct futhark_context *ctx = futStuff->ctx;

  struct timer_t t;
  timer_begin(&t, "futSieve");

  struct futhark_i64_1d *input_arr = futhark_new_i64_1d(ctx, input, inputLen);
  timer_report_tick(&t, "futhark_new_i64_1d");

  struct futhark_bool_1d *output_arr;

  futhark_entry_main(ctx, &output_arr, input_arr, outputLen);
  timer_report_tick(&t, "futhark_entry_main");

  futhark_context_sync(ctx);
  timer_report_tick(&t, "futhark_context_sync");

  futhark_values_bool_1d(ctx, output_arr, output);
  timer_report_tick(&t, "futhark_values_bool_1d");

  futhark_free_i64_1d(ctx, input_arr);
  timer_report_tick(&t, "futhark_free_i64_1d");

  futhark_free_bool_1d(ctx, output_arr);
  timer_report_tick(&t, "futhark_free_bool_1d");
}