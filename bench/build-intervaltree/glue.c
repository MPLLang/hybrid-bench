#include "futsort/sort.h"
#include "timer.h"
#include <pthread.h>

// ==========================================================================
// context boilerplate

struct fut_context
{
  struct futhark_context_config *cfg;
  struct futhark_context *ctx;

  struct futhark_i32_1d *vals;
};

void *fut_init(unsigned char *device, int32_t *vals, int64_t start, int64_t len)
{
  struct timer_t t;
  timer_begin(&t, "fut_init");

  struct futhark_context_config *cfg = futhark_context_config_new();
  futhark_context_config_set_device(cfg, device);
  timer_report_tick(&t, "futhark_context_config_new");

  struct futhark_context *ctx = futhark_context_new(cfg);
  timer_report_tick(&t, "futhark_context_new");

  struct fut_context *result = malloc(sizeof(struct fut_context));
  result->cfg = cfg;
  result->ctx = ctx;
  result->vals = futhark_new_i32_1d(ctx, vals + start, len);
  timer_report_tick(&t, "futhark_new_i32_1d");

  return (void *)result;
}

void fut_cleanup(struct fut_context *fut_context)
{
  struct futhark_context_config *cfg = fut_context->cfg;
  struct futhark_context *ctx = fut_context->ctx;

  struct timer_t t;
  timer_begin(&t, "fut_cleanup");

  futhark_free_i32_1d(ctx, fut_context->vals);
  timer_report_tick(&t, "futhark_free_i32_1d");

  futhark_context_free(ctx);
  timer_report_tick(&t, "futhark_context_free");

  futhark_context_config_free(cfg);
  timer_report_tick(&t, "futhark_context_config_free");

  fut_context->ctx = NULL;
  fut_context->cfg = NULL;
  fut_context->vals = NULL;
  free(fut_context);
}

// ==========================================================================
// sort boilerplate

struct sort_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  int32_t *input;
  int64_t start;
  int64_t len;

  int32_t *output;
};

void *sort_threadfunc(void *rawArg)
{
  struct timer_t t;
  timer_begin(&t, "sort_threadfunc");

  struct sort_pack *pack = (struct sort_pack *)rawArg;
  struct futhark_context *ctx = pack->fut_context->ctx;

  struct futhark_i32_1d *input;
  struct futhark_i32_1d *output;

  input = futhark_new_i32_1d(ctx, pack->input + pack->start, pack->len);
  // timer_report_tick(&t, "copy input      ");

  futhark_entry_sort(ctx, &output, pack->fut_context->vals, input);
  futhark_context_sync(ctx);
  // timer_report_tick(&t, "sort            ");

  futhark_values_i32_1d(ctx, output, pack->output);
  futhark_free_i32_1d(ctx, input);
  futhark_free_i32_1d(ctx, output);
  timer_report_tick(&t, "sort");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST);
  return NULL;
}

struct sort_pack *
sort_spawn(
    struct fut_context *fut_context,
    int32_t *input,
    int64_t start,
    int64_t len,
    int32_t *output)
{
  struct sort_pack *pack = malloc(sizeof(struct sort_pack));
  pack->fut_context = fut_context;
  pack->input = input;
  pack->start = start;
  pack->len = len;
  pack->output = output;
  pack->finished = false;

  sort_threadfunc(pack);

  // if (0 != pthread_create(&(pack->friend), NULL, &sort_threadfunc, pack))
  // {
  //   printf("ERROR: glue.c: sort_spawn: pthread_create failed\n");
  //   exit(1);
  // }

  return pack;
}

// uint8_t sort_poll(struct sort_pack *pack)
// {
//   return pack->finished ? 1 : 0;
// }

void sort_finish(struct sort_pack *pack)
{
  // if (0 != pthread_join(pack->friend, NULL))
  // {
  //   printf("ERROR: glue.c: pthread_join failed\n");
  //   exit(1);
  // }
  free(pack);
}