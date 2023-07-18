#include "futtinykaboom/tinykaboom.h"
#include "timer.h"
#include <pthread.h>

// ==========================================================================
// context boilerplate

struct fut_context
{
  struct futhark_context_config *cfg;
  struct futhark_context *ctx;
};

void *fut_init()
{
  struct timer_t t;
  timer_begin(&t, "fut_init");

  struct futhark_context_config *cfg = futhark_context_config_new();
  timer_report_tick(&t, "futhark_context_config_new");

  struct futhark_context *ctx = futhark_context_new(cfg);
  timer_report_tick(&t, "futhark_context_new");

  struct fut_context *result = malloc(sizeof(struct fut_context));
  result->cfg = cfg;
  result->ctx = ctx;
  return (void *)result;
}

void fut_cleanup(struct fut_context *fut_context)
{
  struct futhark_context_config *cfg = fut_context->cfg;
  struct futhark_context *ctx = fut_context->ctx;

  struct timer_t t;
  timer_begin(&t, "fut_cleanup");

  futhark_context_free(ctx);
  timer_report_tick(&t, "futhark_context_free");

  futhark_context_config_free(cfg);
  timer_report_tick(&t, "futhark_context_config_free");

  fut_context->ctx = NULL;
  fut_context->cfg = NULL;
  free(fut_context);
}

// ==========================================================================
// compute_frame boilerplate

struct compute_frame_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  int64_t width;
  int64_t height;
  float t;
  uint32_t *output;
};


void *compute_frame_threadfunc(void *rawArg)
{
  struct timer_t t;
  timer_begin(&t, "compute_frame_threadfunc");

  struct compute_frame_pack *pack = (struct compute_frame_pack *)rawArg;
  struct futhark_context *ctx = pack->fut_context->ctx;

  struct futhark_u32_1d *img;

  futhark_entry_compute_frame(
      ctx,
      &img,
      pack->width,
      pack->height,
      pack->t);
  futhark_context_sync(ctx);
  futhark_values_u32_1d(ctx, img, pack->output);
  futhark_free_u32_1d(ctx, img);

  timer_report_tick(&t, "render+move+free");

  pack->finished = true;
  return NULL;
}


struct compute_frame_pack *
compute_frame_spawn(
    struct fut_context *fut_context,
    int64_t width,
    int64_t height,
    float t,
    uint32_t *output)
{
  struct compute_frame_pack *pack = malloc(sizeof(struct compute_frame_pack));
  pack->fut_context = fut_context;
  pack->width = width;
  pack->height = height;
  pack->t = t;
  pack->output = output;

  pack->finished = false;

  if (0 != pthread_create(&(pack->friend), NULL, &compute_frame_threadfunc, pack))
  {
    printf("ERROR: glue.c: compute_frame_spawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}


uint8_t compute_frame_poll(struct compute_frame_pack *pack)
{
  return pack->finished ? 1 : 0;
}


void compute_frame_finish(struct compute_frame_pack *pack)
{
  if (0 != pthread_join(pack->friend, NULL))
  {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }
  free(pack);
}