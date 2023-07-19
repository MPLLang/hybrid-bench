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
// render_pixels boilerplate

struct render_pixels_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  int64_t width;
  int64_t height;
  float t;
  int64_t lo;
  int64_t hi;
  uint32_t *output;
};


void *render_pixels_threadfunc(void *rawArg)
{
  // struct timer_t t;
  // timer_begin(&t, "render_pixels_threadfunc");

  struct render_pixels_pack *pack = (struct render_pixels_pack *)rawArg;
  struct futhark_context *ctx = pack->fut_context->ctx;

  struct futhark_u32_1d *img;

  futhark_entry_render_pixels(
      ctx,
      &img,
      pack->width,
      pack->height,
      pack->t,
      pack->lo,
      pack->hi);
  futhark_context_sync(ctx);
  futhark_values_u32_1d(ctx, img, pack->output + pack->lo);
  futhark_free_u32_1d(ctx, img);

  // timer_report_tick(&t, "render+move+free");

  pack->finished = true;
  return NULL;
}


struct render_pixels_pack *
render_pixels_spawn(
    struct fut_context *fut_context,
    int64_t width,
    int64_t height,
    float t,
    int64_t lo,
    int64_t hi,
    uint32_t *output)
{
  struct render_pixels_pack *pack = malloc(sizeof(struct render_pixels_pack));
  pack->fut_context = fut_context;
  pack->width = width;
  pack->height = height;
  pack->t = t;
  pack->lo = lo;
  pack->hi = hi;
  pack->output = output;

  pack->finished = false;

  if (0 != pthread_create(&(pack->friend), NULL, &render_pixels_threadfunc, pack))
  {
    printf("ERROR: glue.c: render_pixels_spawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}


uint8_t render_pixels_poll(struct render_pixels_pack *pack)
{
  return pack->finished ? 1 : 0;
}


void render_pixels_finish(struct render_pixels_pack *pack)
{
  if (0 != pthread_join(pack->friend, NULL))
  {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }
  free(pack);
}