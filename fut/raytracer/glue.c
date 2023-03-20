#include "futray/ray.h"
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
// render boilerplate

struct render_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  struct futhark_opaque_scene *scene;
  struct futhark_opaque_prepared_scene *prepared_scene;
  struct futhark_i32_1d *img;
  int64_t height;
  int64_t width;
  int64_t start;
  int64_t len;

  int32_t *output;
};

void *render_threadfunc(void *rawArg)
{
  struct timer_t t;
  timer_begin(&t, "render_threadfunc");

  struct render_pack *pack = (struct render_pack *)rawArg;
  struct futhark_context *ctx = pack->fut_context->ctx;

  futhark_entry_rgbbox(ctx, &pack->scene);
  futhark_context_sync(ctx);

  timer_report_tick(&t, "make scene");

  futhark_entry_prepare_scene(
      ctx,
      &pack->prepared_scene,
      pack->height,
      pack->width,
      pack->scene);
  futhark_context_sync(ctx);

  timer_report_tick(&t, "prepare scene");

  futhark_entry_render_pixels(
      ctx,
      &pack->img,
      pack->height,
      pack->width,
      pack->start,
      pack->len,
      pack->prepared_scene);
  futhark_context_sync(ctx);

  timer_report_tick(&t, "render");

  futhark_values_i32_1d(ctx, pack->img, pack->output);

  timer_report_tick(&t, "move result to cpu");

  futhark_free_i32_1d(ctx, pack->img);
  futhark_free_opaque_prepared_scene(ctx, pack->prepared_scene);
  futhark_free_opaque_scene(ctx, pack->scene);

  timer_report_tick(&t, "free");

  pack->finished = true;
  return NULL;
}

struct render_pack *
render_spawn(
    struct fut_context *fut_context,
    int64_t height,
    int64_t width,
    int64_t start,
    int64_t len,
    int32_t *output)
{
  struct render_pack *pack = malloc(sizeof(struct render_pack));
  pack->fut_context = fut_context;
  pack->height = height;
  pack->width = width;
  pack->start = start;
  pack->len = len;
  pack->finished = false;
  pack->scene = NULL;
  pack->prepared_scene = NULL;
  pack->img = NULL;
  pack->output = output + start;

  if (0 != pthread_create(&(pack->friend), NULL, &render_threadfunc, pack))
  {
    printf("ERROR: glue.c: futSieveSpawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}

uint8_t render_poll(struct render_pack *pack)
{
  return pack->finished ? 1 : 0;
}

void render_finish(struct render_pack *pack)
{
  if (0 != pthread_join(pack->friend, NULL))
  {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }
  free(pack);
}