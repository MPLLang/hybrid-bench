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
// prepare scene boilerplate

struct prepare_scene_pack
{
  int64_t height;
  int64_t width;
  struct futhark_opaque_scene *scene;
  struct futhark_opaque_prepared_scene *prepared_scene;
};

void *prepare_rgbbox_scene(
    struct fut_context *fut_context,
    int64_t height,
    int64_t width)
{
  struct prepare_scene_pack *pack = malloc(sizeof(struct prepare_scene_pack));
  pack->height = height;
  pack->width = width;

  struct timer_t t;
  timer_begin(&t, "prepare_rgbbox_scene");

  struct futhark_context *ctx = fut_context->ctx;

  futhark_entry_rgbbox(ctx, &pack->scene);
  futhark_context_sync(ctx);

  timer_report_tick(&t, "make scene");

  futhark_entry_prepare_scene(
      ctx,
      &pack->prepared_scene,
      height,
      width,
      pack->scene);
  futhark_context_sync(ctx);

  timer_report_tick(&t, "prepare scene");

  return pack;
}

void prepare_rgbbox_scene_free(
    struct fut_context *fut_context,
    struct prepare_scene_pack *pack)
{
  futhark_free_opaque_prepared_scene(fut_context->ctx, pack->prepared_scene);
  futhark_free_opaque_scene(fut_context->ctx, pack->scene);
  free(pack);
}

// ==========================================================================
// prepare scene and render boilerplate

struct render_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  struct prepare_scene_pack *prepared_scene;
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

  struct futhark_i32_1d *img;

  futhark_entry_render_pixels(
      ctx,
      &img,
      pack->prepared_scene->height,
      pack->prepared_scene->width,
      pack->start,
      pack->len,
      pack->prepared_scene->prepared_scene);
  futhark_context_sync(ctx);
  futhark_values_i32_1d(ctx, img, pack->output);
  futhark_free_i32_1d(ctx, img);

  timer_report_tick(&t, "render+move+free");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST);
  return NULL;
}

struct render_pack *
render_spawn(
    struct fut_context *fut_context,
    struct prepare_scene_pack *prepared_scene,
    int64_t start,
    int64_t len,
    int32_t *output)
{
  struct render_pack *pack = malloc(sizeof(struct render_pack));
  pack->fut_context = fut_context;
  pack->prepared_scene = prepared_scene;
  pack->start = start;
  pack->len = len;
  pack->finished = false;
  pack->output = output + start;

  if (0 != pthread_create(&(pack->friend), NULL, &render_threadfunc, pack))
  {
    printf("ERROR: glue.c: render_spawn: pthread_create failed\n");
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