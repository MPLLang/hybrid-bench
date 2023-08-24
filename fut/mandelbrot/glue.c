#include "futmandelbrot/mandelbrot.h"
#include "timer.h"
#include <pthread.h>
#include <sched.h>

void set_cpu_affinity(int cpu) {
  cpu_set_t cpuset;
  pthread_t thread;
  thread = pthread_self();
  CPU_ZERO(&cpuset);
  CPU_SET(cpu, &cpuset);
  pthread_setaffinity_np(thread, sizeof cpuset, &cpuset);
}

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
// mandelbrot boilerplate

struct mandelbrot_pack
{
  struct fut_context *fut_context;
  bool finished;
  pthread_t friend;

  int64_t ylo;
  int64_t yhi;
  int64_t blo;
  int64_t bhi;
  uint8_t *output;
};

void *mandelbrot_threadfunc(void *rawArg)
{
  set_cpu_affinity(31);
  struct timer_t t;
  timer_begin(&t, "mandelbrot_threadfunc");

  struct mandelbrot_pack *pack = (struct mandelbrot_pack *)rawArg;
  struct futhark_context *ctx = pack->fut_context->ctx;

  struct futhark_u8_1d *output;

  futhark_entry_mandelbrot(ctx, &output, pack->ylo, pack->yhi, pack->blo, pack->bhi);
  futhark_context_sync(ctx);

  futhark_values_u8_1d(ctx, output, pack->output);
  futhark_free_u8_1d(ctx, output);
  timer_report_tick(&t, "mandelbrot");

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST);
  return NULL;
}

struct mandelbrot_pack *
mandelbrot_spawn(
    struct fut_context *fut_context,
    int64_t ylo,
    int64_t yhi,
    int64_t blo,
    int64_t bhi,
    uint8_t *output)
{
  struct mandelbrot_pack *pack = malloc(sizeof(struct mandelbrot_pack));
  pack->fut_context = fut_context;
  pack->ylo = ylo;
  pack->yhi = yhi;
  pack->blo = blo;
  pack->bhi = bhi;
  pack->output = output;
  pack->finished = false;

  if (0 != pthread_create(&(pack->friend), NULL, &mandelbrot_threadfunc, pack))
  {
    printf("ERROR: glue.c: mandelbrot_spawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}

uint8_t mandelbrot_poll(struct mandelbrot_pack *pack)
{
  return pack->finished ? 1 : 0;
}

void mandelbrot_finish(struct mandelbrot_pack *pack)
{
  if (0 != pthread_join(pack->friend, NULL))
  {
    printf("ERROR: glue.c: pthread_join failed\n");
    exit(1);
  }
  free(pack);
}