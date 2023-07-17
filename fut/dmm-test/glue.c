#include "bigadd.h"
#include "timer.h"
#include <pthread.h>

// ==========================================================================
// context boilerplate


/* TODO: this stuff can probably go away entirely */

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

// ==========================================================================
// bigAdd boilerplate


/* TODO: inputs and outputs for leaf DMM, dimension info, etc. */
struct bigAddPackage {
  struct futStuff *futStuff;  /* won't need this */

  /* need to be specialized for DMM */
  struct futhark_u8_1d * a;
  struct futhark_u8_1d * b;
  struct futhark_u8_1d * output;
  int64_t outputLen;

  /* these should stay */
  bool finished;
  pthread_t friend;
};

/* TODO: call cublas */
void* asyncBigAddFunc(void* rawArg) {
  struct timer_t t;
  timer_begin(&t, "ayscBigAddFunc");

  struct bigAddPackage *pack = (struct bigAddPackage *)rawArg;

  futhark_entry_add(pack->futStuff->ctx,
    &(pack->output),
    &(pack->outputLen), 
    pack->a, 
    pack->b);

  futhark_context_sync(pack->futStuff->ctx);
  timer_report_tick(&t, "done");
  pack->finished = true; /* VERY IMPORTANT! */
  return NULL;
}


/* TODO: build the package, but otherwise shouldn't need to change much. 
 *
 * (NOTE: futhark_new_... is essentially a memcpy, these need to be replaced
 *  with stuff for cublas)
 */
struct bigAddPackage * 
futBigAddSpawn(
  struct futStuff * futStuff,
  uint8_t * a,
  uint8_t * b,
  int64_t inputLen)
{
  struct futhark_context *ctx = futStuff->ctx;
  struct bigAddPackage *pack = malloc(sizeof(struct bigAddPackage));
  pack->futStuff = futStuff;
  pack->a = futhark_new_u8_1d(ctx, a, inputLen);
  pack->b = futhark_new_u8_1d(ctx, b, inputLen);
  pack->outputLen = 0;
  pack->output = NULL;
  pack->finished = false;

  if (0 != pthread_create(&(pack->friend), NULL, &asyncBigAddFunc, pack)) {
    printf("ERROR: glue.c: futBigAddSpawn: pthread_create failed\n");
    exit(1);
  }

  return pack;
}

/* TODO: probably doesn't need to change */
uint8_t futBigAddPoll(struct bigAddPackage *pack) {
  return pack->finished ? 1 : 0;
}

int64_t futBigAddOutputSize(struct bigAddPackage *pack) {
  // struct timer_t t;
  // timer_begin(&t, "futPrimesOutputSize");

  if (0 != pthread_join(pack->friend, NULL)) {
    printf("ERROR: glue.c: futBigAddOutputSize: pthread_join failed\n");
    exit(1);
  }

  // timer_report_tick(&t, "done");
  return pack->outputLen;
}

/* TODO: memcpy from GPU back to pack->output
 *
 * (NOTE: futhark_values is equivalent of this memcpy. needs to be replaced) */
void futBigAddFinish(
  struct bigAddPackage * pack,
  uint8_t * output)
{
  // if (0 != pthread_join(pack->friend, NULL)) {
  //   printf("ERROR: glue.c: pthread_join failed\n");
  //   exit(1);
  // }

  futhark_values_u8_1d(pack->futStuff->ctx, pack->output, output);
  futhark_free_u8_1d(pack->futStuff->ctx, pack->a);
  futhark_free_u8_1d(pack->futStuff->ctx, pack->b);
  futhark_free_u8_1d(pack->futStuff->ctx, pack->output);
  free(pack);

}
