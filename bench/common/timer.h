#pragma once

#include <sys/time.h>

struct timer_t {
  const char *name;
  struct timespec start;
  struct timespec most_recent_tick;
};

void timer_begin(struct timer_t *t, const char *name);
void timer_report_tick(struct timer_t *t, const char *msg);
