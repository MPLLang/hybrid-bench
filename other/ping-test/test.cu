#include <stdlib.h>
#include <stdio.h>
#include <cuda.h>
#include <cassert>
#include <sys/time.h>

__global__ void gpu_ping(int* foo) {
  int smid;
  asm volatile("mov.u32 %0, %%smid;" : "=r"(smid));
  *foo = smid;
  return;
}

void timespec_sub(struct timespec *x, struct timespec *y) {
  assert(x->tv_sec >= y->tv_sec);

  if (x->tv_nsec < y->tv_nsec) {
    assert(x->tv_sec >= y->tv_sec + 1);
    x->tv_sec -= 1;
    x->tv_nsec += 1000000000L;
  }

  assert(x->tv_sec >= y->tv_sec);
  assert(x->tv_nsec >= y->tv_nsec);

  x->tv_sec -= y->tv_sec;
  x->tv_nsec -= y->tv_nsec;
}

void timespec_add(struct timespec *x, struct timespec *y) {
  x->tv_sec += y->tv_sec;
  x->tv_nsec += y->tv_nsec;

  if (x->tv_nsec >= 1000000000L) {
    x->tv_sec += 1;
    x->tv_nsec -= 1000000000L;
  }
}

bool timespec_geq(struct timespec *t1, struct timespec *t2) {
  if (t1->tv_sec > t2->tv_sec)
    return true;
  if (t1->tv_sec < t2->tv_sec)
    return false;
  return t1->tv_nsec >= t2->tv_nsec;
}

const int MAX_SMIDS = 100000;

void compute_favorites(int* smid_counts, int* favorites, int num_favorites) {
  int* favorite_counts = (int*)malloc(num_favorites*sizeof(int));
  for (int i = 0; i < num_favorites; i++) {
    favorites[i] = -1;
    favorite_counts[i] = 0;
  }

  for (int i = 0; i < MAX_SMIDS; i++) {
    int smid = i;
    int count = smid_counts[i];

    for (int j = 0; j < num_favorites; j++) {
      if (count > favorite_counts[j]) {
        int next_smid = favorites[j];
        int next_count = favorite_counts[j];
        favorites[j] = smid;
        favorite_counts[j] = count;
        smid = next_smid;
        count = next_count;
      }
    }
  }

  free(favorite_counts);
}

int main() {
  struct timespec interval;
  interval.tv_sec = 0;
  interval.tv_nsec = 200000000L; // 1/5th of a second

  int smid_counts[MAX_SMIDS];
  for (int i = 0; i < MAX_SMIDS; i++) {
    smid_counts[i] = 0;
  }

  int* smid_d;
  cudaMalloc(&smid_d, sizeof(int));

  struct timespec accum;
  accum.tv_sec = 0;
  accum.tv_nsec = 0;

  int succeeded = 0;

  struct timespec last_report;
  clock_gettime(CLOCK_MONOTONIC_RAW, &last_report);

  while (true) {
    struct timespec start;
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);

    gpu_ping<<<1,1>>>(smid_d);
    int smid;
    cudaMemcpy(&smid, smid_d, sizeof(int), cudaMemcpyDeviceToHost);
    cudaDeviceSynchronize();

    struct timespec stop;
    clock_gettime(CLOCK_MONOTONIC_RAW, &stop);

    struct timespec report_diff;
    report_diff = stop;
    timespec_sub(&report_diff, &last_report);
    bool report = timespec_geq(&report_diff, &interval);

    succeeded++;
    smid_counts[smid]++;

    timespec_sub(&stop, &start);
    timespec_add(&accum, &stop);

    if (report) {
      int fav[3];
      compute_favorites(smid_counts, fav, 3);

      double seconds = (double)accum.tv_sec + ((double)accum.tv_nsec / 1e9);
      double avg_seconds = seconds / (double)succeeded;

      printf("ping favorites=[%d:%d,%d:%d,%d:%d] avg_ping=%.9lf\n",
        fav[0], (fav[0] >= 0 ? smid_counts[fav[0]] : 0),
        fav[1], (fav[1] >= 0 ? smid_counts[fav[1]] : 0),
        fav[2], (fav[2] >= 0 ? smid_counts[fav[2]] : 0),
        avg_seconds
      );

      accum.tv_sec = 0;
      accum.tv_nsec = 0;
      succeeded = 0;
      for (int i = 0; i < MAX_SMIDS; i++) smid_counts[i] = 0;
      clock_gettime(CLOCK_MONOTONIC_RAW, &last_report);
    }
  }
}