#include <time.h>
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include <cusparse.h> // cusparseSpMV
#include <pthread.h>
#include <error.h>

#define SIZE 256

#define ENABLE_TIMER_TICKS false

#define CHECK_CUDA(func)                                                       \
{                                                                              \
    cudaError_t status = (func);                                               \
    if (status != cudaSuccess) {                                               \
        printf("CUDA API failed at line %d with error: %s (%d)\n",             \
               __LINE__, cudaGetErrorString(status), status);                  \
        return EXIT_FAILURE;                                                   \
    }                                                                          \
}

#define CHECK_CUSPARSE(func)                                                   \
{                                                                              \
    cusparseStatus_t status = (func);                                          \
    if (status != CUSPARSE_STATUS_SUCCESS) {                                   \
        printf("CUSPARSE API failed at line %d with error: %s (%d)\n",         \
               __LINE__, cusparseGetErrorString(status), status);              \
        return EXIT_FAILURE;                                                   \
    }                                                                          \
}

// ==========================================================================
// timer stuff

struct my_timer_t {
  const char *name;
  struct timespec start;
  struct timespec most_recent_tick;
};

static void timespec_subtract(struct timespec *x, struct timespec *y) {
  if (x->tv_nsec < y->tv_nsec) {
    x->tv_sec -= 1;
    x->tv_nsec += 1000000000L;
  }
  x->tv_sec -= y->tv_sec;
  x->tv_nsec -= y->tv_nsec;
}

static void report_elapsed(
  const char *name, 
  const char *msg, 
  struct timespec *x, 
  struct timespec *y)
{
  struct timespec diff = *x;
  timespec_subtract(&diff, y);
  double secs = (double)diff.tv_sec + ((double)diff.tv_nsec / 1000000000.0);
  printf("tick: %s: %s: elapsed: %lf\n", name, msg, secs);
  fflush(stdout);
}

void timer_begin(struct my_timer_t *t, const char *name) {
  t->name = name;
  clock_gettime(CLOCK_MONOTONIC, &(t->start));
  t->most_recent_tick = t->start;
}

void timer_report_tick(struct my_timer_t *t, const char *msg) {
  struct timespec prev = t->most_recent_tick;
  clock_gettime(CLOCK_MONOTONIC, &(t->most_recent_tick));
  if (ENABLE_TIMER_TICKS) {
    report_elapsed(t->name, msg, &(t->most_recent_tick), &prev);
  }
}


// ==========================================================================
// spMV boilerplate



struct spMVPackage {
  // struct futStuff *futStuff;  /* won't need this */

  /* need to be specialized for spMV */
  int64_t nnz; // number of non zeros in submatrix, this is the length of each array coming from MPL
  int64_t num_rows; //number of rows in submatrix, how many rows does this submatrix represent
  int64_t num_cols; //number of columns in submatrix, treat as dense 
  int64_t * row_indices; //these need to start from 0 coming from MPL, shift all values by submatrix start row distance from first row
  int64_t * col_indices; // unmodified so long as columns are not partitioned and parallelized otherwise start at 0 as well
  float * values;
  float * vector_values; // should be same size as num_cols
  float * output; //should be the same size as num_rows
  /* these should stay */
  bool finished;
  pthread_t friends;
};


void set_cpu_affinity(int cpu) {
  cpu_set_t cpuset;
  pthread_t thread;

  thread = pthread_self();

  CPU_ZERO(&cpuset);
  CPU_SET(cpu, &cpuset);

  if (pthread_setaffinity_np(thread, sizeof cpuset, &cpuset) != 0) {
    printf("ERROR: glue.c: could not set affinity\n");
    exit(1);
  }
}


void* asyncspMVFunc(void* rawArg) {
  struct my_timer_t t;
  timer_begin(&t, "asyncspMVFunc");

  // set_cpu_affinity(31);


  struct spMVPackage *pack = (struct spMVPackage *)rawArg;
  
  //cuda memory management
  float alpha        = 1.0f;
  float beta         = 0.0f; //cusparse spmv performs y = Ax + By (y acts as both input and output) so we make B=0 
  int64_t   *drows, *dcolumns; // matrix row/col indices
  float *dvalues, *dX, *dY; // matrix values, input vector, output vector

  

  CHECK_CUDA( cudaMalloc((void**) &drows,    pack->nnz * sizeof(int64_t))    )
  CHECK_CUDA( cudaMalloc((void**) &dcolumns, pack->nnz * sizeof(int64_t))    )
  CHECK_CUDA( cudaMalloc((void**) &dvalues,  pack->nnz * sizeof(float))      )
  CHECK_CUDA( cudaMalloc((void**) &dX,       pack->num_cols * sizeof(float)) )
  CHECK_CUDA( cudaMalloc((void**) &dY,       pack->num_rows * sizeof(float)) )       
  CHECK_CUDA( cudaMemcpy(drows, pack->row_indices, pack->nnz * sizeof(int64_t),
                        cudaMemcpyHostToDevice) )
  CHECK_CUDA( cudaMemcpy(dcolumns, pack->col_indices, pack->nnz * sizeof(int64_t),
                        cudaMemcpyHostToDevice) )
  CHECK_CUDA( cudaMemcpy(dvalues, pack->values, pack->nnz * sizeof(float),
                        cudaMemcpyHostToDevice) )
  CHECK_CUDA( cudaMemcpy(dX, pack->vector_values, pack->num_cols * sizeof(float),
                        cudaMemcpyHostToDevice) )
  //this is required because we could get an unmodified index and we dont want a garbage value
  CHECK_CUDA( cudaMemset(dY, 0, pack->num_rows * sizeof(float)) )
  
  //cusparse memory management
  cusparseHandle_t     handle = NULL;
  cusparseSpMatDescr_t matA;
  cusparseDnVecDescr_t vecX, vecY;
  void*                dBuffer    = NULL;
  size_t               bufferSize = 0;


  CHECK_CUSPARSE( cusparseCreate(&handle) )
    // Create sparse matrix A in COO format
  CHECK_CUSPARSE( cusparseCreateCoo(&matA, pack->num_rows,
                                        pack->num_cols, pack->nnz,
                                      drows, dcolumns, dvalues,
                                      CUSPARSE_INDEX_64I,
                                      CUSPARSE_INDEX_BASE_ZERO, CUDA_R_32F) )
  // Create dense vector X
  CHECK_CUSPARSE( cusparseCreateDnVec(&vecX, pack->num_cols, dX, CUDA_R_32F) )
  // Create dense vector y
  CHECK_CUSPARSE( cusparseCreateDnVec(&vecY, pack->num_rows, dY, CUDA_R_32F) )
  // allocate an external buffer (might not be needed, is used to get cusparse call configuration info)
  CHECK_CUSPARSE( cusparseSpMV_bufferSize(
                                 handle, CUSPARSE_OPERATION_NON_TRANSPOSE,
                                 &alpha, matA, vecX, &beta, vecY, CUDA_R_32F,
                                 CUSPARSE_SPMV_ALG_DEFAULT, &bufferSize) )
  CHECK_CUDA( cudaMalloc(&dBuffer, bufferSize) )

  // execute SpMV
  CHECK_CUSPARSE( cusparseSpMV(handle, CUSPARSE_OPERATION_NON_TRANSPOSE,
                                 &alpha, matA, vecX, &beta, vecY, CUDA_R_32F,
                                 CUSPARSE_SPMV_ALG_DEFAULT, dBuffer) )


  CHECK_CUDA( cudaMemcpy(pack->output, dY, pack->num_rows * sizeof(float),
                           cudaMemcpyDeviceToHost) )


  // destroy matrix/vector descriptors
  CHECK_CUSPARSE( cusparseDestroySpMat(matA) )
  CHECK_CUSPARSE( cusparseDestroyDnVec(vecX) )
  CHECK_CUSPARSE( cusparseDestroyDnVec(vecY) )
  CHECK_CUSPARSE( cusparseDestroy(handle) )  

  //destory cuda memory objects
  CHECK_CUDA( cudaFree(dBuffer) )
  CHECK_CUDA( cudaFree(drows) )
  CHECK_CUDA( cudaFree(dcolumns) )
  CHECK_CUDA( cudaFree(dvalues) )
  CHECK_CUDA( cudaFree(dX) )
  CHECK_CUDA( cudaFree(dY) )

  __atomic_store_n(&(pack->finished), (bool)true, __ATOMIC_SEQ_CST); /* VERY IMPORTANT! */
  return NULL;
}


/* TODO: build the package, but otherwise shouldn't need to change much. 
 *
 * (NOTE: futhark_new_... is essentially a memcpy, these need to be replaced
 *  with stuff for cublas)
 */
extern "C" struct spMVPackage * 
spMVSpawn(
  int64_t nnz, // number of non zeros in submatrix
  int64_t num_rows, //number of rows in submatrix
  int64_t num_cols, //number of columns in submatrix
  int64_t * row_indices, //these need to start from 0 coming from MPL
  int64_t * col_indices,
  float * values,
  float * vector_values, // should be same size as num_cols
  float * output)
{
  // struct futhark_context *ctx = futStuff->ctx;
  struct spMVPackage *pack = (spMVPackage*)malloc(sizeof(struct spMVPackage));
  // pack->futStuff = futStuff;
  // pack->a = futhark_new_u8_1d(ctx, a, inputLen);

  pack->nnz = nnz;
  pack->num_rows = num_rows;
  pack->num_cols = num_cols;
  pack->row_indices = row_indices;
  pack->col_indices = col_indices;
  pack->values = values;
  pack->vector_values = vector_values;
  pack->output = output;
  pack->finished = false;

  asyncspMVFunc(pack);

  // if (0 != pthread_create(&(pack->friends), NULL, &asyncspMVFunc, pack)) {
  //   printf("ERROR: glue.c: futspMVSpawn: pthread_create failed\n");
  //   exit(1);
  // }

  return pack;
}

// extern "C" uint8_t spMVPoll(struct spMVPackage *pack) {
//   return pack->finished ? 1 : 0;
// }


/* TODO: memcpy from GPU back to pack->output
 *
 * (NOTE: futhark_values is equivalent of this memcpy. needs to be replaced) */
extern "C" void spMVFinish(
  struct spMVPackage * pack)
{
  // if (0 != pthread_join(pack->friends, NULL)) {
  //   printf("ERROR: glue.c: pthread_join failed\n");
  //   exit(1);
  // }

  free(pack);
}
