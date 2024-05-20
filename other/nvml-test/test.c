#include <nvml.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

// from https://github.com/mnicely/nvml_examples/blob/master/nvmlClass.h
#ifndef NVML_RT_CALL
#define NVML_RT_CALL( call )                                                                                           \
    {                                                                                                                  \
        nvmlReturn_t status = ( call );                                                               \
        if ( status != NVML_SUCCESS )                                                                                  \
            fprintf( stderr,                                                                                           \
                     "ERROR: CUDA NVML call \"%s\" in line %d of file %s failed "                                      \
                     "with "                                                                                           \
                     "%s (%d).\n",                                                                                     \
                     #call,                                                                                            \
                     __LINE__,                                                                                         \
                     __FILE__,                                                                                         \
                     nvmlErrorString( status ),                                                                        \
                     status );                                                                                         \
    }
#endif  // NVML_RT_CALL


int main() {
  NVML_RT_CALL(nvmlInit());

  unsigned int num_devices;
  NVML_RT_CALL(nvmlDeviceGetCount_v2(&num_devices));

  nvmlDevice_t* devices = malloc(num_devices * sizeof(nvmlDevice_t));
  unsigned long lastSeenTimeStamp = 0;

  while(true) {
    for (int i = 0; i < num_devices; i++) {
      nvmlDevice_t d;
      NVML_RT_CALL(nvmlDeviceGetHandleByIndex_v2(i, &d));
      devices[i] = d;

      //int major, minor;
      //NVML_RT_CALL(nvmlDeviceGetCudaComputeCapability(d, &major, &minor));
      //printf("device %d capability %d.%d\n", i, major, minor);




      /* =====================================================================
       * nvmlDeviceGetProcessesUtilizationInfo
       *
       * This doesn't appear to be supported at all? (It's in the documentation
       * but maybe the documentation is for a different software version...?)
       */

      /*
      nvmlProcessesUtilizationInfo_t info;
      info.lastSeenTimeStamp = 0;
      info.procUtilArray = NULL;
      info.processSamplesCount = 1000;
      info.version = 0; // ?

      NVML_RT_CALL(nvmlDeviceGetProcessesUtilizationInfo(d, &info));
      info.procUtilArray =
        malloc(info.processSamplesCount * sizeof(nvmlProcessUtilizationSample_t));
      NVML_RT_CALL(nvmlDeviceGetProcessesUtilizationInfo(d, &info));
      */



      /* ======================================================================
       * nvmlDeviceGetProcessUtilization
       *
       * doesn't ever see any processes?
       */

      unsigned int buffer_size = 0;
      nvmlReturn_t status;
      status = nvmlDeviceGetProcessUtilization(d, NULL, &buffer_size, lastSeenTimeStamp);
      if (7 != status) {
        fprintf(stderr, "huh?\n");
      }
      //printf("desired buffer_size = %u\n", buffer_size);
      nvmlProcessUtilizationSample_t* buffer =
        malloc(buffer_size * sizeof(nvmlProcessUtilizationSample_t));
      unsigned int num_samples = buffer_size;
      NVML_RT_CALL(nvmlDeviceGetProcessUtilization(d, buffer, &num_samples, lastSeenTimeStamp));
      if (num_samples != 0) {
        printf("device %d desired %u actual %u\n", i, buffer_size, num_samples);
      }
      free(buffer);


      /* ======================================================================
       * nvmlDeviceGetUtilizationRates
       *
       * SUPER COARSE-GRAINED? Only updates a few times every second???
       */

      nvmlUtilization_t util;
      util.gpu = 0;
      util.memory = 0;
      NVML_RT_CALL(nvmlDeviceGetUtilizationRates(d, &util));
      printf("device %d utilization gpu %u memory %u\n", i, util.gpu, util.memory);
    }
  }
  return 0;
}
