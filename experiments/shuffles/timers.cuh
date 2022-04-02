#ifndef TIMERS_CUH
#define TIMERS_CUH

#include <cuda_runtime_api.h>

#include <cstdint>
#include <cstdio>

extern "C" static __device__ uint64_t get_globaltimer()
{
  uint64_t globaltimer;
  asm volatile("mov.u64  %0, %globaltimer;" : "=l"(globaltimer));
  return globaltimer;
}

#endif // TIMERS_CUH