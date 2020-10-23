#include "timers.cuh"

#include <cstdio>

// RESEARCH QUESTION: how accurate is nanosleep
// [0...2n] is what we read
//
// QUESTIONS:
//   what PTX does clock64() use (%globaltimer?)

// __nanosleep
// The sleep duration is approximated, but guaranteed to be in the
// interval [0, 2*t].  The implementation may reduce the sleep duration for
// individual threads within a warp such that all sleeping threads in the
// warp wake up together.



// %globaltimer (A predefined, 64-bit global nanosecond timer.)
//   https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-globaltimer
//
//
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-warpid
// %warpid  %nwarpid

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-smid
// %smid and %nsmid  (may not be contiguous)

extern "C" static __device__ uint32_t get_dbi_bits()
{
//  uint32_t w_id;
//  uint32_t nw_id;
//  asm("mov.u32 %0, %warpid;" : "=r"(w_id));
//  asm("mov.u32 %0, %nwarpid;" : "=r"(nw_id));

//  printf(" #%03d ==> @%04d  (%d)\n", blockIdx.x, sm_id, nsm_id);
  return ((uint32_t)blockIdx.x << 16) | (uint16_t)get_smid();
}



extern "C" __global__ void glob_get_nsmid(int *nsmid)
{
  if (blockIdx.x == 0)
    *nsmid = get_nsmid();
}


extern "C" __global__ void glob_block_dist(
  int64_t *times, uint32_t *ids, long delay)
{
  auto now = clock64();
  if (delay > 0)
    __nanosleep(delay);
  ids[get_tid()] = get_dbi_bits();
  times[get_tid()] = now;
}

// __device__ warp_event::warp_event(uint64_t st)

extern "C" __global__ void glob_block_dist_atomic(
  warp_event *evt_stream, uint32_t *evt_stream_idx, long delay)
{
  auto st = clock64();
  if (delay > 0)
    __nanosleep(delay);
  write_warp_event(st, evt_stream, evt_stream_idx);
}


extern "C" __global__ void glob_get_times(tstamps *tss)
{
  tss[get_tid()] = tstamps();
}

extern "C" __global__ void glob_globaltimer_cost(
  int *cost, int count, uint64_t *unused)
{
  const auto st = clock64();
  ticks_t sum = 0;
  for (int i = 0; i < count; i++) {
    sum += get_globaltimer();
  }
  const auto en = clock64();

  cost[get_tid()] = (int)(en - st);

  if (get_tid() > 1000000) {
    unused[0] = sum;
  }
}



/*
extern "C" static __device__ __noinline__ int function_nodelay(long delay)
{
  int id = get_tid();
  return id;
}
extern "C" static __device__ __noinline__ int function_delay(long delay)
{
  int id = get_tid();
  __nanosleep(delay);
  return id;
}




extern "C" __global__ void glob_init(int64_t *times, uint32_t *ids, long delay)
{
  auto st = clock64();
  auto id = function_nodelay(delay);
  auto en = clock64();

  times[id] = (int)(en - st);
  ids[id] = get_phys_id();
}

extern "C" __global__ void glob_nanosleep(int64_t *times, uint32_t *ids, long delay)
{
  auto st = clock64();
  auto id = function_delay(delay);
  auto en = clock64();

  times[id] = (int)(en - st);
  ids[id] = get_phys_id();
}

extern "C" __global__ void glob_clock_value(int64_t *times, uint32_t *ids)
{
  auto clock_value = clock64();
  auto id = get_tid();
  times[id] = clock_value;
  ids[id] = get_phys_id();
}

*/