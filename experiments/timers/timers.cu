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

extern "C" __global__ void glob_globaltimer_cost(uint32_t *cost_samples)
{
  uint32_t samples[GLOBAL_TIMER_COST_LOOP_TRIPS];
  uint64_t sum = 0;
  for (int i = 0; i < GLOBAL_TIMER_COST_LOOP_TRIPS; i++) {
    const auto st = clock64();
    sum += get_globaltimer();
    const auto en = clock64();
    samples[i] = (uint32_t)(en - st);
  }

  for (int i = 0; i < GLOBAL_TIMER_COST_LOOP_TRIPS; i++) {
    cost_samples[GLOBAL_TIMER_COST_LOOP_TRIPS * get_tid() + i] = samples[i];
  }
  if (sum > 0x7FFFFFFFFFFFFFFFull) {
    cost_samples[0] = 0;
  }
}

// fetch N delta samples really fast and return a sample set
extern "C" __global__ void glob_globaltimer_resolution(
  uint64_t *ticks, int timer_source)
{
  auto getSample = [&]() -> uint64_t {
    if (timer_source == 0) {
      return get_globaltimer();
    } else {
      return clock64();
    }
  };
  uint64_t samples[GLOBALTIMER_RES_SAMPLES];
  auto prev = getSample();
  for (int i = 0; i < GLOBALTIMER_RES_SAMPLES; i++) {
    auto t = getSample();
    samples[i] = t - prev;
    prev = t;
  }
  for (int i = 0; i < GLOBALTIMER_RES_SAMPLES; i++) {
    ticks[GLOBALTIMER_RES_SAMPLES * get_tid() + i] = samples[i];
  }
}

extern "C" __global__ void glob_globaltimer_resolution2(
  uint32_t *ticks_g, uint32_t *ticks_l)
{
  uint32_t samples_g[GLOBALTIMER_RES_SAMPLES], samples_l[GLOBALTIMER_RES_SAMPLES];
  uint64_t prev_g = get_globaltimer();
  uint64_t prev_l = clock64();
  for (int i = 0; i < GLOBALTIMER_RES_SAMPLES; i++) {
    uint64_t curr_g = get_globaltimer();
    samples_g[i] = (uint32_t)(curr_g - prev_g);
    prev_g = curr_g;
    //
    uint64_t curr_l = clock64();
    samples_l[i] = (uint32_t)(curr_l - prev_l);
    prev_l = curr_l;
  }
  for (int i = 0; i < GLOBALTIMER_RES_SAMPLES; i++) {
    ticks_g[GLOBALTIMER_RES_SAMPLES * get_tid() + i] = samples_g[i];
    ticks_l[GLOBALTIMER_RES_SAMPLES * get_tid() + i] = samples_l[i];
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