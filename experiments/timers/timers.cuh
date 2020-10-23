#ifndef TIMERS_HPP
#define TIMERS_HPP

#include <cuda_runtime_api.h>

#include <cstdint>
#include <cstdio>

using smid_t = uint32_t;
using block_id_t = uint32_t;
using ticks_t = int64_t;

// dispatched block id (a block id and which SM created it)
struct dbi {
  block_id_t   block;
  smid_t      sm;

  __host__ __device__ dbi(block_id_t bid, smid_t smid) : block(bid), sm(smid) { }
  __host__ __device__ dbi(uint32_t bits) : dbi(bits >> 16, (uint16_t)bits) { }
  __host__ __device__ dbi(uint64_t bits) : dbi((uint32_t)(bits >> 32), (uint32_t)bits) { }
};
//
// struct dbicg // clocks + global timer

extern "C" static __device__ int get_tid()
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  return id;
}

extern "C" static __device__ uint32_t get_smid()
{
  uint32_t smid;
  asm("mov.u32 %0, %smid;" : "=r"(smid));
  return smid;
}

// appears to be the number of compute units in OpenCL too
extern "C" static __device__ uint32_t get_nsmid()
{
  uint32_t nsmid;
  asm("mov.u32 %0, %nsmid;" : "=r"(nsmid));
  return nsmid;
}

extern "C" static __device__ uint64_t get_globaltimer()
{
  uint64_t globaltimer;
  asm("mov.u64  %0, %globaltimer;" : "=l"(globaltimer));
  return globaltimer;
}

extern "C" static __device__ uint32_t get_warpid()
{
  uint32_t warpid;
  asm("mov.u32 %0, %warpid;" : "=r"(warpid));
  return warpid;
}

struct warp_event {
  int64_t   at_start, at_end;
  uint16_t  smid;
  uint16_t  block_idx;
  uint8_t   warpid; // warp in block
//  uint8_t   _padding;

  __device__ inline warp_event(uint64_t st)
    : at_start(st)
  //  , at_end(clock64())
  //  , block_idx(blockIdx.x)
  //  , warpid(get_warpid())
  //  , smid(get_smid())
  {
    at_end = clock64();
    smid = get_smid();
    block_idx = blockIdx.x;
    warpid = get_warpid();
  }
  // to_json()
};


extern "C" static __device__ void write_warp_event(
  int64_t     at_start,
  warp_event *evt_stream,
  uint32_t   *evt_stream_ix)
{
  auto evt_idx = atomicInc(evt_stream_ix, 0xFFFFFFFF);
  evt_stream[evt_idx] = warp_event(at_start);
}


struct tstamps {
  smid_t      smid;
  block_id_t  block_idx;
  ticks_t     ticks;
  ticks_t     globaltimer; // in ns apparently

  __host__ __device__ tstamps()
  {
#if defined(__CUDA_ARCH__)
    smid = get_smid();
    block_idx = blockIdx.x;
    ticks = clock64();
    globaltimer = get_globaltimer();
#else
    smid = 0xFEFEFEFE;
    block_idx = 0xFEFEFEFE;
    ticks = 0xFEFEFEFEFEFEFEFE;
    globaltimer = 0xFEFEFEFEFEFEFEFE;
#endif // CUDA_ARCH
  }
};


#endif // TIMERS_HPP