#pragma once

#include <cstdio>
#include <cstdint>

// CUDA runtime
#include <cuda_runtime.h>
#include <cuda/pipeline>

#if __CUDA_ARCH__ >= 700
#include <cuda/barrier>
#endif
#include <cooperative_groups.h>
//
namespace cg = cooperative_groups;


static const size_t EPT = 16; // elements per thread (lane)
static const size_t TPB = 4 * 32; // threads per warp


struct opts {
  int verbosity = 0;
  int iterations = 1;
  bool check = false;
  size_t blocks_per_grid = 1024;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

constexpr opts DFT_OPTS;

// In windows of EPT*TPB counts the number of elements that are
// greater than the thread index (within block) mod the block size.
// E.g. given sequential data [0..grid_size-1]...
// Suppose EPT is 2 and TPB is 64.
// Then BLOCK[0] has:
//  0 1 2 3 .. 127
// threadIdx.x[0] => oups[0] = 127 // all except {0}
// threadIdx.x[1] => oups[1] = 126 // all except {0,1}
// ...
// threadIdx.x[63] => oups[63] = 64 // all except {0,1,..63}
// BLOCK[1] repeats the pattern because we mod all input values by 128
//          and the values loaded are the same per block...
//
static __device__ uint32_t count_gt(const uint32_t *smem, uint32_t smem_elems)
{
  uint32_t n = 0;
  for (int i = 0; i < smem_elems; i++) {
    if ((smem[i] % (EPT * TPB)) > threadIdx.x) {
      n++;
    }
  }
  return n;
}


extern "C"
__global__ void count_gt_sync(uint32_t *oups, const uint32_t *inps);
extern "C"
__global__ void count_gt_async(uint32_t *oups, const uint32_t *inps);
