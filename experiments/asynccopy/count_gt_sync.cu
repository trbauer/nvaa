#include "count_gt.h"


extern "C"
__global__ void count_gt_sync(
    uint32_t *oups, // [grid_size]
    const uint32_t *inps) // [grid_size*EPT]
{
  __shared__ uint32_t smem[EPT * TPB]; // 2k floats => 8K of shared mem

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t *inp_st = inps + blockIdx.x * EPT * TPB;
  // for (int i = threadIdx.x; i < EPT * TPW * WPB; i += blockDim.x) {
  //  smem[i] = inps[inp_st + i];
  // }
  // load it in the same order as above
  // (other use cases might need that)
  for (int i = 0; i < EPT; i++) {
    smem[threadIdx.x * EPT + i] = inp_st[threadIdx.x * EPT + i];
  }
  __syncthreads();

  // find the number in this tile that are greater than our local id
  auto n = count_gt(smem, sizeof(smem) / sizeof(smem[0]));

  oups[gid] = n;
}
