#include "count_gt.h"


extern "C"
__global__ void count_gt_async(
          uint32_t *oups, // [grid_size]
    const uint32_t *inps) // [grid_size*EPT]
{
  __shared__ uint32_t smem[EPT * TPB];

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t *inp_st = inps + blockIdx.x * EPT * TPB;

  cuda::pipeline<cuda::thread_scope_thread> pipeline = cuda::make_pipeline();

  // each block works processes (EPT * TPW * WPB)

  // warp0
  // T0 loads [  0,    1 ...  EPT-1]
  // T1 loads [EPT,EPT+1,...2*EPT-1]
  // ...
  // T31 ...
  //
  // warp1
  // T32 loads [32*EPT,...]
  // T33 ...
  //
  const auto shape = cuda::aligned_size_t<alignof(uint32_t)>(EPT * sizeof(uint32_t));

  pipeline.producer_acquire();
  cuda::memcpy_async(
    smem + EPT * threadIdx.x,
    inp_st + threadIdx.x * EPT,
    shape,
    pipeline); // maybe several cp.async
  pipeline.producer_commit();
  pipeline.consumer_wait();
  __syncthreads();

  // find the number in this tile that are greater than our local id
  auto n = count_gt(smem, sizeof(smem) / sizeof(smem[0]));

  oups[gid] = n;
}
