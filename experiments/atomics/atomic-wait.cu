#include <cstdio>

#include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx


//  relaxed, consume, acquire, release, acq_rel, seq_cst

__global__ void atomic_exch2_kernel(
  cuda::atomic<int32_t, cuda::thread_scope_system> *atm_sys,
  cuda::atomic<int32_t, cuda::thread_scope_device> *atm_dev,
  int32_t * dst, int32_t s)
{
  __shared__ cuda::atomic<int32_t, cuda::thread_scope_block> atms_blk[128];

  // int32_t curr = atm_dev[threadIdx.x].load(cuda::memory_order::relaxed);
  // int32_t curr = atm_dev[threadIdx.x].load(cuda::memory_order::acquire);
  int32_t curr = atm_dev[threadIdx.x].load(cuda::memory_order::seq_cst);
  atms_blk[threadIdx.x] = curr + 1;
  __syncthreads();
  if (threadIdx.y == 1) {
    auto &natm = atms_blk[threadIdx.y];
    natm = 0;
    natm.notify_one();
  }
  auto &atm = atms_blk[(threadIdx.x + 1) % 128];

  atm.wait(0, std::memory_order::acquire);
  // atm.wait(0, std::memory_order::acquire);

  dst[threadIdx.x] =  atm;
}
