#include <cuda_runtime.h>
// #include <cuda_runtime_api.h>

// https://nvidia.github.io/libcudacxx/extended_api/synchronization_primitives/barrier/init.html
#include <cuda/barrier>
#include <cooperative_groups.h>
// #if __CUDA_ARCH__ >= 900
// #endif // __CUDA_ARCH__ >= 900

// CUDA Async Data Copies
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#asynchronous-data-copies

// PTX completion mechanisms
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-asynchronous-copy-completion-mechanisms

// PTX cp.async.shared.global [vector]
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-cp-async

// reqntid{x, y, z}

// __cluster_dims__(2, 1, 1)
/*
extern "C"
__global__ void
async_adventures_wait_groups(
    uint32_t *dsts,
    const uint32_t *srcs,
    uint32_t src)
{
  __shared__ int32_t smem[8][64];
//  extern __shared__ int32_t smem2[];

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

//  smem2[threadIdx.y] = gid;

// async completions
// 1. async-groups
//    commit denotes what the group is: L, L, *C*, ...
//                                      ^^^^<-+++ group
//    wait waits for groups to finish
//
  // cp-size{, src-size}{, cache-policy}, ignore-src
  asm volatile ("cp.async.ca.shared.global [%0], [%1], 16, 16;" :: "l"(smem[0] + threadIdx.x), "l"(srcs + gid + 0x0) : "memory");
  asm volatile ("cp.async.ca.shared.global [%0], [%1], 16, 8;" :: "l"(smem[1] + threadIdx.x), "l"(srcs + gid + 0x4) : "memory");
//  asm volatile ("cp.async.ca.shared.global [%0], [%1], 16, ignore-src;" :: "l"(smem[2] + threadIdx.x), "l"(srcs + gid + 16) : "memory");
//                                                           ^^^^^^^^^^^ rejected...
  asm volatile ("cp.async.commit_group;" ::: "memory");
  //
  asm volatile ("cp.async.ca.shared.global [%0], [%1], 4;" :: "l"(smem[2] + threadIdx.x), "l"(srcs + gid + 0x8) : "memory");
  asm volatile ("cp.async.ca.shared.global [%0], [%1], 8, 4;" :: "l"(smem[3] + threadIdx.x), "l"(srcs + gid + 0xC) : "memory");
  asm volatile ("cp.async.ca.shared.global [%0], [%1], 16, 4;" :: "l"(smem[4] + threadIdx.x), "l"(srcs + gid + 0x10) : "memory");
  asm volatile ("cp.async.commit_group;" ::: "memory");

  asm volatile ("cp.async.wait_group 1;" ::: "memory");
//  asm volatile ("cp.async.wait_wall ;" ::: "memory"); equivalent to commit_group; wait_group 0;

  // needed for coherencey across groups
  __syncthreads();

  dsts[gid] = smem[threadIdx.y % 8][(threadIdx.x + 1) % 64]; // + smem2[threadIdx.x];

// OPENS:
//  * Is the __syncthreads() necessary? (Is the counter really global?)
//    ==> probably; the samples have it
//
//  * How does the .ZFILL work?  How can it tell that the source size is smaller in the final ISA.
//    How does the ISA encode the cp vs fill amount? I only see .128, not the .32
//    OPEN: write micro test to test if it works; e.g. cpy 16, 4
//
//  * Is the return counter in-order?
///  ==> YES: cp.async.wait_group "N ... or fewer of the most recent"...
//       PTX sample listing seems to indicate it too.
}

*/

static __device__ float4 operator+(float4 a, float4 b) {
  return make_float4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);
}

/*
extern "C"
__global__ void
async_adventures_mbar(
    float4 *dsts,
    const float4 *srcs,
    float4 src)
{
//  __shared__ alignas(16) float4 smems[2][256];
  __shared__ float4 smems[2][256];
#pragma nv_diag_suppress static_var_with_dynamic_init
  __shared__ cuda::barrier<cuda::thread_scope_block> mbar;

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  const auto shape = cuda::aligned_size_t<alignof(float4)>(sizeof(float4));
  // const auto shape = sizeof(float4);
  if (threadIdx.x == 0) {
    init(&mbar, blockDim.x);
  }
  __syncthreads();

  cuda::memcpy_async(
      smems[0] + threadIdx.x + threadIdx.y,
      srcs + gid +   0 * sizeof(float4) + threadIdx.y,
      shape, mbar);
  cuda::memcpy_async(
      smems[1] + threadIdx.x + threadIdx.y,
      srcs + gid + 256 * sizeof(float4) + threadIdx.y,
      shape, mbar);

  mbar.wait(mbar.arrive());

  float4 s1 = smems[0][(threadIdx.x + 1) % 256];
  float4 s2 = smems[0][(threadIdx.x + 2) % 256];

  dsts[gid] = s1 + s2;
}
*/



extern "C"
__global__ void
async_adventures_mbar(
    float *dsts,
    const float *srcs,
    float src)
{
  __shared__ float smems[1][4*256];
#pragma nv_diag_suppress static_var_with_dynamic_init
  __shared__ cuda::barrier<cuda::thread_scope_block> mbar;

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  const auto shape = cuda::aligned_size_t<alignof(float)>(sizeof(float));
  if (threadIdx.x == 0) {
    init(&mbar, blockDim.x);
  }
  __syncthreads();

  for (int k = 0; k < 4; k++) {
    cuda::memcpy_async(
        smems[0] + threadIdx.x,
        srcs + gid + k*256 * sizeof(float),
        shape, mbar);
  }

  mbar.wait(mbar.arrive());

  auto s1 = smems[0][(threadIdx.x + 1) % 256];
  auto s2 = smems[0][(threadIdx.x + 2) % 256];

  dsts[gid] = s1 + s2;
}


/*
#if __CUDA_ARCH__ >= 900
extern "C"
__global__ void
async_adventures_mbarrier(
    uint32_t *dsts,
    const uint32_t *srcs,
    uint32_t src)
{
  // using barrier = cuda::barrier<cuda::thread_scope_block>;
  __shared__  cuda::barrier<cuda::thread_scope_block> mbar;

  __shared__ int32_t smem[8][64];

  auto block = cooperative_groups::this_thread_block();

  if (block.thread_rank() == 0) {
    init(&mbar, ...);
  }

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

}
#endif // __CUDA_ARCH__ >= 900
*/