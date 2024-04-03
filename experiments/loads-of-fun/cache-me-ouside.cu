// d=f64
// f=f32
// l=b64
// r=b32
// h=?32
// rd=.u64?
// p= 32b condition register
#include <cstdio>
#include <cstdint>

#include <cuda_runtime.h>


// [ applypriority.global.L2::evict_normal [ptr], 128; ]

extern "C"
__device__ __noinline__
void prefetch_tensor(const int32_t *t)
{
  // asm volatile ("prefetch.tensormap [%0];" :: "l"(s32_srcs + gid + 3) : "memory");
  // asm volatile ("prefetch.tensormap_space.tensormap [%0];" :: "l"(s32_srcs + gid + 3) : "memory");
  asm volatile("prefetch.const.tensormap [%0];" :: "l"(t) : "memory");
}

extern "C"
__global__ void cache_me_ouside(
    int32_t *s32_dsts,
    const int32_t *s32_srcs)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  // prefetch*
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-prefetch-prefetchu
  asm volatile ("prefetchu.L1  [%0];" :: "l"(s32_srcs + gid + 0) : "memory");
  asm volatile ("prefetch.L1  [%0];" :: "l"(s32_srcs + gid + 1) : "memory");
  asm volatile ("prefetch.L2  [%0];" :: "l"(s32_srcs + gid + 2) : "memory");
  prefetch_tensor(s32_srcs + gid + 3);

  // applypriority
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-applypriority
  asm volatile ("applypriority.global.L2::evict_normal [%0], 128;" :: "l"(s32_srcs + gid + 4) : "memory");

  // discard
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-discard
  asm volatile ("discard.global.L2 [%0], 128;" :: "l"(s32_srcs + gid + 5) : "memory");


  // createpolicy.fractional.L2::evict_last.L2::evict_unchanged.b64 cache-policy, 1;
  // PTX assembler dislikes - maybe in cache-policy?
  // I tried cache_policy.   Unknown symbol 'cache_policy'

  // createpolicy
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-createpolicy
  // asm volatile ("createpolicy.range.L2::evict_last.L2::evict_first.b64 "
  //                 "policy, [%0], 0x100000, 0x200000;" :: "l"(s32_srcs + gid + 5) : "memory");

  // asm volatile (
  //   "createpolicy.fractional.L2::evict_last.b64 cache_policy, 0.5;\n"
  //   "st.global.L2::cache_hint.b32  [%0], %1, cache_policy;\n" :: "l"(s32_srcs + gid + 6), "r"(gid) : "memory");

}

