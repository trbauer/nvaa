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
#include <cuda_fp16.h>

extern "C"
__global__ void multimem_madness(
    float *f32_dsts,
    const float *f32_srcs,
    const __half2 *f16x2_srcs,
    int32_t *s32_dsts,
    const int32_t *s32_srcs)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  int32_t s32_vals[16] = { };
  float f32_vals[16] = { };

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-multimem-ld-reduce-multimem-st-multimem-red
// multimem.ld_reduce.and.b32                    val1_b32, [addr1];

  asm volatile ("multimem.ld_reduce.global.min.s32 %0, [%1];" : "=r"(s32_vals[0]) : "l"(s32_srcs + gid + 0) : "memory");

  asm volatile ("multimem.ld_reduce.acquire.gpu.global.add.f32 %0, [%1];" : "=f"(f32_vals[1]) : "l"(f32_srcs + gid + 0) : "memory");
  // __half f16_val_a, f16_val_b;
  union {
    __half    f16_val[2];
    uint16_t  u16_val[2];
  };
  u16_val[0] = u16_val[1] = 0;
  // asm volatile ("multimem.ld_reduce.add.acc::f32.v2.f16 %0, [%1];" : "=h"(u16_val[0]) : "l"(f16x2_srcs + gid + 0) : "memory");
  // asm volatile ("multimem.ld_reduce.add.acc::f32.v2.f16x2 {%0,%1}, [%2];" : "=h"(u16_val[0]), "=h"(u16_val[1]) : "l"(f16x2_srcs + gid + 0) : "memory");

  int32_t s32_sum = 0;
  for (auto v : s32_vals)
    s32_sum += v;

  float f32_sum = 0;
  f32_sum += __half2float(f16_val[0]) + __half2float(f16_val[1]);
  for (auto v : f32_vals)
    f32_sum += v;

  asm volatile ("multimem.st.relaxed.gpu.global.b32 [%0], %1;" :: "l"(s32_dsts + gid + 1), "r"(s32_sum) : "memory");
  asm volatile ("multimem.st.release.gpu.global.f32 [%0], %1;" :: "l"(f32_dsts + gid + 1), "f"(f32_sum) : "memory");

  asm volatile ("multimem.red.relaxed.gpu.global.add.f64  [%0], %1;" :: "l"(f32_dsts + gid + 1), "d"((double)f32_sum) : "memory");
  asm volatile ("multimem.red.relaxed.sys.global.add.f32  [%0], %1;" :: "l"(f32_dsts + gid + 2), "f"(f32_sum) : "memory");


  s32_dsts[gid] = s32_sum;
  f32_dsts[gid] = f32_sum;
}

