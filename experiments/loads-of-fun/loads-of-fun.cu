// 1. Compile with something like
// % nvcc -arch sm_90 micro.cu -cubin --generate-line-info -o micro-sm_90.cubin
// 2. Then disassemble with:
// % nvdisasm micro-sm_90.cubin --print-line-info --print-code
#include <cstdio>

#include <cuda_runtime.h>


// like a pure annotation
// __attribute__((const)) __device__ int get(int in);

// __device__ __managed__ int ret[1000];

extern "C"
__global__ void loads_of_fun(
    float *dsts,
    const float *srcs,
    const float *unified_srcs)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  float vals[23] = { };

  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#id82
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-ld
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-ld-global-nc
  asm volatile ("ld.global.b32 %0, [%1];" : "=f"(vals[0]) : "l"(srcs + gid + 0) : "memory");
  //
  asm volatile ("ld.global.ca.b32 %0, [%1];" : "=f"(vals[1]) : "l"(srcs + gid + 1) : "memory");
  asm volatile ("ld.global.cg.b32 %0, [%1];" : "=f"(vals[2]) : "l"(srcs + gid + 2) : "memory");
  asm volatile ("ld.global.cs.b32 %0, [%1];" : "=f"(vals[3]) : "l"(srcs + gid + 3) : "memory");
  asm volatile ("ld.global.lu.b32 %0, [%1];" : "=f"(vals[4]) : "l"(srcs + gid + 4) : "memory");
  asm volatile ("ld.global.cv.b32 %0, [%1];" : "=f"(vals[5]) : "l"(srcs + gid + 5) : "memory");

  asm volatile ("ld.global.L2::64B.b32 %0, [%1];" : "=f"(vals[6]) : "l"(srcs + gid + 6) : "memory");
  asm volatile ("ld.global.L2::128B.b32 %0, [%1];" : "=f"(vals[7]) : "l"(srcs + gid + 7) : "memory");
#if __CUDA_ARCH__ >= 900
  asm volatile ("ld.global.L2::256B.b32 %0, [%1];" : "=f"(vals[8]) : "l"(srcs + gid + 8) : "memory");
#else
// #error __CUDA_ARCH__
#endif
  asm volatile ("ld.weak.global.L1::evict_normal.b32 %0, [%1];" : "=f"(vals[9]) : "l"(srcs + gid + 9) : "memory");
  asm volatile ("ld.weak.global.L1::evict_unchanged.b32 %0, [%1];" : "=f"(vals[10]) : "l"(srcs + gid + 10) : "memory");
  asm volatile ("ld.weak.global.L1::evict_first.b32 %0, [%1];" : "=f"(vals[11]) : "l"(srcs + gid + 11) : "memory");
  asm volatile ("ld.weak.global.L1::evict_last.b32 %0, [%1];" : "=f"(vals[12]) : "l"(srcs + gid + 12) : "memory");
  asm volatile ("ld.weak.global.L1::no_allocate.b32 %0, [%1];" : "=f"(vals[13]) : "l"(srcs + gid + 13) : "memory");

  asm volatile ("ld.relaxed.cta.global.b32 %0, [%1];" : "=f"(vals[14]) : "l"(srcs + gid + 14) : "memory");
  asm volatile ("ld.acquire.cta.global.b32 %0, [%1];" : "=f"(vals[15]) : "l"(srcs + gid + 15) : "memory");

  asm volatile ("ld.mmio.relaxed.sys.global.b32 %0, [%1];" : "=f"(vals[16]) : "l"(srcs + gid + 16) : "memory");

  // ld.nc (non coherent)
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-ld-global-nc
  // (comes from the texture cache and is read only)
  //   "On some architectures, the texture cache is larger, has higher bandwidth,
  //   and longer latency than the global memory cache. For applications with
  //   sufficient parallelism to cover the longer latency, ld.global.nc
  //   should offer better performance than ld.global on such architectures."
  // maps to LDG.E.CONSTANT
  asm volatile ("ld.global.nc.b32    %0, [%1];" : "=f"(vals[17]) : "l"(srcs + gid + 17) : "memory");
  asm volatile ("ld.global.ca.nc.b32 %0, [%1];" : "=f"(vals[18]) : "l"(srcs + gid + 18) : "memory");
  asm volatile ("ld.global.cg.nc.b32 %0, [%1];" : "=f"(vals[19]) : "l"(srcs + gid + 19) : "memory");
  asm volatile ("ld.global.cs.nc.b32 %0, [%1];" : "=f"(vals[20]) : "l"(srcs + gid + 20) : "memory");

#if CUDA_ARCH >= 900
  // .global .attribute(.unified(19,95)) .f32 f;
  // .global .attribute(.managed) .s32 g;
  // .global .attribute(.managed) .u64 x;

  // .global .attribute(.unified(19,95)) .f32 f;

  // .func .attribute(.unified(0xAB, 0xCD)) bar() { ... }
  asm volatile (
      ".global .attribute(.unified(19,95)) .f32 f;\n"
      "ld.weak.global.b32 %0, [&f].unified;\n"
      : "=f"(vals[21]) :: "memory");
  // asm volatile (
  //     ".global .attribute(.unified(19,95)) .f32 f;\n"
  //     "ld.weak.global.b32 %0, [&f].unified;\n"
  //     // "ld.weak.global.b32 %0, [%1].unified;\n"
  //     : "=f"(vals[21]) : "l"(unified_srcs + gid + 1) : "memory");
#endif

// PTX ASM rejects this
//  asm volatile (
//    "createpolicy.fractional.L2::evict_last.b64 policy, 0.5;\n"
//    "ld.global.nc.L2::cache_hint.f32  %0, [%1], policy;\n"
//    : "=f"(vals[22]) : "l"(srcs + gid + 22) : "memory");

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-createpolicy
// createpolicy.fractional.L2::evict_last.b64                      policy, 1.0;
// createpolicy.fractional.L2::evict_last.L2::evict_unchanged.b64  policy, 0.5;
//
// createpolicy.range.L2::evict_last.L2::evict_first.b64
//                                             policy, [ptr], 0x100000, 0x200000;


  float sum = 0.0f;
  for (float v : vals)
    sum += v;

  dsts[gid] = sum;
}

