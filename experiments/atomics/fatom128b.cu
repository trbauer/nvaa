#include <cuda_fp16.h>
// #include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx
#include <cstdio>



// static __device__  min(uint4 *addr, uint4 cmp, uint4 swap)
// {


//  asm volatile ("atom.global.v2.f32.add  {%0, %1}, [%2], {%3, %4};"
//    : "=f"(ret.x), "=f"(ret.y) : "l"(dst + gid), "f"(src.x), "f"(src.y));

// atom{.sem}{.scope}{.global}.add{.level::cache_hint}.vec_32_bit.f32                  d, [a], b{, cache-policy};
// atom{.sem}{.scope}{.global}.op.noftz{.level::cache_hint}.vec_16_bit.half_word_type  d, [a], b{, cache-policy};
// atom{.sem}{.scope}{.global}.op.noftz{.level::cache_hint}.vec_32_bit.packed_type     d, [a], b{, cache-policy};

// .sem =               { .relaxed, .acquire, .release, .acq_rel };
// .scope =             { .cta, .gpu, .sys };
// .op =                { .add, .min, .max };
// .half_word_type =    { .f16, .bf16 };
// .packed_type =       { .f16x2, .bf16x2 };
// .vec_16_bit =        { .v2, .v4, .v8 }
// .vec_32_bit =        { .v2, .v4 };
// .level::cache_hint = { .L2::cache_hint }



extern "C"
__global__ void atomicf32v4(float4 * dst, float4 src0)
{
  // https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#atomicadd
//  __shared__ float4 smem[128];
//  smem[threadIdx.x] = make_float4(threadIdx.x, threadIdx.y, threadIdx.z, 0);
//  __syncthreads();
  auto val = src0;
//  val = atomicAdd(smem + threadIdx.x, val); // shared: with return
//  __syncthreads();
  val = atomicAdd(dst + threadIdx.x, val); // global: with return
  val = atomicAdd_block(dst + threadIdx.y, val); // global: with return
  val = atomicAdd_system(dst + threadIdx.z, val); // global: with return
  atomicAdd(dst + threadIdx.y, val); // global without return
}
extern "C"
__global__ void atomicf32v2(float2 * dst, float2 src0)
{
  // https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#atomicadd
//  __shared__ float2 smem[128];
//  smem[threadIdx.x] = make_float2(threadIdx.x, threadIdx.y);
//  __syncthreads();
  auto val = src0;
//  val = atomicAdd(smem + threadIdx.x, val); // shared: with return
//  __syncthreads();
  val = atomicAdd(dst + threadIdx.x, val); // global: with return
  atomicAdd(dst + threadIdx.y, val); // global without return
}



/*
__device__ ushort8 atomicAddH8(ushort8 *addr, ushort8 src)
{
  ushort8 ret;
  asm volatile (
    "atom.global.add.noftz.v8.f16 {%0, %1, %2, %3, %4, %5, %6, %7}, [%8], {%9, %10, %11, %12, %13, %14, %15, %16};"
    : "=h"(ret.s[0]), "=h"(ret.s[1]), "=h"(ret.s[2]), "=h"(ret.s[3]),
      "=h"(ret.s[4]), "=h"(ret.s[5]), "=h"(ret.s[6]), "=h"(ret.s[7])
    : "l"(addr),
      "h"(src.s[0]), "h"(src.s[1]), "h"(src.s[2]), "h"(src.s[3]),
      "h"(src.s[4]), "h"(src.s[5]), "h"(src.s[6]), "h"(src.s[7])
    : "memory");
  return ret;
}
__device__ ushort8 atomicMinH8(ushort8 *addr, ushort8 src)
{
  ushort8 ret;
  asm volatile (
    "atom.global.min.noftz.v8.f16 {%0, %1, %2, %3, %4, %5, %6, %7}, [%8], {%9, %10, %11, %12, %13, %14, %15, %16};"
    : "=h"(ret.s[0]), "=h"(ret.s[1]), "=h"(ret.s[2]), "=h"(ret.s[3]),
      "=h"(ret.s[4]), "=h"(ret.s[5]), "=h"(ret.s[6]), "=h"(ret.s[7])
    : "l"(addr),
      "h"(src.s[0]), "h"(src.s[1]), "h"(src.s[2]), "h"(src.s[3]),
      "h"(src.s[4]), "h"(src.s[5]), "h"(src.s[6]), "h"(src.s[7])
    : "memory");
  return ret;
}
__device__ ushort8 atomicMaxH8(ushort8 *addr, ushort8 src)
{
  ushort8 ret;
  asm volatile (
    "atom.global.max.noftz.v8.f16 {%0, %1, %2, %3, %4, %5, %6, %7}, [%8], {%9, %10, %11, %12, %13, %14, %15, %16};"
    : "=h"(ret.s[0]), "=h"(ret.s[1]), "=h"(ret.s[2]), "=h"(ret.s[3]),
      "=h"(ret.s[4]), "=h"(ret.s[5]), "=h"(ret.s[6]), "=h"(ret.s[7])
    : "l"(addr),
      "h"(src.s[0]), "h"(src.s[1]), "h"(src.s[2]), "h"(src.s[3]),
      "h"(src.s[4]), "h"(src.s[5]), "h"(src.s[6]), "h"(src.s[7])
    : "memory");
  return ret;
}
*/

struct alignas(16) ushort8 {uint16_t s[8];};
struct alignas(8) ushort4x {uint16_t s[4];};

// e.g. MK_ATOM_FUNC(min, bf16)
#define MK_ATOM_FUNC_V8(OP,TYPE) \
  __device__ ushort8 atomic_ ## OP ## _ ## TYPE ## _v8(ushort8 *addr, ushort8 src) \
  { \
    ushort8 ret; \
    asm volatile ( \
      "atom.global." #OP ".noftz.v8." #TYPE " {%0, %1, %2, %3, %4, %5, %6, %7}, [%8], {%9, %10, %11, %12, %13, %14, %15, %16};" \
      : "=h"(ret.s[0]), "=h"(ret.s[1]), "=h"(ret.s[2]), "=h"(ret.s[3]), \
        "=h"(ret.s[4]), "=h"(ret.s[5]), "=h"(ret.s[6]), "=h"(ret.s[7]) \
      : "l"(addr), \
        "h"(src.s[0]), "h"(src.s[1]), "h"(src.s[2]), "h"(src.s[3]), \
        "h"(src.s[4]), "h"(src.s[5]), "h"(src.s[6]), "h"(src.s[7]) \
      : "memory"); \
    return ret; \
  }
#define MK_ATOM_FUNC_V4(OP,TYPE) \
  __device__ ushort4x atomic_ ## OP ## _ ## TYPE ## _v4(ushort4x *addr, ushort4x src) \
  { \
    ushort4x ret; \
    asm volatile ( \
      "atom.global." #OP ".noftz.v4." #TYPE " {%0, %1, %2, %3}, [%4], {%5, %6, %7, %8};" \
      : "=h"(ret.s[0]), "=h"(ret.s[1]), "=h"(ret.s[2]), "=h"(ret.s[3]) \
      : "l"(addr), \
        "h"(src.s[0]), "h"(src.s[1]), "h"(src.s[2]), "h"(src.s[3]) \
      : "memory"); \
    return ret; \
  }

MK_ATOM_FUNC_V8(add, bf16);
MK_ATOM_FUNC_V8(min, bf16);
MK_ATOM_FUNC_V8(max, bf16);
MK_ATOM_FUNC_V8(add, f16);
MK_ATOM_FUNC_V8(min, f16);
MK_ATOM_FUNC_V8(max, f16);
//
MK_ATOM_FUNC_V4(add, bf16);
MK_ATOM_FUNC_V4(min, bf16);
MK_ATOM_FUNC_V4(max, bf16);
MK_ATOM_FUNC_V4(add, f16);
MK_ATOM_FUNC_V4(min, f16);
MK_ATOM_FUNC_V4(max, f16);

extern "C"
__global__ void atomic_f16_v8(ushort8 * dst, ushort8 src0)
{
  auto val = src0;
  val = atomic_add_f16_v8(dst + threadIdx.x, val);
  atomic_add_f16_v8(dst + threadIdx.y, val);
  val = atomic_min_f16_v8(dst + threadIdx.x, val);
  atomic_min_f16_v8(dst + threadIdx.y, val);
  val = atomic_max_f16_v8(dst + threadIdx.x, val);
  atomic_max_f16_v8(dst + threadIdx.y, val);
}

extern "C"
__global__ void atomic_bf16_v8(ushort8 * dst, ushort8 src0)
{
  auto val = src0;
  val = atomic_add_bf16_v8(dst + threadIdx.x, val);
  atomic_add_bf16_v8(dst + threadIdx.y, val);
  val = atomic_min_bf16_v8(dst + threadIdx.x, val);
  atomic_min_bf16_v8(dst + threadIdx.y, val);
  val = atomic_max_bf16_v8(dst + threadIdx.x, val);
  atomic_max_bf16_v8(dst + threadIdx.y, val);
}

extern "C"
__global__ void atomic_f16_v4(ushort4x * dst, ushort4x src0)
{
  auto val = src0;
  val = atomic_add_f16_v4(dst + threadIdx.x, val);
  atomic_add_f16_v4(dst + threadIdx.y, val);
  val = atomic_min_f16_v4(dst + threadIdx.x, val);
  atomic_min_f16_v4(dst + threadIdx.y, val);
  val = atomic_max_f16_v4(dst + threadIdx.x, val);
  atomic_max_f16_v4(dst + threadIdx.y, val);
}

extern "C"
__global__ void atomic_bf16_v4(ushort4x *dst, ushort4x src0)
{
  auto val = src0;
  val = atomic_add_bf16_v4(dst + threadIdx.x, val);
  atomic_add_bf16_v4(dst + threadIdx.y, val);
  val = atomic_min_bf16_v4(dst + threadIdx.x, val);
  atomic_min_bf16_v4(dst + threadIdx.y, val);
  val = atomic_max_bf16_v4(dst + threadIdx.x, val);
  atomic_max_bf16_v4(dst + threadIdx.y, val);
}