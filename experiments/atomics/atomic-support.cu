#include <cuda_runtime.h>

// #include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx
#include <array>
#include <cstdio>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include "mincu.hpp"

using namespace mincu;

//  asm volatile ("atom.global.v2.f32.add  {%0, %1}, [%2], {%3, %4};"
//    : "=f"(ret.x), "=f"(ret.y) : "l"(dst + gid), "f"(src.x), "f"(src.y));

__global__ void atomics(
    uint64_t *dstsU64,
    int64_t *dstsI64,
    double *dstsF64,
//    __half *dstsF16,
//    __half2 *dstsF16x2,
//    __nv_bfloat16 *dstsBF16,
//    __nv_bfloat16 *dstsBF16x2,
    int zero)
{
//  __shared__ uint64_t smemU64[32];
  __shared__ int64_t smemI64[32];
//  __shared__ double smemF64[32];
//  __shared__ dstsF16 smemF16[32];
//  __shared__ dstsF16x2 smemF16x2[32];
//  __shared__ dstsBF16 smemBF16[32];
//  __shared__ dstsBF16x2 smemBF16x2[32];

//  x += (double)threadIdx.x;

  // init SLM
//  if (threadIdx.x < 32) {
//    smemU64[threadIdx.x] = (uint64_t)gid;
//    dstsI64[threadIdx.x] = (int64_t)gid;
//    dstsF64[threadIdx.x] = (double)gid;
//  }

  // forces pointer to be generic
  if (threadIdx.x % 2) {
//    dstsU64 = smemU64 + threadIdx.x % 32;
    dstsI64 = smemI64 + threadIdx.x % 32;
//    dstsF64 = smemF64 + threadIdx.x % 32;
//    const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
//    dstsU64[gid] =
//      atomicAdd_block(smemU64 + threadIdx.x % 32, (uint64_t)threadIdx.x);
//    atomicAdd(smemF64 + threadIdx.x % 32, (double)threadIdx.x);
  } else {
    const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
//    dstsU64 += gid;
    dstsI64 += gid;
//    dstsF64 += gid;
  }
// const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
// dstsF64[gid] = smemF64[threadIdx.x];

//  atomicAdd(dstsU64 + 0, (uint64_t)threadIdx.x);
//  atomicMin(dstsU64 + 1, (uint64_t)threadIdx.x);
//  atomicMax(dstsU64 + 2, (uint64_t)threadIdx.x);
//  atomicExch(dstsU64 + 3, (uint64_t)threadIdx.x);
//  atomicInc(dstsU64 + 4, (uint64_t)threadIdx.x);
//  atomicDec(dstsU64 + 5, (uint64_t)threadIdx.x);

//  uint64_t retU64;
//  asm volatile ("atom.u64.add  %0, [%1], %2;"
//    : "=l"(retU64) : "l"(dstsU64 + 4), "l"((uint64_t)threadIdx.x));
//  (void)retU64;
//
//  atomicAdd(dstsI64 + 0, (int64_t)threadIdx.x);
  atomicMin(dstsI64 + 1, (int64_t)threadIdx.x);
//  atomicMax(dstsI64 + 2, (int64_t)threadIdx.x);
//  atomicExch(dstsI64 + 3, (int64_t)threadIdx.x);
//  atomicInc(dstsI64 + 4, (int64_t)threadIdx.x);
//  atomicDec(dstsI64 + 5, (int64_t)threadIdx.x);

//  uint64_t retU64;
//  asm volatile ("atom.f64.min %0, [%1], %2;"
//    : "=l"(retU64) : "l"(dstsF64 + 4), "l"((uint64_t)threadIdx.x));
//  (void)retU64;

//  atomicAdd(dstsF64 + 0, (double)threadIdx.x);
//  atomicMin(dstsF64 + 1, (double)threadIdx.x);
//  atomicMax(dstsF64 + 2, (double)threadIdx.x);
//  atomicExch(dstsF64 + 3, (double)threadIdx.x);
//  if (dstsU64[threadIdx.x] == 0) {
//    dstsU64[threadIdx.x] = 2;
//  }
//  uint64_t retU64;
//  asm volatile ("atom.u64.add  %0, [%1], %2;"
//    : "=l"(retU64) : "l"(dstsU64 + 4), "l"((uint64_t)threadIdx.x));

/*
  int is_spc;
  asm volatile (
    "{\n"
    ".reg .pred p0;\n"
//    "isspacep.const  p0, %1;\n"
//   "isspacep.shared  p0, %1;\n"
//    "isspacep.shared::cta  p0, %1;\n"
//    "isspacep.shared::cluster p0, %1;\n"
//    "isspacep.global p0, %1;\n"
    "isspacep.param p0, %1;\n"
    "selp.u32  %0, 1, 0, p0;\n"
    "}\n"
    : "=r"(is_spc) : "l"(dstsU64) : "memory");

  dstsU64[threadIdx.x] = is_spc;
*/
}





/*

int main(int argc, const char **argv)
{
  umem<unsigned> oups(32, init_const<unsigned>(0u));
  umem<float> inps(32, init_const<float>(0.0f));
  inps[1] = -0.0f;
  inps[3] = 1.0f;
  inps[8] = 2.0f;
  inps[9] = 2.0f;
  inps.str(std::cout, 8, 3);

  micro<<<1,32>>>(oups, inps);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  oups.str(std::cout, 8);

  return EXIT_SUCCESS;
}

*/