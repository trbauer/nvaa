#include <cstdio>

#include <cuda_runtime.h>


// like a pure annotation
// __attribute__((const)) __device__ int get(int in);

// __device__ __managed__ int ret[1000];


extern "C"
__global__ void
__cluster_dims__(2, 1, 1) cluster_groups(
    uint32_t *dsts,
    const uint32_t *srcs,
    uint32_t src)
{
  __shared__ int32_t smem[16];

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

  int32_t cta_rank = -1;
  asm volatile ("getctarank.shared::cluster.u32 %0, %1;"
    : "=r"(cta_rank) : "r"(smem[4]) : "memory");

  int32_t a;
  asm volatile ("mapa.shared::cluster.u32 %0, %1, %2;"
    : "=r"(a) : "r"(smem[4]), "r"(cta_rank) : "memory");

  dsts[gid] = a;
}