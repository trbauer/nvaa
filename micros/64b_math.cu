#include "mincu.hpp"

#include <cuda_runtime_api.h>

#include <cctype>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>


using namespace mincu;

static const uint32_t GRID_SIZE = 64;
static const uint32_t BLOCK_SIZE = 32;
// static_cast(GRID_SIZE%BLOCK_SIZE == 0, "block size must divide grid size");

__global__ void add64(
        int64_t *dsts,
  const int64_t *src0s,
        int64_t  src1)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int64_t dst = src0s[id] + src1;
  dsts[id] = dst;
}

__global__ void add64(
        uint64_t *dsts,
  const uint64_t *src0s,
        uint64_t  src1)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int64_t dst = src0s[id] + src1;
  dsts[id] = dst;
}

__global__ void add64_x16(
        uint64_t *dsts,
  const uint64_t *src0s)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int64_t sum = 0;
  for (int i = 0; i < 16; i++) {
    sum += src0s[id+i];
  }
  dsts[id] = sum;
}


__global__ void mul64(
        int64_t *dsts,
  const int64_t *src0s,
  const int64_t *src1s)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int64_t dst = src0s[id] * src1s[id];
  dsts[id] = dst;
}

__global__ void mul64(
        uint64_t *dsts,
  const uint64_t *src0s,
  const uint64_t *src1s)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  uint64_t dst = src0s[id] * src1s[id];
  dsts[id] = dst;
}



template <typename T>
static void run_test(T src1)
{
  umem<T> IN(GRID_SIZE);
  for (T i = 0; i < GRID_SIZE; i++) {
    IN[i] = i;
  }
  umem<T> OUT(GRID_SIZE, init_const<>(0x0));
  add64<<<GRID_SIZE/BLOCK_SIZE,BLOCK_SIZE>>>(OUT, IN, src1);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (" ,cudaGetErrorString(e), "): unexpected error");
  } else {
    OUT.str(std::cout);
  }
}


int main(int argc, const char* argv[])
{
  if (false) {
    run_test<uint64_t>(1);
  } else {
    run_test<int64_t>(1);
  }


  return EXIT_SUCCESS;
}
