#include "mincu.hpp"
#define IS_CUDA

#include "clocks.h"

using namespace mincu;

int main(int argc, const char *arv[])
{
  umem<uint32_t> out32 {128, const_seq_zero};
  umem<int32_t> out32_dt {128, const_seq_zero};
  clocks_delta<<<1,1>>>(out32, out32_dt);
  auto e0 = cudaDeviceSynchronize();
  if (e0 != cudaSuccess) {
    fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
  }
  std::cout << "%clock\n";
  out32.str(std::cout, 8);
  std::cout << "curr - prev\n";
  out32_dt.str(std::cout, 8);
  return EXIT_SUCCESS;
}
