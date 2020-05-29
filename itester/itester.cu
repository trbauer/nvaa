#include "mincuda.hpp"

#include <cuda_runtime_api.h>

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>

static const uint32_t GRID_SIZE = 64;
static const uint32_t BLOCK_SIZE = 32;

// static_cast(GRID_SIZE%BLOCK_SIZE == 0, "block size must divide grid size");


extern "C"
__global__ void test_insts(
  uint32_t *OUT,
  const uint32_t *IN,
  uint32_t arg1,
  uint32_t arg2)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int sum = 0;
  for (int i = 0; i < 32; i++) {
    sum += IN[(id + i)%32] + arg1 + arg2;
  }
  OUT[id] = sum;
}

template <typename...Ts>
void emit(std::ostream &) { }
template <typename T, typename...Ts>
void emit(std::ostream &os, const T &t, Ts... ts) {
  os << t;
  emit(os, ts...);
}

template <typename T, typename...Ts>
static void fatal(const T &t0, Ts... ts)
{
  emit(std::cerr, t0, ts...);
  std::cerr << "\n";
  exit(EXIT_FAILURE);
}


int main(int argc, const char* argv[])
{
  int verbosity = 0;
  for (int ai = 1; ai < argc; ai++) {
    std::string arg(argv[ai]);

    auto badArg = [&](const char *msg) {
      fatal(arg, ": ", msg);
    };

    if (arg == "-h" || arg == "--help") {
      std::cout <<
        "usage: itester OPTS\n"
        "where OPTS are:\n"
        "  -v      verbose output\n"
        "";
      return EXIT_SUCCESS;
    } else if (arg == "-v" || arg == "--verbose") {
      verbosity = 1;
    } else {
      badArg("unrecognized option");
    }
  }

  umem<uint32_t> IN(GRID_SIZE);
  for (uint32_t i = 0; i < GRID_SIZE; i++) {
    IN[i] = i;
  }
  umem<uint32_t> OUT(GRID_SIZE, init_const<>(0xFFFFFFFF));

  if (verbosity > 0) {
    std::cout <<
      "-------------------------------------------------------\n"
      "running micro\n";
  }

  test_insts<<<GRID_SIZE/BLOCK_SIZE,BLOCK_SIZE>>>(OUT, IN, 1, 2);

  auto e = cudaDeviceSynchronize();
  if (verbosity > 0) {
    std::cout << "cudaDeviceSynchronize: returned " <<
      cudaGetErrorString(e) << "\n";
  }
  if (e != cudaSuccess) {
    std::cerr << "  unexpected error code " <<
      cudaGetErrorName(e) << " (" << cudaGetErrorString(e) << ")\n";
  } else {
      OUT.str(std::cout);
  }
  return e != cudaSuccess ? EXIT_SUCCESS : EXIT_FAILURE;
}
