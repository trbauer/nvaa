#include "mincu.hpp"

#include <cuda_runtime_api.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <thread>
#include <tuple>
#include <vector>


using namespace mincu;

static int verbosity = 0;


__global__ void fatomics_kernel(float2 * dst, float2 * src0s)
{
  // auto gid = ( item.get_group(0) * item.get_local_range(0) + item.get_local_id(0) ) * per_wi;
  auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  float2 src = src0s[gid];
  float2 ret;
//  asm(
//    "atom.global.v4.f32.add  {%f0, %f1, %f2, %f3}, [gbl], {%f0, %f1, %f2, %f3};"
//    : dst.s0
//    );
  asm volatile ("atom.global.v2.f32.add  {%0, %1}, [%2], {%3, %4};"
    : "=f"(ret.x), "=f"(ret.y) : "l"(dst + gid), "f"(src.x), "f"(src.y));
}

static void run_fatomics()
{
  static const size_t NBLOCKS = 1;
  static const size_t BUFFER_ELEMS = 32;

//  umem<float2> oup(BUFFER_ELEMS, init_const(make_float2(1.0f, 1.0f)));
//  umem<float2> inp(BUFFER_ELEMS, init_const(make_float2(-2.0f, 2.0f)));
  umem<float2> oup(BUFFER_ELEMS);
  umem<float2> inp(BUFFER_ELEMS);
  for (size_t i = 0; i < BUFFER_ELEMS; i++) {
    oup[i] = make_float2( 1.0f, 1.0f);
    inp[i] = make_float2(-1.0f, 1.0f); // -1.0f and 1.0f
  }

  fatomics_kernel<<<NBLOCKS,BUFFER_ELEMS>>>(oup, inp);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  if (verbosity >= 0) {
    size_t off = 0x0;
    const size_t ELEMS_PER_ROW = 1;
    for (size_t i = 0; i < oup.size(); i += ELEMS_PER_ROW, off += ELEMS_PER_ROW * sizeof(oup[0])) {
      std::cout << "[" << hex(off, 5) << "]: ";
      for (size_t c = 0; c < ELEMS_PER_ROW && i + c < oup.size(); c++) {
        std::cout << "  " << coll<std::string>(format(frac(oup[i + c])), 32);
      }
      std::cout << "\n";
    }
  }
}

int main(int argc, const char* argv[])
{
  std::string test_name;
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-h" || arg == "--help") {
      std::cerr <<
        "usage: fatomics.exe TESTNAME+\n"
        "where TESTNAME =\n"
        "  {coalesced,scalar}{,4}{,_noalias}\n"
        "  e.g. coalesced4_noalias\n"
        " --- or ---\n"
        "  all         to run all tests\n";
      return EXIT_FAILURE;
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (arg == "fatomics") {
      run_fatomics();
    } else {
      fatal(arg, ": inavlid argument");
    }
  }

  return EXIT_SUCCESS;
}
