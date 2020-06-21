#include "mincu.hpp"

#include <cuda_runtime_api.h>

#include <cctype>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>

static const uint32_t GRID_SIZE = 64;
static const uint32_t BLOCK_SIZE = 32;

// static_cast(GRID_SIZE%BLOCK_SIZE == 0, "block size must divide grid size");

using namespace mincu;


extern "C"
__global__ void test_insts(
  uint32_t *OUT,
  const uint32_t *IN,
  uint32_t arg1)
{
  const int id = blockDim.x*blockIdx.x + threadIdx.x;
  int sum = 0;
  for (int i = 0; i < 32; i++) {
    sum += IN[(id + i)%32] + arg1;
  }
  OUT[id] = sum;
}



int main(int argc, const char* argv[])
{
  const int32_t DFT_SRC1 = 1;
  int32_t src1 = DFT_SRC1;
  int verbosity = 0;

  for (int ai = 1; ai < argc; ai++) {
    std::string arg(argv[ai]);
    auto eq_ix = arg.find('=');
    std::string key = eq_ix != std::string::npos ?
      arg.substr(0,eq_ix+1) : arg;
    std::string val = eq_ix != std::string::npos ?
      arg.substr(eq_ix+1) : "";

    auto badArg = [&](const char *msg) {
      fatal(arg, ": ", msg);
    };
    auto parseInt = [&](){
      int radix = 10;
      bool negate = false;
      const char *cval = val.c_str();
      if (cval[0] == '-') {
        cval++;
        negate = true;
      }
      if (cval[0] == '0' &&
         (cval[1] == 'x' || cval[1] == 'X') &&
         isxdigit(cval[2]))
      {
        radix = 16;
        cval += 2;
      }
      long ival = 0;
      try {
        char *end = nullptr;
        ival = std::strtol(cval, &end, radix);
        if (*end) {
          badArg("malformed integer");
        }
      } catch (...) {
        badArg("malformed integer");
      }
      if (negate)
        ival = -ival;
      return (int)ival;
    };


    if (arg == "-h" || arg == "--help") {
      std::cout <<
        "usage: itester OPTS\n"
        "where OPTS are:\n"
        "  -v            verbose output\n"
        "  -v=INT        sets verbosity\n"
        "  -src1=INT     sets src1 (default: " << DFT_SRC1 << ")\n"
        "";
      return EXIT_SUCCESS;
    } else if (key == "-src1=") {
      src1 = parseInt();
    } else if (key == "-v" || key == "--verbose") {
      verbosity = 1;
    } else if (key == "-v=") {
      verbosity = parseInt();
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

  test_insts<<<GRID_SIZE/BLOCK_SIZE,BLOCK_SIZE>>>(OUT, IN, src1);

  auto e = cudaDeviceSynchronize();
  if (verbosity > 0) {
    std::cout << "cudaDeviceSynchronize: returned " <<
      cudaGetErrorString(e) << "\n";
  }
  if (e != cudaSuccess) {
    std::cerr << "  unexpected error code " <<
      cudaGetErrorName(e) << " (" << cudaGetErrorString(e) << ")\n";
    exit(EXIT_FAILURE);
  } else if (verbosity >= 0) {
      OUT.str(std::cout);
  }

  return EXIT_SUCCESS;
}
