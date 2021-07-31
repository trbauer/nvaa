#include "mincu.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#define UNROLL_LEN 128

const int DEFAULT_BLOCKS = 2;
const int DEFAULT_THREADS_PER_BLOCK = 64;

extern "C" __global__ void kernel(
          uint32_t *dsts,
    const uint32_t *src0s,
          uint32_t src1,
          int n)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;

  uint32_t sum = 0;
  for (int i = 0; i < UNROLL_LEN; i++) {
    const auto off = std::min(id + i, n);
    sum += src0s[off] * src1;
  }

  dsts[id] = sum;
}



static bool parseInt(std::string s, uint32_t &val)
{
  try {
    char *end = nullptr;
    int radix = 10;
    if (s.size() >= 2 && (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))) {
      radix = 16;
      s = s.substr(2);
    }
    val = std::strtoul(s.c_str(), &end, radix);
    if (*end != 0) {
      return false;
    }
  } catch (...) {
      return false;
  }
  return true;
}

static std::string fmtHex(uint32_t val, int w = 8)
{
  std::stringstream ss;
  ss << "0x" << std::uppercase << std::hex << std::setw(w) << std::setfill('0') << val;
  return ss.str();
}


int main(int argc, char **argv)
{
  static const uint32_t INIT_DST = 0xAAAAAAAA, INIT_SRC0 = 0x1, INIT_SRC1 = 0x1;

  int verbosity = 0;
  uint32_t init_dst_val = INIT_DST, init_src0s_val = INIT_SRC0, src1 = INIT_SRC1;
  int blocks = DEFAULT_BLOCKS;
  int threads_per_block = DEFAULT_THREADS_PER_BLOCK;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    auto badArg = [&](std::string msg) {
      std::cerr << arg << ": " << msg << "\n";
      exit(EXIT_FAILURE);
    };
    auto eq_off = arg.find("=");
    std::string key = arg;
    std::string val;
    if (eq_off != std::string::npos) {
      key = arg.substr(0, eq_off + 1);
      val = arg.substr(eq_off + 1);
    }

    auto parseIntVal = [&]() {
      uint32_t ival;
      if (!parseInt(val, ival)) {
        badArg("malformed integer value");
      }
      return ival;
    };

    if (arg == "-h" || arg == "-help") {
      std::cout <<
        "usage: run OPTS\n"
        "where OPTS are:\n"
        "  -q/-v/-v2                       sets verbosity\n"
        "\n"
        "  -ds=/--dsts=VAL                 sets the initial buffer value (defaults to " << fmtHex(INIT_DST) << ")\n"
        "  -s0s=/--src0s=VAL               sets the initial buffer value (defaults to " << fmtHex(INIT_SRC0) << ")\n"
        "  -s1=/--src1=VAL                 sets the initial buffer value (defaults to " << fmtHex(INIT_SRC1) << ")\n"
        "\n"
        "  -bs=/--blocks=VAL               sets the initial buffer value (defaults to " << DEFAULT_BLOCKS << ")\n"
        "  -tpb=/--threads-per-block=VAL   sets the initial buffer value (defaults to " << DEFAULT_THREADS_PER_BLOCK << ")\n"
        "EXAMPLES:\n"
        "  % run -src0s=100 -src1=0x3 -bs=1\n"
        "     set src0s initially set to 100 and pass src1 as 3 with one block\n"
        "\n";
      exit(EXIT_SUCCESS);
    } else if (key == "-s0s=" || key == "--src0s=") {
      init_src0s_val = parseIntVal();
    } else if (key == "-s1=" || key == "--src1=") {
      src1 = parseIntVal();
    } else if (key == "-ds=" || key == "-dsts=") {
      init_dst_val = parseIntVal();
    } else if (key == "-bs=" || key == "--blocks=") {
      blocks = parseIntVal();
    } else if (key == "-tpb=" || key == "--threads-per-block=") {
      threads_per_block = parseIntVal();
    //
    //
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (arg == "-q") {
      verbosity = -1;
    } else {
      badArg("invalid option");
    }
  } //

  if (verbosity >= 2) {
    std::cout << " allocating surfaces\n";
  }
  int n = blocks * threads_per_block;
  mincu::umem<uint32_t> src0s((size_t)n, mincu::init_const(init_src0s_val));
  mincu::umem<uint32_t> dsts((size_t)n, mincu::init_const(init_dst_val));

  if (verbosity >= 2) {
    std::cout << " dispatching\n";
  }
  kernel<<<blocks,threads_per_block>>>(dsts, src0s, src1, n);
  if (verbosity >= 2) {
    std::cout << " synchronizing\n";
  }
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    // https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__TYPES.html#group__CUDART__TYPES_1g3f51e3575c2178246db0a94a430e0038
    std::cerr << "kernel returned error " << (int)e << "\n";
    exit(EXIT_FAILURE);
  }
  if (verbosity >= 2) {
    std::cout << " emitting buffers\n\n";
  }

  std::cout << "DSTS:\n";
  std::cout << fmtHex(0,4) << ":";
  int col = 0;
  for (int i = 0; i < n; i++) {
    std::cout << " " << fmtHex(dsts[i]);
    col++;
    if (i == n - 1) {
      std::cout << "\n";
    } else if (col == 8) {
      std::cout << "\n";
      std::cout << fmtHex(i * sizeof(dsts[i]), 4) << ":";
      col = 0;
    }
  }

  return EXIT_SUCCESS;
}


