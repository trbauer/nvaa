#include "mincu.hpp"
#include "stats.hpp"
#include "timers.cuh"

#include <cuda_runtime_api.h>

#include <algorithm>
#include <array>
#include <cctype>
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

struct opts {
  int verbosity = 0;

  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
};

extern "C" static __device__ int get_tid()
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  return id;
}

///////////////////////////////////////////////////////////////////////////////
// similar test, but pairs the samples (so we can see the clock counter on long 1024 ns cases)
extern "C" __global__ void glob_shuffle_uidx(
  uint32_t *oups, int idx, int cluster_width)
{
  auto val = get_tid();
  // T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
  auto rval = __shfl_sync(0xFFFFFFFF, val, idx, cluster_width);
  oups[get_tid()] = rval;
}

static void test_shuffle_uidx(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 1;

  // umem<uint32_t> inps(TOTAL_BLOCKS * 32, init_seq<uint32_t>(0, 1));
  umem<uint32_t> oups(TOTAL_BLOCKS * 32);

  auto test = [&](int idx, int w) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_shuffle_uidx<<<TOTAL_BLOCKS,32>>>(oups, idx, w);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== __shfl_sync(" << idx << "," << w << ") ===============\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 32; i++) {
      std::cout << '[' << colr<uint32_t>(i,2,'0') << ']';
    }
    std::cout << "\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 32; i++) {
      std::cout << '[' << colr<uint32_t>(oups[i],2,'0') << ']';
    }
    std::cout << "\n";
  };
  test(3, 32);
  test(3, 16);
  test(3, 4);
  test(-3, 32);
}

///////////////////////////////////////////////////////////////////////////////
extern "C" __global__ void glob_shuffle_vidx(
  uint32_t *oups, const uint32_t *inps, int cluster_width)
{
  auto val = get_tid();
  auto idx = inps[threadIdx.x];
  // T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
  auto rval = __shfl_sync(0xFFFFFFFF, val, idx, cluster_width);
  oups[get_tid()] = rval;
}

static void test_shuffle_vidx(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 1;

  umem<uint32_t> idxs(TOTAL_BLOCKS * 32, init_seq<uint32_t>(1));
  idxs[0] = 16;
  idxs[1] = 4;
  idxs[2] = 4;
  idxs[3] = 3;
  umem<uint32_t> oups(TOTAL_BLOCKS * 32);

  auto test = [&](int w) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_shuffle_vidx<<<TOTAL_BLOCKS,32>>>(oups, idxs, w);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== __shfl_sync(var, " << w << ") ===============\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 32; i++) {
      std::cout << '[' << colr<uint32_t>(i,2,'0') << ']';
    }
    std::cout << "\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 32; i++) {
      std::cout << '[' << colr<uint32_t>(oups[i],2,'0') << ']';
    }
    std::cout << "\n";
  };
  test(32);
}

///////////////////////////////////////////////////////////////////////////////
extern "C" __global__ void glob_shuffle_vidx_perf(
  uint32_t *oups,
  uint64_t *oups_time,
  const uint32_t *inps,
  int cluster_width)
{
  auto val = get_tid();
  const auto idx = inps[get_tid()];
  auto st = clock64();
  // T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
  for (int i = 0; i < 128; i++) {
    val += __shfl_sync(0xFFFFFFFF, val + idx, idx, cluster_width);
  }
  auto en = clock64();
  oups[get_tid()] = val;
  if (threadIdx.x == 0) {
    oups_time[blockIdx.x] = en - st;
  }
}

static void emitIndices(const umem<uint32_t> &idxs) {
  for (uint32_t i = 0; i < idxs.size(); i++) {
    std::cout << '[' << colr<uint32_t>(i, 2, '0') << ']';
  }
  std::cout << "\n";
  for (uint32_t i = 0; i < idxs.size(); i++) {
    std::cout << '[' << colr<uint32_t>(idxs[i], 2, '0') << ']';
  }
  std::cout << "\n";
}

static void test_shuffle_vidx_perf(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 300;

//  idxs[0] = 16;
//  idxs[1] = 4;
//  idxs[2] = 4;
//  idxs[3] = 3;

  umem<uint64_t> oup_times(TOTAL_BLOCKS);
  umem<uint32_t> oups(TOTAL_BLOCKS * 32);

  auto test = [&](const char *which, const umem<uint32_t> &idxs) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_shuffle_vidx_perf<<<TOTAL_BLOCKS,32>>>(oups, oup_times, idxs, 32);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== test_shuffle_vidx_perf<" << which << "> ===============\n";
    if (os.verbose()) {
      std::cout << "===\n";
      emitIndices(idxs);
      if (os.debug()) {
        std::cout << "===\n";
        emitIndices(oups);
      }
    }

    const auto s = statistics::construct(oup_times.to_vector());
    std::cout << s.str();
    if (os.verbose())
      format_histogram(std::cout, oup_times.to_vector());
  }; // test

  umem<uint32_t> inpsID(32, init_seq<uint32_t>(0));
  umem<uint32_t> inpsK0(32, init_const<uint32_t>(0));

  umem<uint32_t> inpsREV(32);
  for (uint32_t i = 0; i < 32; i++) {
      inpsREV[i] = (32 - i);
  }

  // point to
  umem<uint32_t> inps0(32);
  for (uint32_t i = 0; i < 32; i++) {
    if (i % 2) {
      inps0[i] = (i + 1) % 32;
    } else {
      inps0[i] = (i + 2) % 32;
    }
  }

  random_state rs(12007);
  umem<uint32_t> inpsRND(32, init_random(rs, 0, 31));
  emitIndices(inpsRND);

  test("inps0", inps0);
  test("inps0", inps0);

  test("inpsK0", inpsK0);
  test("inpsK0", inpsK0);

  test("inpsID", inpsID);
  test("inpsID", inpsID);

  test("inpsREV", inpsREV);
  test("inpsREV", inpsREV);

  test("inpsRND", inpsRND);
  test("inpsRND", inpsRND);
}


///////////////////////////////////////////////////////////////////////////////

int main(int argc, const char* argv[])
{
  opts os;
  // test_timer_latency(0, block_counts);
  if ((argc != 2 && argc != 3) || (argc == 1 &&
    (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")))
  {
    std::cerr <<
      "usage: shuffles TESTNAME\n"
      "where TESTNAME =\n"
      "    uidx\n"
      "    vidx\n"
      "    vidx-perf\n"
      ;
    return EXIT_FAILURE;
  }
  std::string test;
  for (int ai = 1; ai < argc; ai++) {
    std::string arg = argv[ai], key = argv[ai], val;
    auto eqIdx = arg.find('=');
    if (eqIdx != std::string::npos) {
      key = arg.substr(0, eqIdx + 1);
      val = arg.substr(eqIdx + 1);
    }
    auto badArg =
      [&](std::string msg) {
        fatal(arg, ": ", msg, "\n");
      };
    auto parseSInt =
      [&](std::string s) {
        size_t off = 0;
        int64_t val = 0;
        int64_t sign = 1;
        if (off < s.size() && s[off] == '-') {
          sign = -1;
          off++;
        }
        if (s.substr(0, 2) == "0x" || s.substr(0, 2) == "0X") {
          off += 2;
          int64_t val = 0;
          if (off >= s.size()) {
            badArg("malformed integer");
          }
          while (off < s.size()) {
            if (s[off] >= '0' && s[off] <= '9') {
              val = 0x10 * val + s[off] - '0';
            } else if (s[off] >= 'a' && s[off] <= 'f') {
              val = 0x10 * val + s[off] - 'a';
            } else if (s[off] >= 'A' && s[off] <= 'F') {
              val = 0x10 * val + s[off] - 'A';
            } else {
              badArg("malformed integer");
            }
            off++;
          }
        } else {
          if (off >= s.size()) {
            badArg("malformed integer");
          }
          while (off < s.size()) {
            if (s[off] >= '0' && s[off] <= '9') {
              val = 10 * val + s[off] - '0';
            } else {
              badArg("malformed integer");
            }
          }
        }
        return sign * val;
      }; // parseInt

    if (key == "-v") {
      os.verbosity = 1;
    } else if (key == "-v=") {
      os.verbosity = parseSInt(val);
    } else if (arg.substr(0, 1) == "-") {
      badArg("invalid option");
    } else if (test.empty()) {
      test = arg;
    } else if (!test.empty()) {
      badArg("test already specified");
    }
  } // for

  if (test.empty()) {
    fatal("expected test name");
  }

  std::string test_name = argv[1];
  if (test_name == "uidx") {
    test_shuffle_uidx(os);
  } else if (test_name == "vidx") {
    test_shuffle_vidx(os);
  } else if (test_name == "vidx-perf") {
    test_shuffle_vidx_perf(os);
  } else {
    fatal(test_name, ": unsupported test");
  }

  return EXIT_SUCCESS;
}


// also