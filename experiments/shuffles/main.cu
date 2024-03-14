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

// maps dataLayout => <impl,double>
using stats = std::map<std::string,std::map<std::string,double>>;

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

#define SHUFFLE_ITRS 4096

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

  umem<uint32_t> idxs(TOTAL_BLOCKS * 32, arith_seq<uint32_t>(1));
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
extern "C" __global__ void glob_shuffle_vidx_reg_perf(
  uint32_t *oups,
  uint64_t *oups_time,
  const uint32_t *inps,
  int cluster_width)
{
  uint32_t idx = inps[get_tid()];
  auto st = clock64();
  // T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
  for (int i = 0; i < SHUFFLE_ITRS; i++) {
    idx = __shfl_sync(0xFFFFFFFF, idx, idx, cluster_width);
  }
  auto en = clock64();
  oups[get_tid()] = idx;
  if (threadIdx.x == 0) {
    oups_time[blockIdx.x] = en - st;
  }
}

extern "C" __global__ void glob_shuffle_vidx_slm_perf(
  uint32_t *oups,
  uint64_t *oups_time,
  const uint32_t *inps,
  int cluster_width)
{
  __shared__ uint32_t slm_val[32];
  slm_val[threadIdx.x] = get_tid();
  __syncthreads();
  auto idx = inps[get_tid()];

  auto st = clock64();
  for (int i = 0; i < SHUFFLE_ITRS; i++) {
    idx = slm_val[idx];
  }
  auto en = clock64();
  oups[get_tid()] = idx;
  if (threadIdx.x == 0) {
    oups_time[blockIdx.x] = en - st;
  }
}

extern "C" __global__ void glob_shuffle_vidx_glb_perf(
  uint32_t *oups,
  uint64_t *oups_time,
  const uint32_t *inps,
  int cluster_width,
  uint32_t *glb_buf)
{
  glb_buf[get_tid()] = get_tid();
  auto idx = inps[get_tid()];
  auto st = clock64();
  for (int i = 0; i < SHUFFLE_ITRS; i++) {
    idx = glb_buf[idx];
  }
  auto en = clock64();
  oups[get_tid()] = idx;
  if (threadIdx.x == 0) {
    oups_time[blockIdx.x] = en - st;
  }
}

extern "C" __global__ void glob_shuffle_vidx_imadw_perf(
  uint32_t *oups,
  uint64_t *oups_time,
  const uint32_t *inps,
  int cluster_width)
{
  uint64_t idx64 = inps[get_tid()];
  auto st = clock64();
  for (uint32_t i = 0; i < SHUFFLE_ITRS; i++) {
    idx64 += 3 * (uint32_t)idx64;
  }
  auto en = clock64();
  oups[get_tid()] = (uint32_t)(idx64 >> 32);
  if (threadIdx.x == 0) {
    oups_time[blockIdx.x] = en - st;
  }
}


/////////////////////////////////////////////////////////////////////////////////////////
static void emitIndices(const umem<uint32_t> &idxs) {
  for (uint32_t i = 0; i < idxs.size(); i++) {
    std::cout << '|' << colr<uint32_t>(i, 2, '0') << '|';
  }
  std::cout << "\n";
  for (uint32_t i = 0; i < idxs.size(); i++) {
    std::cout << '[' << colr<uint32_t>(idxs[i], 2, '0') << ']';
  }
  std::cout << "\n";
}
enum class Alg {REG, SLM, GLB, IMAW};
static const std::array<Alg,4> ALL_ALGS {Alg::REG, Alg::SLM, Alg::GLB, Alg::IMAW};
static std::string ToKey(Alg a)
{
  switch (a) {
  case Alg::REG: return "reg";
  case Alg::SLM: return "slm";
  case Alg::GLB: return "glb";
  case Alg::IMAW: return "imaw";
  default: fatal("bad enum");
  }
  return "?";
}


static void test_shuffle_vidx_xxx_perf(
  const opts &os,
  stats &sts,
  const Alg alg)
{

  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
//  const uint32_t TOTAL_BLOCKS = 300;
  const uint32_t TOTAL_BLOCKS = 1;

  umem<uint64_t> oup_times(TOTAL_BLOCKS);
  umem<uint32_t> oups(TOTAL_BLOCKS * 32);
  umem<uint32_t> glb(32);

  auto testRun =
    [&](const char *which_idxs, const umem<uint32_t> &idxs) {
      // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
      auto which_hw = ToKey(alg);
      if (os.verbose()) {
        std::cout << which_idxs << "\n";
        emitIndices(idxs);
      }
      if (os.debug()) {
        for (size_t i = 0; i < 32; i++) {
          if (idxs[i] > 31) {
            fatal(which_idxs, ": ", which_hw, ": has oob index");
          }
        }
      }

      if (alg == Alg::REG) {
        glob_shuffle_vidx_reg_perf<<<TOTAL_BLOCKS,32>>>(oups, oup_times, idxs, 32);
      } else if (alg == Alg::SLM) {
        glob_shuffle_vidx_slm_perf<<<TOTAL_BLOCKS,32>>>(oups, oup_times, idxs, 32);
      } else if (alg == Alg::GLB) {
        glob_shuffle_vidx_glb_perf<<<TOTAL_BLOCKS,32>>>(oups, oup_times, idxs, 32, glb);
      } else if (alg == Alg::IMAW) {
        glob_shuffle_vidx_imadw_perf<<<TOTAL_BLOCKS,32>>>(oups, oup_times, idxs, 32);
      } else {
        fatal("bad enum");
      }
      auto e0 = cudaDeviceSynchronize();
      if (e0 != cudaSuccess) {
        fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
      }
      std::cout << "=============== test_shuffle_vidx_" << which_hw << "_perf(" << which_idxs << ") ===============\n";
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
      double val = s.min() / (double)SHUFFLE_ITRS;
      std::cout << val << " c per (best measured).\n";
      return val;
    }; // testRun

  auto testCase =
    [&](const char *which_idxs, const umem<uint32_t> &idxs) -> void {
      testRun(which_idxs, idxs); // warm up
      auto r = testRun(which_idxs, idxs); // real run
      sts[which_idxs][ToKey(alg)] = r;
    };

  //////////////////////////////////////////////////////////////////////
  umem<uint32_t> inpsID(32, arith_seq<uint32_t>(0));
  umem<uint32_t> inpsIDM2(32, arith_seq<uint32_t>(0, 1, 2));
  umem<uint32_t> inpsIDM4(32, arith_seq<uint32_t>(0, 1, 4));
  umem<uint32_t> inpsNXT32(32, arith_seq<uint32_t>(0, 1, 32));
  umem<uint32_t> inpsNXT4(32, arith_seq<uint32_t>(0, 1, 4));
  umem<uint32_t> inpsK5(32, const_seq<uint32_t>(25));
  umem<uint32_t> inpsREV(32, arith_seq<uint32_t>(31, (uint32_t)-1));


  random_state rs(12007);
  umem<uint32_t> inpsRND(32, rnd_seq<uint32_t>(rs, 0, 31));
  //////////////////////////////////////////////////////////////////////
  testCase("id",  inpsID);
  testCase("idm2", inpsIDM2);
  testCase("idm4", inpsIDM4);
  testCase("const5", inpsK5);
  testCase("nxt", inpsNXT32);
  testCase("nxtm4", inpsNXT4);
  testCase("rev", inpsREV);
  testCase("rnd", inpsRND);
}

static void test_shuffle_vidx_reg_perf(const opts &os, stats &sts)
{
  test_shuffle_vidx_xxx_perf(os, sts, Alg::REG);
}
static void test_shuffle_vidx_slm_perf(const opts &os, stats &sts)
{
  test_shuffle_vidx_xxx_perf(os, sts, Alg::SLM);
}
static void test_shuffle_vidx_glb_perf(const opts &os, stats &sts)
{
  test_shuffle_vidx_xxx_perf(os, sts, Alg::GLB);
}
static void test_shuffle_vidx_imadw_perf(const opts &os, stats &sts)
{
  test_shuffle_vidx_xxx_perf(os, sts, Alg::GLB);
}
static void test_shuffle_vidx_perf(const opts &os, stats &sts)
{
  for (auto alg : ALL_ALGS) {
    test_shuffle_vidx_xxx_perf(os, sts, alg);
  }
}

///////////////////////////////////////////////////////////////////////////////

int main(int argc, const char* argv[])
{
  opts os;
  std::vector<std::string> tests;

  // test_timer_latency(0, block_counts);
  if ((argc != 2 && argc != 3) || (argc == 1 &&
    (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")))
  {
    std::cerr <<
      "usage: shuffles TESTNAME\n"
      "where TESTNAME =\n"
      "    uidx\n"
      "    vidx\n"
      "    vidx-all-perf (runs all cases)\n"
      "      vidx-reg-perf\n"
      "      vidx-slm-perf\n"
      "      vidx-glb-perf\n"
      "      vidx-imadw-perf\n"
      ;
    return EXIT_FAILURE;
  }
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
    } else if (key == "-v2") {
      os.verbosity = 2;
    } else if (key == "-v=") {
      os.verbosity = parseSInt(val);
      std::cout << "verb: " << os.verbosity << "\n";
    } else if (arg.substr(0, 1) == "-") {
      badArg("invalid option");
    } else {
      tests.push_back(arg);
    }
  } // for

  if (tests.empty()) {
    fatal("expected test name");
  }

  // test -> <test,val>

  stats sts;
  for (auto test : tests) {
    if (test == "uidx") {
      test_shuffle_uidx(os);
    } else if (test == "vidx") {
      test_shuffle_vidx(os);
    } else if (test == "vidx-all-perf") {
      test_shuffle_vidx_perf(os, sts);
    } else if (test == "vidx-reg-perf") {
      test_shuffle_vidx_reg_perf(os, sts);
    } else if (test == "vidx-slm-perf") {
      test_shuffle_vidx_slm_perf(os, sts);
    } else if (test == "vidx-glb-perf") {
      test_shuffle_vidx_glb_perf(os, sts);
    } else if (test == "vidx-imadw-perf") {
      test_shuffle_vidx_imadw_perf(os, sts);
    } else {
      fatal(test, ": unsupported test");
    }
  } // tests

  std::stringstream csv;
  csv << "pattern";
  for (const auto &a : ALL_ALGS) {
    csv << "," << ToKey(a);
  }
  csv << ",loopitrs:" << SHUFFLE_ITRS;
  csv << "\n";

  for (const auto &me : sts) {
    csv << me.first;
    for (auto alg : ALL_ALGS) {
      auto key = ToKey(alg);
      csv << ",";
      auto itr = me.second.find(key);
      if (itr == me.second.end()) {
        csv << "?";
      } else {
        csv << itr->second;
      }
    }
    csv << "\n";
  }
  std::ofstream out("shuffles.csv");
  out << csv.str();
  out.flush();

  return EXIT_SUCCESS;
}


// also