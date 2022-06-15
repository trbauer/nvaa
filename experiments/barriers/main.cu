#include "mincu.hpp"

#include <cooperative_groups.h>
// https://nvidia.github.io/libcudacxx/extended_api/synchronization_primitives/barrier/init.html
#include <cuda/barrier>
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
static __device__ int next_lid(int id) {
  return id == blockIdx.x - 1 ? 0 : id;
}

///////////////////////////////////////////////////////////////////////////////
extern "C" __global__ void glob_test_syncthreads(
  uint32_t *oups, int idx, int cluster_width)
{
  __shared__ int grp_mem[64];
  auto tid = get_tid();
  grp_mem[threadIdx.x] = tid;
  // T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
  __syncthreads();
//  if (tid % 2) {
//    __syncwarp(tid);
//  } else {
//    __syncwarp(tid);
//  }
  oups[get_tid()] = grp_mem[next_lid(threadIdx.x)];
}

static void test_syncthreads(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 1;

  // umem<uint32_t> inps(TOTAL_BLOCKS * 32, init_seq<uint32_t>(0, 1));
  umem<uint32_t> oups(TOTAL_BLOCKS * 64);

  auto test = [&](int idx, int w) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_test_syncthreads<<<TOTAL_BLOCKS,64>>>(oups, idx, w);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== glob_simpler_barrier2(" << idx << "," << w << ") ===============\n";
    std::cout << "\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 64; i++) {
      std::cout << '[' << i << "] = " << oups[i] << "\n";
    }
    std::cout << "\n";
  };
  test(3, 32);
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
extern "C" __global__ void glob_test_syncthreads2(
  uint32_t *oups, int idx, int itrs)
{
  auto block = cooperative_groups::this_thread_block();

  __shared__ int grp_mem[64];
  auto tid = get_tid();
  grp_mem[threadIdx.x] = tid;
  block.sync();
  int id = threadIdx.x;
  for (int i = 0; i < itrs; i++) {
    id = next_lid(id);
    block.sync();
    grp_mem[threadIdx.x] = grp_mem[id] + 1;
  }
  oups[get_tid()] = grp_mem[next_lid(id)];
}

static void test_syncthreads2(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 1;

  // umem<uint32_t> inps(TOTAL_BLOCKS * 32, init_seq<uint32_t>(0, 1));
  umem<uint32_t> oups(TOTAL_BLOCKS * 64);

  auto test = [&](int idx, int w) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_test_syncthreads<<<TOTAL_BLOCKS,64>>>(oups, idx, w);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== glob_simpler_barrier(" << idx << "," << w << ") ===============\n";
    std::cout << "\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 64; i++) {
      std::cout << '[' << i << "] = " << oups[i] << "\n";
    }
    std::cout << "\n";
  };
  test(3, 32);
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
__device__ void compute(float *data, int i)
{
  if (i % 2)
    *data *= 2.0f + (float)i;
  else
    *data += 2.0f * (float)i;
}
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#temporal_split_5_stage_sync
__global__ void glob_split(uint32_t *oups, int idx, int itrs)
{
  using barrier = cuda::barrier<cuda::thread_scope_block>;
  __shared__  barrier bar0, bar1;
  __shared__ float sums[64];
  auto block = cooperative_groups::this_thread_block();

  sums[threadIdx.x] = (float)threadIdx.x;
  const int next_id = threadIdx.x == blockDim.x - 1 ? 0 : threadIdx.x + 1;

  // Initialize the barrier with expected arrival count
  int n = 32; // block.size() / 2;
  if (block.thread_rank() == 0) {
    init(&bar0, n); // even channels
    init(&bar1, n);
  }
  block.sync();
  sums[threadIdx.x] += sums[next_id];

//  for (int i = 0; i < itrs; ++i) {
   // this thread arrives. Arrival does not block a thread
   int i = 10;
   barrier::arrival_token token =
     threadIdx.x % 2 == 0 ? bar0.arrive() : bar1.arrive();
   compute(sums + next_id, threadIdx.x);
   // wait for all threads participating in the barrier to complete bar.arrive()
   if (threadIdx.x % 2 == 0) {
     bar0.wait(std::move(token));
   } else {
     bar1.wait(std::move(token));
   }
//  }
  oups[get_tid()] = (int)sums[threadIdx.x];
}

static void test_split(const opts &os)
{
  // launch 1 warp per block and one block per SM so that there's not
  // likely to be contention over SMs
  const uint32_t TOTAL_BLOCKS = 1;

  // umem<uint32_t> inps(TOTAL_BLOCKS * 32, init_seq<uint32_t>(0, 1));
  umem<uint32_t> oups(TOTAL_BLOCKS * 64);

  auto test = [&](int idx, int w) {
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
    glob_split<<<TOTAL_BLOCKS,64>>>(oups, idx, w);
    auto e0 = cudaDeviceSynchronize();
    if (e0 != cudaSuccess) {
      fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
    }
    std::cout << "=============== glob_simpler_barrier(" << idx << "," << w << ") ===============\n";
    std::cout << "\n";
    for (uint32_t i = 0; i < TOTAL_BLOCKS * 64; i++) {
      std::cout << '[' << i << "] = " << oups[i] << "\n";
    }
    std::cout << "\n";
  };
  test(3, 32);
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
      "    __syncthreads\n"

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

  for (auto test : tests) {
    if (test == "__syncthreads" || test == "syncthreads") {
      test_syncthreads(os);
    } else if (test == "__syncthreads2" || test == "syncthreads2") {
      test_syncthreads2(os);
    } else if (test == "test_split") {
      test_split(os);
    } else {
      fatal(test, ": unsupported test");
    }
  } // tests

  return EXIT_SUCCESS;
}

