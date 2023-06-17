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

static bool check = false;
static int verbosity = 0;
static bool failed = false;

extern "C" static __device__ int get_tid()
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  return id;
}
// at least 64 times for 512k buffer
static const int LOADS_PER_THREAD = 64 * (512*1024 / 4);

///////////////////////////////////////////////////////
// latency

template <size_t B>
__global__ void test_stack_lat_device(int32_t *oup, uint64_t *times)
{
  static const size_t N = B / 4;
  static_assert(B % 4 == 0, "B must be multiple of 4");

  const auto tid = get_tid();
  int32_t stack[N];

  for (int i = 0; i < N; i++) {
    stack[i] = (threadIdx.x + i) % N;
  }
  int idx = threadIdx.x;
  auto st = clock64();
  for (int i = 0; i < LOADS_PER_THREAD; i++) {
    idx = stack[idx];
  }
  auto en = clock64();

  oup[tid] = idx;
  times[get_tid()] = en - st;
}

template <size_t B>
static uint64_t test_stack_lat()
{
//  std::cout << "running test_stack_lat<" << B << ">()\n";
  static_assert(B % 4 == 0, "B must be multiple of 4");

  // https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__TYPES.html#group__CUDART__TYPES_1g4c4b34c054d383b0e9a63ab0ffc93651
  size_t limit = 0;
  auto e = cudaThreadGetLimit(&limit, cudaLimitStackSize);
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e),
          " (", cudaGetErrorString(e), "): unexpected error in cudaThreadGetLimit");
  }
//  std::cout << "cudaLimitStackSize: " << limit << "\n";

  umem<int32_t> mem(1);
  umem<uint64_t> times(1);

  // warm up
  test_stack_lat_device<B><<<1,1>>>(mem, times);
  e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  test_stack_lat_device<B><<<1,1>>>(mem, times);
  e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  return times[0];
}

///////////////////////////////////////////////////////
// throughput

// PROBLEMS:
//   - setup cost affects results
template <size_t B>
__global__ void test_stack_tpt_device(int32_t *oup, int zero)
{
  static const size_t N = B / 4;
  static_assert(B % 4 == 0, "B must be multiple of 4");

  //   sum prv(N) = tid * N + N/2
  int32_t prv[N];

  // alternating sum (negative evens):
  //   -0 + 1 -2 + 3 ... -(N-2)+(N+1)
  //     factor out tid (which is tid * N)
  //     group pairs off even/odd which produce 1 each
  //       (-0 + 1) + (-2 + 3) + ...
  //     so sequence of N is (N/2 + tid * N)
  //     let altSum2 t n = t * n + n`div`2 [ FOR EVEN N ]
  //   test with:
  //     let altSum t n = [if even i then t - i else t + i | i<-[0..n-1]]
  for (int i = 0; i < N; i += 2) {
    prv[i + 0] = threadIdx.x - (i + 0);
    prv[i + 1] = threadIdx.x + (i + 1);
  }
  int32_t sum = 0;
  int var_zero = min(zero, threadIdx.x); // force non-uniform index
  for (int i = 0; i < LOADS_PER_THREAD; i++) {
    sum += prv[(var_zero + i) % N];
  }

  oup[get_tid()] = sum;
}

// https://versus.com/en/nvidia-geforce-rtx-2060-vs-nvidia-geforce-rtx-2060-super/max-mem-bandwidth
// 336 gb/s (192b memory bus)
// 1920 shading units (30 SM)

// return GB/s
template <size_t B>
static double test_stack_tpt()
{
  static const auto N = B / 4;
  static_assert(B % 4 == 0, "B must be multiple of 4");
  static_assert(LOADS_PER_THREAD >= N, "B/4 must be greater than");

  static const auto NBLOCKS = 10*30; // 10 full waves
  static const auto BLOCKSIZE = 32;
  umem<int32_t> oup(NBLOCKS * BLOCKSIZE);

  // warm up
  test_stack_tpt_device<B><<<NBLOCKS,BLOCKSIZE>>>(oup, 0);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
  auto st = std::chrono::steady_clock::now();
  test_stack_tpt_device<B><<<NBLOCKS,BLOCKSIZE>>>(oup, 0);
  e = cudaDeviceSynchronize();
  auto ed = std::chrono::steady_clock::now();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  // return std::chrono::duration_cast<std::chrono::microseconds>(ed - st).count();
  uint64_t total_bytes_read = BLOCKSIZE * NBLOCKS *
    ((uint64_t)4 * LOADS_PER_THREAD); // doesn't count setup cost
  double elapsed = std::chrono::duration<double>(ed - st).count();
  // double elapsed = std::chrono::duration_cast<std::chrono::seconds>(ed - st).count();
  if (verbosity >= 1)
    std::cout << "elapsed: " << elapsed << "\n";

  if (check) {
    for (size_t i = 0 ; i < NBLOCKS * BLOCKSIZE; i++) {
      uint32_t oup_i = oup[i];
      uint32_t tid = i % 32u;

      // let altSum2 t n = t * n + n`div`2 [ FOR EVEN N ]
      auto alt_sum = [tid](uint32_t n) {return tid * n + n / 2;};
      //
      uint32_t full_prv_sums = (LOADS_PER_THREAD / N);
      uint32_t leftovers = LOADS_PER_THREAD % N;
      auto oup_expected = full_prv_sums * alt_sum(N) + alt_sum(leftovers);
      if (oup_i != oup_expected) {
        std::cerr << "test_stack_tpt<" << B << ">: mismatch on oup[" << i << "]\n"
          "expected " << oup_expected << "; got " << oup_i << "\n";
        failed = true;
        break;
      }
    }
  }

  return (total_bytes_read / elapsed) / 1024.0 / 1024.0 / 1024.0;
}

static void test_stack_emit_header()
{
  std::cout <<
    std::setw(24) << "stack_size (B/lane)" <<
    " " <<
    std::setw(24) << "latency (c/read)"
    " " <<
    std::setw(24) << "throughput (GB/s)" <<
    "\n";
}
static void test_stack_emit_row(size_t B, uint64_t lat, double tpt)
{
  std::stringstream ss;
  ss <<
    std::setw(24) << B <<
    " " <<
    std::setw(24) << std::fixed << std::setprecision(1) << (lat / (double)LOADS_PER_THREAD) <<
    " " <<
    std::setw(24) << std::fixed << std::setprecision(3) << tpt <<
    "\n";
  std::cout << ss.str();
}

using lat_func = uint64_t (*)();
using tpt_func = double (*)();
using test_case = std::tuple<uint64_t,lat_func,tpt_func>;


template <size_t B>
static test_case mk_test_case() {
  return std::make_tuple(B, test_stack_lat<B>, test_stack_tpt<B>);
}
static test_case test_cases[] {
  mk_test_case<16>(),
  mk_test_case<32>(),
  mk_test_case<48>(),
  mk_test_case<64>(),
  mk_test_case<96>(),
  mk_test_case<128>(),
  mk_test_case<256>(),
  mk_test_case<512>(),
  mk_test_case<1*1024>(),
  mk_test_case<2*1024>(),
  mk_test_case<4*1024>(),
  mk_test_case<8*1024>(),
  mk_test_case<12*1024>(),
  mk_test_case<16*1024>(),
  mk_test_case<32*1024>(),
  mk_test_case<48*1024>(),
  mk_test_case<64*1024>(),
  mk_test_case<96*1024>(),
  mk_test_case<128*1024>(),
  mk_test_case<160*1024>(),
  mk_test_case<192*1024>(),
  mk_test_case<224*1024>(),
  mk_test_case<256*1024>(),
  mk_test_case<384*1024>(),
  mk_test_case<480*1024>(),
  mk_test_case<512*1024>(),
};

static void test_stack_all()
{
  test_stack_emit_header();
  for (auto p : test_cases) {
    auto N = std::get<0>(p);
    auto lat_func = std::get<1>(p);
    auto lat = lat_func();
    auto tpt_func = std::get<2>(p);
    auto tpt = tpt_func();
    test_stack_emit_row(N, lat, tpt);
  }
}

// cudaDeviceSetLimit
// https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__DEVICE.html#group__CUDART__DEVICE_1g05956f16eaa47ef3a4efee84563ccb7d

int main(int argc, const char* argv[])
{
  std::stringstream ss_cs;
  for (auto lc : test_cases) {
    auto B = std::get<0>(lc);
    std::stringstream ss_sz;
    if (B % (1024*1024) == 0) {
      ss_sz << B/1024/1024 << "M";
    } else if (B % 1024 == 0) {
      ss_sz << B/1024 << "K";
    } else {
      ss_sz << B;
    }
    ss_cs << "  case" << ss_sz.str() << "\n";
  }

  std::string test_name;
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-h" || arg == "--help") {
      std::cerr <<
        "usage: stack.exe [-v|-v2] TESTNAME\n"
        "where TESTNAME =\n"
        "  all - runs all tests\n"
        "  -- or --\n" <<
        ss_cs.str();
      return EXIT_FAILURE;
    } else if (arg == "-c" || arg == "--check") {
      check = true;
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (!test_name.empty()) {
      fatal("test already specified");
    } else {
      test_name = arg;
    }
  }
  if (test_name.empty()) {
    fatal("expected test name (use -h)");
  }

  if (test_name == "all") {
    test_stack_all();
  } else if ((test_name.substr(0, 4) == "case") &&
             std::isdigit(test_name[4])) {
    size_t scale = 1;
    auto sz_str = test_name.substr(4);
    if (sz_str[sz_str.size() - 1] == 'K') {
      scale = 1024;
      sz_str = sz_str.substr(0, sz_str.size() - 1);
    } else if (sz_str[sz_str.size() - 1] == 'M') {
      scale = 1024*1024;
      sz_str = sz_str.substr(0, sz_str.size() - 1);
    }
    const size_t B = std::atoi(sz_str.c_str()) * scale;
    if (B == 0) {
      fatal(test_name, ": malformed test");
    }
    const test_case *t = nullptr;
    for (const auto &p : test_cases) {
      if (std::get<0>(p) == B) {
        t = &p;
        break;
      }
    }
    if (t == nullptr) {
      fatal(test_name, ": cannot find test");
    }

    test_stack_emit_header();
    auto lat = (std::get<1>(*t))();
    auto tpt = (std::get<2>(*t))();
    test_stack_emit_row(std::get<0>(*t), lat, tpt);
  } else {
    fatal(test_name, ": unsupported test");
  }

  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}


// also