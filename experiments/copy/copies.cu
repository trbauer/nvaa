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

// C.f. https://developer.nvidia.com/blog/cuda-pro-tip-increase-performance-with-vectorized-memory-access/

using namespace mincu;

static bool check = false;
static int verbosity = 0;
static bool failed = false;

///////////////////////////////////////////////////////////////////////////////
static const int PER_WI = 32;
static const int NBLOCKS = 32 * 64 * 30; // saturate an RTX 2060 (30 SMs)
static const int BLOCKSIZE = 32;

///////////////////////////////////////////////////////////////////////////////
template <typename T>
__global__ void copy_coalesced_kernel(T *  dst, const T * src)
{
  // const auto inc = item.get_local_range(0);
  // auto gid = item.get_group(0) * item.get_local_range(0) * per_wi + item.get_local_id(0);
  auto gid = blockIdx.x * blockDim.x * PER_WI + threadIdx.x;

  #pragma unroll
  for (size_t i = 0; i < PER_WI; i++) {
    dst[gid] = src[gid];
    gid += blockDim.x;
  }
}
template <typename T>
__global__ void copy_scalar_kernel(T *  dst, const T * src)
{
  // auto gid = ( item.get_group(0) * item.get_local_range(0) + item.get_local_id(0) ) * per_wi;
  auto gid = (blockIdx.x * blockDim.x + threadIdx.x) * PER_WI;

  #pragma unroll
  for (size_t i = 0; i < PER_WI; i++) {
    dst[gid] = src[gid];
    gid++;
  }
}
template <typename T>
__global__ void copy_coalesced_noalias_kernel(T * __restrict__  dst, const T * __restrict__ src)
{
  // const auto inc = item.get_local_range(0);
  // auto gid = item.get_group(0) * item.get_local_range(0) * per_wi + item.get_local_id(0);
  auto gid = blockIdx.x * blockDim.x * PER_WI + threadIdx.x;

  #pragma unroll
  for (size_t i = 0; i < PER_WI; i++) {
    dst[gid] = src[gid];
    gid += blockDim.x;
  }
}
template <typename T>
__global__ void copy_scalar_noalias_kernel(T * __restrict__  dst, const T * __restrict__ src)
{
  // auto gid = ( item.get_group(0) * item.get_local_range(0) + item.get_local_id(0) ) * per_wi;
  auto gid = (blockIdx.x * blockDim.x + threadIdx.x) * PER_WI;

  #pragma unroll
  for (size_t i = 0; i < PER_WI; i++) {
    dst[gid] = src[gid];
    gid++;
  }
}

///////////////////////////////////////////////////////////////////////////////
bool operator !=(int4 a, int4 b) {
  return memcmp(&a, &b, sizeof(a)) != 0;
}

// return GB/s
template <typename T>
static double test_copy(
  const char *which,
  std::function<void(T*,T*)> dispatch)
{
  auto BUFFER_ELEMS = PER_WI * NBLOCKS * BLOCKSIZE;

  umem<T> oup(BUFFER_ELEMS);
//  umem<T> inp(BUFFER_ELEMS, init_seq(0, 1));
  umem<T> inp(BUFFER_ELEMS);
  for (size_t i = 0; i < BUFFER_ELEMS; i++) {
    for (int k = 0; k < sizeof(inp[0]) / sizeof(int32_t); k++) {
      ((int32_t *)&inp[i])[k] = i + k;
    }
  }

  // warm up
  dispatch(oup, inp);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  if (check) {
    for (size_t i = 0; i < oup.size(); i++) {
      if (oup[i] != inp[i]) {
        fatal(which, ": mismatch in elem [", i, "] unexpected error");
        failed = true;
      }
    }
  }

  auto st = std::chrono::steady_clock::now();
  dispatch(oup, inp);
  e = cudaDeviceSynchronize();
  auto ed = std::chrono::steady_clock::now();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  // return std::chrono::duration_cast<std::chrono::microseconds>(ed - st).count();
  uint64_t total_bytes_read = inp.size() * sizeof(inp[0]);
  double elapsed = std::chrono::duration<double>(ed - st).count();
  if (verbosity >= 1)
    std::cout << "elapsed: " << elapsed << "\n";

  return (total_bytes_read / elapsed) / 1024.0 / 1024.0 / 1024.0;
}


///////////////////////////////////////////////////////////////////////////////
static double test_copy_coalesced()
{
  return test_copy<int32_t>(
      "coalesced",
      [&](int32_t *dst, int32_t *src) {
        copy_coalesced_kernel<int32_t><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_coalesced4()
{
  return test_copy<int4>(
      "coalesced4",
      [&](int4 *dst, int4 *src) {
        copy_coalesced_kernel<int4><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_scalar()
{
  return test_copy<int32_t>(
      "scalar",
      [&](int32_t *dst, int32_t *src) {
        copy_scalar_kernel<int32_t><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_scalar4()
{
  return test_copy<int4>(
      "scalar4",
      [&](int4 *dst, int4 *src) {
        copy_scalar_kernel<int4><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}

static double test_copy_coalesced_noalias()
{
  return test_copy<int32_t>(
      "coalesced noalias",
      [&](int32_t *dst, int32_t *src) {
        copy_coalesced_noalias_kernel<int32_t><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_coalesced4_noalias()
{
  return test_copy<int4>(
      "coalesced4 noalias",
      [&](int4 *dst, int4 *src) {
        copy_coalesced_noalias_kernel<int4><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_scalar_noalias()
{
  return test_copy<int32_t>(
      "scalar noalias",
      [&](int32_t *dst, int32_t *src) {
        copy_scalar_noalias_kernel<int32_t><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}
static double test_copy_scalar4_noalias()
{
  return test_copy<int4>(
      "scalar4 noalias",
      [&](int4 *dst, int4 *src) {
        copy_scalar_noalias_kernel<int4><<<NBLOCKS,BLOCKSIZE>>>(dst, src);
      });
}

///////////////////////////////////////////////////////////////////////////////
static void test_stack_emit_header()
{
  std::cout <<
    std::setw(24) << "algorithm" <<
    " " <<
    std::setw(24) << "throughput (GB/s)" <<
    "\n";
}
static void test_stack_emit_row(const char *alg, double tpt)
{
  std::stringstream ss;
  ss <<
    std::setw(24) << alg <<
    " " <<
    std::setw(24) << std::fixed << std::setprecision(3) << tpt <<
    "\n";
  std::cout << ss.str();
}


// cudaDeviceSetLimit
// https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__DEVICE.html#group__CUDART__DEVICE_1g05956f16eaa47ef3a4efee84563ccb7d

int main(int argc, const char* argv[])
{
  bool run_coalesced = false, run_coalesced4 = false,
       run_scalar = false, run_scalar4 = false;
  bool run_coalesced_noalias = false, run_coalesced4_noalias = false,
       run_scalar_noalias = false, run_scalar4_noalias = false;
  std::string test_name;
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-h" || arg == "--help") {
      std::cerr <<
        "usage: copies.exe [-v|-v2|--check] TESTNAME+\n"
        "where TESTNAME =\n"
        "  {coalesced,scalar}{,4}{,_noalias}\n"
        "  e.g. coalesced4_noalias\n"
        " --- or ---\n"
        "  all         to run all tests\n";
      return EXIT_FAILURE;
    } else if (arg == "-c" || arg == "--check") {
      check = true;
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (arg == "all") {
      run_coalesced = true;
      run_coalesced4 = true;
      run_scalar = true;
      run_scalar4 = true;
      run_coalesced_noalias = true;
      run_coalesced4_noalias = true;
      run_scalar_noalias = true;
      run_scalar4_noalias = true;
    } else if (arg == "coalesced") {
      run_coalesced = true;
    } else if (arg == "coalesced4") {
      run_coalesced4 = true;
    } else if (arg == "scalar") {
      run_scalar = true;
    } else if (arg == "scalar4") {
      run_scalar4 = true;
    } else if (arg == "coalesced_noalias") {
      run_coalesced_noalias = true;
    } else if (arg == "coalesced4_noalias") {
      run_coalesced4_noalias = true;
    } else if (arg == "scalar_noalias") {
      run_scalar_noalias = true;
    } else if (arg == "scalar4_noalias") {
      run_scalar4_noalias = true;
    } else {
      fatal(arg, ": inavlid argument");
    }
  }
  if (!run_coalesced && !run_scalar && !run_scalar4) {
    fatal("expected test name (use -h)");
  }

  test_stack_emit_header();
  //
  if (run_coalesced) {
    test_stack_emit_row("coalesced", test_copy_coalesced());
  }
  if (run_coalesced4) {
    test_stack_emit_row("coalesced4", test_copy_coalesced4());
  }
  if (run_scalar) {
    test_stack_emit_row("scalar", test_copy_scalar());
  }
  if (run_scalar4) {
    test_stack_emit_row("scalar4", test_copy_scalar4());
  }
  //
  if (run_coalesced_noalias) {
    test_stack_emit_row("coalesced noalias", test_copy_coalesced_noalias());
  }
  if (run_coalesced4_noalias) {
    test_stack_emit_row("coalesced4 noalias", test_copy_coalesced4_noalias());
  }
  if (run_scalar_noalias) {
    test_stack_emit_row("scalar noalias", test_copy_scalar_noalias());
  }
  if (run_scalar4_noalias) {
    test_stack_emit_row("scalar4 noalias", test_copy_scalar4_noalias());
  }
  //
  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}


// also