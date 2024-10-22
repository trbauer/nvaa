#include "mincu.hpp"

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

#define INCLUDE_FFMA_LAT
#define INCLUDE_DFMA_LAT
// #define INCLUDE_CBUF_ARG_LAT
// #define INCLUDE_CBUF_DIR_LAT
// #define INCLUDE_CBUF_INDU_LAT
// #define INCLUDE_CBUF_INDV_LAT

/*
extern "C" static __device__ int get_tid()
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  return id;
}*/


static const int TOTAL_ACCESSES = 4096;

static const int KEY_W = 32, VAL_W = 16;

static void emitTableHdr(const char *test, const char *xLabel, const char *yLabel)
{
  std::stringstream ss;
  ss << test << "-" << xLabel;
  std::cout << std::setw(KEY_W) << std::right << ss.str() <<
    "    " << std::setw(VAL_W) << std::right << yLabel << "\n";
}
static int syncAndEmitTableRow(uint32_t x, umem<uint64_t> &runtimes)
{
  auto e0 = cudaDeviceSynchronize();
  if (e0 != cudaSuccess) {
    fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
  }
  auto rtClksSum = runtimes[0];
  double rtAvg = (double)rtClksSum / (double)TOTAL_ACCESSES;
  std::cout << std::setw(KEY_W) << std::right << x << "    ";
  if (rtClksSum == ~0ull) {
    std::cout << std::setw(VAL_W) << std::right << "error\n";
    return EXIT_FAILURE;
  } else {
    std::cout <<
      std::setw(VAL_W) << std::right << std::fixed << std::setprecision(2) <<
      rtAvg << "\n";
  }
  return EXIT_SUCCESS;
}



#ifdef INCLUDE_FFMA_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
extern "C" __global__
void ffma_lat(
  float *oups,
  uint64_t *out_time,
  float src0, float src1,
  int itrs)
{
  const int UNROLL_SIZE = 128;
  uint64_t rtSum = 0;
  float sum = (float)threadIdx.x; // non-zero
  src0 += 1.0f; // force into registers
  src1 += 1.0f; // force into registers
  //
  while (itrs > 0) {
    uint64_t rtStt = clock64();
    for (int i = 0; i < UNROLL_SIZE; i++) {
      sum = src0 + sum * src1;
    }
    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= UNROLL_SIZE;
  }
  if (threadIdx.x > 32) { // never true
    oups[0] = sum;
  }
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
} // ffma_lat

static int test_ffma_lat()
{
  int ret = EXIT_SUCCESS;

  umem<float> oups(1, const_seq<float>(0.0f));
  umem<uint64_t> runtimes((size_t)1, arith_seq<uint64_t>(0));

  emitTableHdr("ffma_lat", "run", "clks/ref");

  for (int i = 0; i <= 8; i++) {
    ffma_lat<<<1,1>>>(oups, runtimes, 0.5f, 1.0f, TOTAL_ACCESSES);
    // 64 unrolled arg iteration * 4B each means 256B touched
    //
    ret |= syncAndEmitTableRow(i, runtimes);
  } // for

  return ret;
}
#endif // INCLUDE_FFMA_LAT

#ifdef INCLUDE_DFMA_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
extern "C" __global__
void dfma_lat(
  double *oups,
  uint64_t *out_time,
  double src0, double src1,
  int itrs)
{
  const int UNROLL_SIZE = 128;
  uint64_t rtSum = 0;
  double sum = (float)threadIdx.x; // non-zero
  src0 += 1.0; // force into registers
  src1 += 1.0; // force into registers
  //
  while (itrs > 0) {
    uint64_t rtStt = clock64();
    for (int i = 0; i < UNROLL_SIZE; i++) {
      sum = src0 + sum * src1;
    }
    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= UNROLL_SIZE;
  }
  if (threadIdx.x > 32) { // never true
    oups[0] = sum;
  }
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
} // dfma_lat

static int test_dfma_lat()
{
  int ret = EXIT_SUCCESS;

  umem<double> oups(1, const_seq<double>(0.0));
  umem<uint64_t> runtimes(1, arith_seq<uint64_t>(0));

  emitTableHdr("dfma-lat", "run", "clks/ref");

  for (int i = 0; i <= 8; i++) {
    dfma_lat<<<1,1>>>(oups, runtimes, 0.5f, 1.0f, TOTAL_ACCESSES);
    //
    ret |= syncAndEmitTableRow(i, runtimes);
  } // for

  return ret;
}
#endif // INCLUDE_DFMA_LAT


#ifdef INCLUDE_CBUF_ARG_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
struct cargs {
  float vals[1024 - 8]; // 4k is max arg space size
};

#define CONST_ARG_UNROLL 64

template <int BUFFER_SIZE>
__device__ // __noinline__
uint64_t const_arg_latency_body(
  float *oups,
  uint64_t *out_time,
  struct cargs args,
  int itrs)
{
  // successive loads step this many floats
  uint64_t rtSum = 0;
  float sum = (float)threadIdx.x + args.vals[0]; // prefetch first
  sum += 1.0f; // serialize the result (we desire constant args preloaded)
  while (itrs > 0) {
    float fi = (float)itrs;

    // verify that we cover the buffer window at least once
    static_assert(
      CONST_ARG_UNROLL * 64 >= BUFFER_SIZE,
      "CONST_ARG_UNROLL is too small for buffer size");

    static const int CACHELINE_FLTS = 64 / 4;

    // Walk cachelines up to the buffer limit
    // Case 0. Buffer size is too large for unroll factor => STATIC ASSERT ERROR
    //
    // Otherwise we walk until we hit the end.
    //
    // If the buffer is
    uint64_t rtStt = clock64();
    for (int i = 0; i < CONST_ARG_UNROLL; i++) {
      sum += fi * args.vals[i * CACHELINE_FLTS % (BUFFER_SIZE / 4)];
    }
    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= CONST_ARG_UNROLL;
  }
  if (threadIdx.x > 32) { // never true
    oups[0] = sum;
  }
  return rtSum;
}


extern "C" __global__
void const_arg_latency(
  float *oups,
  uint64_t *out_time,
  struct cargs args,
  int buffer_size,
  int itrs)
{
  uint64_t rtSum = (uint32_t)(-1);
  switch (buffer_size) {
#define MK_CONST_ARG_CASE(I) \
  case (64*(I)): rtSum = const_arg_latency_body<64*(I)>(oups, out_time, args, itrs); break;
#define MK_CONST_ARG_CASE2(I) \
  MK_CONST_ARG_CASE((I) + 0) \
  MK_CONST_ARG_CASE((I) + 1)
#define MK_CONST_ARG_CASE4(I) \
  MK_CONST_ARG_CASE2((I) + 0) \
  MK_CONST_ARG_CASE2((I) + 2)
#define MK_CONST_ARG_CASE8(I) \
  MK_CONST_ARG_CASE4((I) + 0) \
  MK_CONST_ARG_CASE4((I) + 4)
#define MK_CONST_ARG_CASE16(I) \
  MK_CONST_ARG_CASE8((I) + 0) \
  MK_CONST_ARG_CASE8((I) + 8)
///////////////
  MK_CONST_ARG_CASE16(1)  // 1..16
  MK_CONST_ARG_CASE16(17) // 17..32
  MK_CONST_ARG_CASE16(33) // 33..48
  MK_CONST_ARG_CASE8(49) // 49..56 (3584)
  MK_CONST_ARG_CASE4(57) // 57..60 (3840)
  default:
    // nop -1 implies invalid
    break;
  }
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
} // const_arg_latency

static int test_const_arg_lat()
{
  int ret = EXIT_SUCCESS;
  struct cargs args;
  for (int i = 0; i < sizeof(args.vals)/sizeof(args.vals[0]); i++) {
    args.vals[i] = 1.0f / (float)i;
  }

  umem<float> oups(1, init_const<float>(0.0f));
  umem<uint64_t> runtimes(1, init_seq<uint32_t>(0));

  emitTableHdr("const_arg", "access size (B)", "clks/ref");

  for (int buf_size = 64; buf_size <= 3840; buf_size += 64) {
    const_arg_latency<<<1,1>>>(oups, runtimes, args, buf_size, TOTAL_ACCESSES);
    // 64 unrolled arg iteration * 4B each means 256B touched
    //
    ret |= syncAndEmitTableRow(buf_size, runtimes);
  } // for

  return ret;
}
#endif // INCLUDE_CBUF_ARG_LAT

#ifdef INCLUDE_CBUF_DIR_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
__constant__ float cbuf_dir_table[64*1024/4]; // 64k

#define CONST_BUF_DIR_UNROLL 128

template <int BUFFER_SIZE>
__device__ // __noinline__
uint64_t const_buf_dir_lat_size(
  float *oups,
  uint64_t *out_time,
  int itrs)
{
  // successive loads step this many floats
  uint64_t rtSum = 0;
  float sum = (float)threadIdx.x + cbuf_dir_table[0]; // prefetch first
  sum += 1.0f; // serialize the result (we desire constant args preloaded)
  while (itrs > 0) {
    float fi = (float)itrs;

    // verify that we cover the buffer window at least once
    static_assert(
      CONST_BUF_DIR_UNROLL * 64 >= BUFFER_SIZE,
      "CONST_BUF_DIR_UNROLL is too small for buffer size");

    static const int CACHELINE_FLTS = 64 / 4;

    // Walk cachelines up to the buffer limit
    // Case 0. Buffer size is too large for unroll factor => STATIC ASSERT ERROR
    //
    // Otherwise we walk until we hit the end.
    //
    // If the buffer is
    uint64_t rtStt = clock64();
    for (int i = 0; i < CONST_BUF_DIR_UNROLL; i++) {
      sum += fi * cbuf_dir_table[i * CACHELINE_FLTS % (BUFFER_SIZE / 4)];
    }
    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= CONST_BUF_DIR_UNROLL;
  }
  if (threadIdx.x > 32) { // never true
    oups[0] = sum;
  }
  return rtSum;
}

extern "C" __global__
void const_buf_dir_lat(
  float *oups,
  uint64_t *out_time,
  int buffer_size,
  int itrs)
{
  // stride
  //      0  touches [0] only 4B
  //      1  touches [0, 1, 2, ... 31] => 32*4 => 256 B
  //      2  touches [0, 2, 4, ... 63] => 32*2*4 => 256 B
  //      3  touches [0, 3, 6, ... 95] => 32*3*4 => 384 B
  //      4  touches [0, 4, ... 128] => 32*4*4 => 512 B
  //      ...
  //      s                             32*s*4
// Prelude> let f i = 32*k*4
//  Prelude> [f i | i<-[0..20]]
// [0,128,256,384,512,640,768,896,1024,1152,1280,1408,1536,1664,1792,1920,2048,2176,2304,2432,2560]
  // successive loads step this many floats
  uint64_t rtSum = (uint32_t)(-1);
  switch (buffer_size) {
#define MK_CONST_DIR_BUF_CASE(I) \
  case (64*(I)): rtSum = const_buf_dir_lat_size<64*(I)>(oups, out_time, itrs); break;
#define MK_CONST_DIR_BUF_CASE2(I) \
  MK_CONST_DIR_BUF_CASE((I) + 0) \
  MK_CONST_DIR_BUF_CASE((I) + 1)
#define MK_CONST_DIR_BUF_CASE4(I) \
  MK_CONST_DIR_BUF_CASE2((I) + 0) \
  MK_CONST_DIR_BUF_CASE2((I) + 2)
#define MK_CONST_DIR_BUF_CASE8(I) \
  MK_CONST_DIR_BUF_CASE4((I) + 0) \
  MK_CONST_DIR_BUF_CASE4((I) + 4)
#define MK_CONST_DIR_BUF_CASE16(I) \
  MK_CONST_DIR_BUF_CASE8((I) + 0) \
  MK_CONST_DIR_BUF_CASE8((I) + 8)
#define MK_CONST_DIR_BUF_CASE32(I) \
  MK_CONST_DIR_BUF_CASE16((I) + 0) \
  MK_CONST_DIR_BUF_CASE16((I) + 16)
///////////////
  MK_CONST_DIR_BUF_CASE32(1)  // 1..32
  MK_CONST_DIR_BUF_CASE32(33) // 33..64
  MK_CONST_DIR_BUF_CASE32(65) // 65..96 (6144)
  MK_CONST_DIR_BUF_CASE32(97) // 97..128 (8192)
  default:
    // nop -1 implies invalid
    break;
  }
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
} // const_dir_latency

static int test_const_buf_dir_lat()
{
  int ret = EXIT_SUCCESS;

  float h_table[64*1024/4];
  auto get_idx = [&](uint32_t i) {
    return 4.0f * ((i >= sizeof(h_table)/sizeof(h_table[0]) - 1) ? 0 : i + 1);
  };
  for (uint32_t i = 0; i < sizeof(h_table)/sizeof(h_table[0]); i++) {
    h_table[i] = get_idx(i);
  }
  cudaMemcpyToSymbol(cbuf_dir_table, h_table, sizeof(h_table));

  umem<float> oups(1, init_const<float>(0.0f));
  umem<uint64_t> runtimes(1, init_seq<uint32_t>(0));

  emitTableHdr("const_buf_dir", "access size (B)", "clks/ref");

  for (int buf_size = 64; buf_size <= 8192; buf_size += 64) {
    const_buf_dir_lat<<<1,1>>>(oups, runtimes, buf_size, TOTAL_ACCESSES);
    // 64 unrolled arg iteration * 4B each means 256B touched
    //
    ret |= syncAndEmitTableRow(buf_size, runtimes);
  } // for


  return EXIT_SUCCESS;
}

#endif // INCLUDE_CBUF_DIR_LAT


#ifdef INCLUDE_CBUF_INDU_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
__constant__ uint32_t cbuf_indu_table[64*1024/4];

///////////////////////////////////////////////////////////////////////////////
// similar test, but pairs the samples (so we can see the clock counter on long 1024 ns cases)
extern "C" __global__ void const_buf_indu_latency(
  uint32_t *oups,
  uint64_t *out_time,
  int itrs)
{
  const int UNROLL_SIZE = 32;
  uint32_t idx = cbuf_indu_table[0];
//  printf("%d\n", idx);

  uint64_t rtSum = 0;
  while (itrs > 0) {
    uint64_t rtStt = clock64();
    for (int i = 0; i < UNROLL_SIZE; i++) {
      asm volatile("ld.const.u32 %0, [%1];" : "=r"(idx) : "r"(idx));
    }
    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= UNROLL_SIZE;
  }
  oups[0] = idx;
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
}

static int test_const_buf_ind_lat()
{
  uint32_t h_table[64*1024/4];
  auto get_idx = [&](uint32_t i) {
    return 4 * ((i >= sizeof(h_table)/sizeof(h_table[0]) - 1) ? 0 : i + 1);
  };
  for (uint32_t i = 0; i < sizeof(h_table)/sizeof(h_table[0]); i++) {
    h_table[i] = get_idx(i);
  }

  umem<uint32_t> oups(1, init_seq<uint32_t>(0));
  umem<uint64_t> runtimes(1, init_seq<uint32_t>(0));

  emitTableHdr("const_buf_ind", "access size (B)", "clks/ref");

  for (int k = 32; k <= 64*1024/4; k += 32) {
    h_table[k - 1] = 0;
    cudaMemcpyToSymbol(cbuf_indu_table, h_table, sizeof(h_table));
    h_table[k - 1] = get_idx(k - 1); // restore

    const_buf_indu_latency<<<1,1>>>(oups, runtimes, TOTAL_ACCESSES);

    ret |= syncAndEmitTableRow(access_size, runtimes);
  } // for

  return EXIT_SUCCESS;
}
#endif // INCLUDE_CBUF_INDU_LAT



#ifdef INCLUDE_CBUF_INDV_LAT
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
__constant__ uint32_t cbuf_indv_table[64*1024/4];

///////////////////////////////////////////////////////////////////////////////
// similar test, but pairs the samples (so we can see the clock counter on long 1024 ns cases)
extern "C" __global__ void const_buf_indv_latency(
  uint32_t *oups,
  uint32_t *start_idxs,
  uint64_t *out_time,
  int access_size,
  int itrs)
{
  const int UNROLL_SIZE = 64;
  uint32_t idx = start_idxs[threadIdx.x];

// int k = 0;
  uint64_t rtSum = 0;
  while (itrs > 0) {
    uint64_t rtStt = clock64();
    for (int i = 0; i < UNROLL_SIZE; i++) {
//      int pv_idx = idx;
      asm volatile("ld.const.u32 %0, [%1];" : "=r"(idx) : "r"(idx));
//      if (access_size == 128*4 && k++ <= 1 && i <= 1) {
//        printf("D[L%d][%3d] %4d -> %4d\n", i, threadIdx.x, pv_idx, idx);
//      }
    }

    uint64_t rtEnd = clock64();
    rtSum += rtEnd - rtStt;
    itrs -= UNROLL_SIZE;
  }
  if (threadIdx.x == 0) {
    out_time[0] = rtSum;
  }
  oups[threadIdx.x] = idx;
}


static int test_const_buf_indv_lat_gen(
  bool scattered, std::function<uint32_t(uint32_t,uint32_t)> next_idx)
{
  int ret = EXIT_SUCCESS;
  uint32_t h_table[64*1024/4];
  memset(h_table, 0, sizeof(h_table));

  umem<uint32_t> oups(32, init_seq<uint32_t>(0));
  umem<uint32_t> start_idxs(32, init_seq<uint32_t>(0, 4));
  if (scattered) {
    for (int i = 0; i < 32; i++) {
      // 16*i => start on i'th cacheline
      // i%16 => stay on that cachine
      start_idxs[i] = 4*(16*i + (i % 16));
    }
  }
//  for (int i = 0; i < 32; i++) {
//    std::cout << "H[" << i << "] " << start_idxs[i] << "\n";
//  }
  umem<uint64_t> runtimes(1, init_seq<uint32_t>(0));

  emitTableHdr("const_buf_indv", "access size (B)", "clks/ref");

  for (int access_size = 512; access_size <= 64*1024; access_size += 128) {
    for (uint32_t i = 0; i < sizeof(h_table)/sizeof(h_table[0]); i++) {
      h_table[i] = next_idx(i, access_size / 4);
//      if (access_size == 512)
//        std::cout << "C[" << i << "]: " << h_table[i] << "\n";
    }
    cudaMemcpyToSymbol(cbuf_indv_table, h_table, sizeof(h_table));

    const_buf_indv_latency<<<1,32>>>(oups, start_idxs, runtimes, access_size, TOTAL_ACCESSES);

    ret |= syncAndEmitTableRow(access_size, runtimes);
  } // for

  return ret;
}

static uint32_t next_idx_packed(uint32_t i, uint32_t buf_elems)
{
  // 0 -> 32 -> 64 -> ...
  // 1 -> 33 -> 65 ...
  // ...
  // buf_elems - 32 -> 0
  // buf_elems - 31 -> 1
  // ...
  // buf_elems -  1 -> 31
  if (i >= buf_elems)
    return 4 * (i % 32);
  return 4 * ((i + 32) % buf_elems);
}
static int test_const_buf_indvp_lat()
{
  return test_const_buf_indv_lat_gen(false, next_idx_packed);
}
static int test_const_buf_indvs_lat()
{
  return test_const_buf_indv_lat_gen(true, next_idx_packed);
}

struct rands {
  uint32_t vals[64*1024/4];
  rands() {
    std::default_random_engine g;
    g.seed(12007);
    std::uniform_int_distribution<int> d(0, sizeof(vals)/sizeof(vals[0]) - 1);
    for (uint32_t &val : vals) {
      val = d(g);
    }
  }
};
static rands rs;

static int test_const_buf_indvr_lat()
{
  return test_const_buf_indv_lat_gen(false,
    [&](uint32_t i, uint32_t buf_elems) {
      if (i >= buf_elems)
        return 4 * (i % 32);
      return 4 * (rs.vals[i] % buf_elems);
    });
}
#endif // INCLUDE_CBUF_INDV_LAT

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
int main(int argc, const char* argv[])
{
  struct test {
    std::string label;
    int (*function)();
    test(const char *lbl, int (*func)())
      : label(lbl)
      , function(func)
    {
    }
    bool shouldRun(int argc, const char* argv[]) const {
      for (int i = 1; i < argc; i++) {
        if (argv[i] == label) {
          return true;
        }
      }
      return false;
    }
    int call() const {
      return (*function)();
    }
  }; // test

  const std::vector<test> TESTS {
#ifdef INCLUDE_FFMA_LAT
    {"ffma-lat", test_ffma_lat},
#endif // INCLUDE_FFMA_LAT
#ifdef INCLUDE_DFMA_LAT
    {"dfma-lat", test_dfma_lat},
#endif // INCLUDE_DFMA_LAT
#ifdef INCLUDE_CBUF_ARG_LAT
    {"cbuf-arg-lat", test_const_arg_lat},
#endif // INCLUDE_CBUF_ARG_LAT
#ifdef INCLUDE_CBUF_DIR_LAT
    {"cbuf-dir-lat", test_const_buf_dir_lat},
#endif // INCLUDE_CBUF_DIR_LAT
#ifdef INCLUDE_CBUF_INDU_LAT
    {"cbuf-indu-lat", test_const_buf_ind_lat},
#endif // INCLUDE_CBUF_INDU_LAT
#ifdef INCLUDE_CBUF_INDV_LAT
    {"cbuf-indvp-lat", test_const_buf_indvp_lat},
    {"cbuf-indvs-lat", test_const_buf_indvs_lat},
    {"cbuf-indvr-lat", test_const_buf_indvr_lat},
#endif // INCLUDE_CBUF_INDV_LAT
  };

  int ran = 0;
  int ret = EXIT_SUCCESS;

  for (const test &t : TESTS) {
    if (t.shouldRun(argc, argv)) {
      ret |= t.call();
      ran++;
    }
  }
  if (ran == 0) {
    std::cout <<
      "usage: consts TEST+\n"
      "where TESTS are\n";
    for (const test &t : TESTS) {
      std::cout << "  " << t.label << "\n";
    }
  }
  return ret;
}

