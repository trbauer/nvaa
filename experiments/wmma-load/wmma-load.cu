#include "mincu.hpp"

#include <cuda_fp16.h>

#include <iostream>
#include <cstdio>
#include <functional>
#include <sstream>
#include <vector>

using namespace mincu;

MINCU_ENABLE_COLOR_IO_VIA_STATIC_CONSTRUCTOR();

struct opts {
  int verbosity = 0;
  int iterations = 1;
//  bool check = false;
//  size_t blocks_per_grid = 1024;
//  size_t threads_per_block = 256;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

// constexpr opts DFT_OPTS;

  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#warp-level-matrix-instructions-wmma-ld
/*
extern "C"
__global__ void wmma_load_A_f16x2_row(
          __half * __restrict__ oups,
    const __half * __restrict__ inps)
{
  const size_t tid0 = blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;

  __half xs[8];
  asm("wmma.load.a.sync.aligned.m16n16k16.row.f16\n"
    "                 {%0,%1,%2,%3,%4,%5,%6,%7}, [%8];\n"
    : "=f"(xs[0]),"=f"(xs[1]),"=f"(xs[2]),"=f"(xs[3])
    , "=f"(xs[4]),"=f"(xs[5]),"=f"(xs[6]),"=f"(xs[7])
    : "l"(inps + tid)
    );

  for (int i = 0; i < 8; i++)
    oups[8 * tid + i] = xs[i];
}

extern "C"
__global__ void wmma_load_B_f32_col(
          float * __restrict__ oups,
    const float * __restrict__ inps)
{
  const size_t tid0 = blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;

  float xs[8];
  // Load elements from f32 column-major matrix C and scale the values:
  asm("wmma.load.b.sync.aligned.m16n16k16.col.f32\n"
    "                 {%0,%1,%2,%3,%4,%5,%6,%7}, [%8], 0;\n"
    : "=f"(xs[0]),"=f"(xs[1]),"=f"(xs[2]),"=f"(xs[3])
    , "=f"(xs[4]),"=f"(xs[5]),"=f"(xs[6]),"=f"(xs[7])
    : "l"(inps + tid)
    );

  for (int i = 0; i < 8; i++)
    oups[8 * tid + i] = xs[i];
}
*/


extern "C"
__global__ void wmma_load_C_f32_col(
          float * __restrict__ oups,
    const float * __restrict__ inps)
{
  const size_t tid0 = blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;

  float xs[8];
  const auto *inp_tid = inps + tid;

  // Load elements from f32 column-major matrix C and scale the values:
  asm("wmma.load.c.sync.aligned.m16n16k16.col.f32\n"
    "                 {%0,%1,%2,%3,%4,%5,%6,%7}, [%8];\n"
    : "=f"(xs[0]),"=f"(xs[1]),"=f"(xs[2]),"=f"(xs[3])
    , "=f"(xs[4]),"=f"(xs[5]),"=f"(xs[6]),"=f"(xs[7])
    : "l"(inp_tid)
    );

  for (int i = 0; i < 8; i++)
    oups[8 * tid + i] = xs[i];
}

///////////////////////////////////////////
extern "C"
__global__ void wmma_load_C_f32_row(
          float *oups,
    const float *inps)
{
  const size_t tid0 = blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;

  float xs[8];
  const auto *inp_tid = inps + tid;

  // Load elements from f32 column-major matrix C and scale the values:
  asm("wmma.load.c.sync.aligned.m16n16k16.row.f32\n"
    "                 {%0,%1,%2,%3,%4,%5,%6,%7}, [%8];\n"
    : "=f"(xs[0]),"=f"(xs[1]),"=f"(xs[2]),"=f"(xs[3])
    , "=f"(xs[4]),"=f"(xs[5]),"=f"(xs[6]),"=f"(xs[7])
    : "l"(inp_tid)
    );

  for (int i = 0; i < 8; i++)
    oups[8 * tid + i] = xs[i];
}

////////////////////////////////////////////////////////////////////////////////
static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << "\n";
}

static void print_col(std::ostream &os, std::string key) {
  os << coll(key, 16) << "\n";
}

enum class wmma_load_ord {
  c_f32_col,
  c_f32_row,
};

static void launch_wmma_load_f32(
    const opts &os, const std::string &test_name, wmma_load_ord ord)
{
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  const size_t NBS = 1, TPB = 32;

  dmem<float> inps {32 * NBS * TPB, arith_seq<float>(0.0f)};
  if (os.verbose()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 8, 12, 1);
  }

  dmem<float> oups {8 * NBS * TPB, const_seq<float>(0.0f)};

  for (int i = 0; i < os.iterations; i++) {
    switch (ord) {
    case wmma_load_ord::c_f32_col:
      wmma_load_C_f32_col<<<NBS,TPB>>>(oups, inps);
      break;
    case wmma_load_ord::c_f32_row:
      wmma_load_C_f32_row<<<NBS,TPB>>>(oups, inps);
      break;
    default: fatal("invalid wmma_load_ord");
    }
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
        " (", cudaGetErrorString(e), "): unexpected error");
    }
  }
  print_col(std::cout, test_name);
  std::cout << "OUPS:\n";
  oups.str(std::cout, 8, 12, 1);
}


using host_launcher = std::function<void (const opts &os,std::string,wmma_load_ord)>;
using test = std::tuple<std::string,wmma_load_ord,host_launcher>;

static const test ALL_TESTS[] {
  {"c.f32.col", wmma_load_ord::c_f32_col, launch_wmma_load_f32},
  {"c.f32.row", wmma_load_ord::c_f32_row, launch_wmma_load_f32},
};

int main(int argc, const char* argv[])
{
  struct opts os;
  std::vector<test> tests;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    std::string key = arg, val;
    auto eq = arg.find('=');
    if (eq != std::string::npos) {
      key = arg.substr(0, eq + 1); // include the =
      val = arg.substr(eq + 1);
    }
    auto bad_opt = [&](const char *msg) {
      fatal(arg, ": ", msg);
    };

    if (arg == "-h" || arg == "--help") {
      std::stringstream uss;
      uss <<
        "usage: wmma-load [OPTS] TESTS\n"
        "where [OPTS]:\n"
        "  -i/--iterations=INT   number of runs to take\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       // runs a large set of sizes\n"
        "";
      for (const auto [nm,_,__] : ALL_TESTS) {
        uss << "        | " << nm << "\n";
      }
      uss << "  where [NUMZEROS] is an integer number of zeros in the warp message (0..32)\n"
        "EXAMPLES:\n"
        " % wmma-load all\n"
        "";
      std::cout << uss.str();
      return EXIT_SUCCESS;
    } else if (key == "-i=") {
      os.iterations = parse_integral_positive<int>(val, false);
    } else if (arg == "-v") {
      os.verbosity = 1;
    } else if (arg == "-v2") {
      os.verbosity = 2;
    } else if (arg == "-v3") {
      os.verbosity = 3;
    } else if (arg.substr(0, 1) == "-") {
      bad_opt("invalid option");
    } else {
      if (arg == "all") {
        for (const auto t : ALL_TESTS) {
          tests.emplace_back(t);
        }
      } else {
        bool found = false;
        for (const auto [tnm,ord,fn] : ALL_TESTS) {
          if (arg == tnm) {
            tests.emplace_back(tnm, ord, fn);
            found = true;
            break;
          }
        }
        if (!found)
          fatal("unexpected test (-h to list tests)");
      }
    }
  } // for
  if (tests.empty()) {
    fatal("expected at least one test (use -h)");
  }

  print_col_headers(std::cout);
  for (const auto [tnm,ord,fn] : tests) {
    fn(os, tnm, ord);
  }

  return EXIT_SUCCESS;
} // main

/*
.global .align 32 .f16 A[256], B[256];
.global .align 32 .f32 C[256], D[256];
.reg .b32 a<8> b<8> c<8> d<8>;

wmma.load.a.sync.aligned.m16n16k16.global.row.f16
        {a0, a1, a2, a3, a4, a5, a6, a7}, [A];
wmma.load.b.sync.aligned.m16n16k16.global.col.f16
        {b0, b1, b2, b3, b4, b5, b6, b7}, [B];

wmma.load.c.sync.aligned.m16n16k16.global.row.f32
        {c0, c1, c2, c3, c4, c5, c6, c7}, [C];

wmma.mma.sync.aligned.m16n16k16.row.col.f32.f32
        {d0, d1, d2, d3, d4, d5, d6, d7},
        {a0, a1, a2, a3, a4, a5, a6, a7},
        {b0, b1, b2, b3, b4, b5, b6, b7},
        {c0, c1, c2, c3, c4, c5, c6, c7};

wmma.store.d.sync.aligned.m16n16k16.global.col.f32
        [D], {d0, d1, d2, d3, d4, d5, d6, d7};

// Compute an integer WMMA:
.reg .b32  a, b<4>;
.reg .b32 c<8>, d<8>;
wmma.mma.sync.aligned.m8n32k16.row.col.s32.s8.s8.s32
        {d0, d1, d2, d3, d4, d5, d6, d7},
        {a}, {b0, b1, b2,  b3},
        {c0, c1, c2, c3, c4, c5, c6, c7};

// Compute sub-byte WMMA:
.reg .b32 a, b, c<2> d<2>
wmma.mma.sync.aligned.m8n8k32.row.col.s32.s4.s4.s32
        {d0, d1}, {a}, {b}, {c0, c1};

// Compute single-bit type WMMA:
.reg .b32 a, b, c<2> d<2>
wmma.mma.xor.popc.sync.aligned.m8n8k128.row.col.s32.b1.b1.s32
        {d0, d1}, {a}, {b}, {c0, c1};

// Compute double precision wmma
.reg .f64 a, b, c<2>, d<2>;
wmma.mma.sync.aligned.m8n8k4.row.col.f64.f64.f64.f64
        {d0, d1}, {a}, {b}, {c0, c1};

// Compute alternate floating point precision wmma
.reg .b32 a<2>, b<2>, c<8>, d<8>;
wmma.mma.sync.aligned.m16n16k8.row.col.f32.tf32.tf32.f32
        {d0, d1, d2, d3, d4, d5, d6, d7},
        {a0, a1, a2, a3}, {b0, b1, b2, b3},
        {c0, c1, c2, c3, c4, c5, c6, c7};
*/