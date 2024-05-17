#include "mincu.hpp"


#include <iostream>
#include <cstdio>
#include <functional>
#include <sstream>
#include <vector>

using namespace mincu;

MINCU_ENABLE_COLOR_IO_VIA_STATIC_CONSTRUCTOR();

struct opts {
  int verbosity = 0;
  int iterations = 2;
  bool check = false;
//  size_t blocks_per_grid = 1024;
//  size_t threads_per_block = 256;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

constexpr opts DFT_OPTS;


// loads 'bytes' bytes into the cache and then time the flush (a fence)
__global__ void wmma_load_f32(
          float *oups,
    const float *inps)
{
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#warp-level-matrix-instructions-wmma-ld

  const size_t tid0 = blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;

  float xs[8];
  // Load elements from f32 column-major matrix C and scale the values:
  asm("wmma.load.c.sync.aligned.m16n16k16.col.f32\n"
    "                 {%0,%1,%2,%3,%4,%5,%6,%7}, [%8], 0;\n"
    : "=f"(xs[0]),"=f"(xs[1]),"=f"(xs[2]),"=f"(xs[3])
    , "=f"(xs[4]),"=f"(xs[5]),"=f"(xs[6]),"=f"(xs[7])
    : "l"(inps + tid)
    );

  for (int i = 0; i < 8; i++)
    oups[8 * tid + i] = xs[i];
}


static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << "\n";
}

static void print_col(std::ostream &os, std::string key) {
  os << coll(key, 16) << "\n";
}

static void launch_wmma_load_f32(const opts &os, const std::string &test_name)
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
    wmma_load_f32<<<NBS,TPB>>>(oups, inps);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
        " (", cudaGetErrorString(e), "): unexpected error");
    }
  }
  std::cout << "OUPS:\n";
  oups.str(std::cout, 8, 12, 1);
}


// using handler = std::function<void (const opts &os)>;
using host_launcher = void (*)(const opts &, const std::string &);
using test = std::tuple<std::string,host_launcher>;

static const test ALL_TESTS[] {
  {"f32", launch_wmma_load_f32},
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
        "  --check               referee the output on CPU\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       // runs a large set of sizes\n"
        "";
      for (const auto [nm,_] : ALL_TESTS) {
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
        for (const auto [tnm,fn] : ALL_TESTS) {
          tests.emplace_back(tnm, fn);
        }
      } else {
        for (const auto [tnm,fn] : ALL_TESTS) {
          tests.emplace_back(tnm, fn);
        }
      }
    }
  } // for
  if (tests.empty()) {
    fatal("expected at least one test (use -h)");
  }

  print_col_headers(std::cout);
  for (const auto [tnm,fn] : tests) {
    fn(os, tnm);
  }

  return EXIT_SUCCESS;
} // main
