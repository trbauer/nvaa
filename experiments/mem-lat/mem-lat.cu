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


static __device__ uint4 &operator ^=(uint4 &lhs, const uint4 &rhs) {
  lhs.x ^= rhs.x;
  lhs.y ^= rhs.y;
  lhs.z ^= rhs.z;
  lhs.w ^= rhs.w;
  return lhs;
}
static __device__ uint4 &operator +=(uint4 &lhs, const uint4 &rhs) {
  lhs.x += rhs.x;
  lhs.y += rhs.y;
  lhs.z += rhs.z;
  lhs.w += rhs.w;
  return lhs;
}

static const int WALKS = 128;

// loads 'bytes' bytes into the cache and then time the flush (a fence)
__global__ void load_latency_l1(
          int64_t *times,
          uint32_t *oups,
    const uint32_t *inps,
          uint32_t zero)
{
  const size_t tid = blockIdx.x * blockDim.x + threadIdx.x;

  const uint32_t *ptr = inps + tid;
  // prefetch this 128B and force the dependency
  if (*ptr > 0) {
    ptr++;
    // assert(0 && "whoops, *ptr should be zero!");
  }
  const auto st = clock64();
  for (int i = 0; i < WALKS; i++) {
    // if (threadIdx.x < 1)
    // printf("tid%02d: offset: %p\n", threadIdx.x, ptr - inps);
    auto val = *ptr;
    ptr += val; // should be zero
    // if (threadIdx.x == 0) printf("\n");
  }
  const auto ed = clock64();
  //
  if (threadIdx.x == 0) {
    times[blockIdx.x] = (ed - st) / WALKS;
  }
  oups[tid] = *ptr;
}

static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << " " <<
        colr("Latency(c)", 16) << "\n";
}

static void print_col(std::ostream &os, std::string key, int64_t val) {
  os << coll(key, 16) << " " <<
        colr(val, 16) << "\n";
}

static void launch_latency_test(const opts &os, const char *test_name)
{
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  const size_t NBS = 1, TPB = 32; // need 32 to fill the memory faster

  size_t inp_elems = NBS * TPB;
  // dmem<uint32_t> inps {inp_elems, const_seq<uint32_t>(0u)};
  // dmem<uint32_t> oups {inp_elems, const_seq<uint32_t>(0u)};
  // dmem<int64_t> times {2, const_seq<int64_t>(0)};
  umem<uint32_t> inps {inp_elems, const_seq<uint32_t>(0u)};
  umem<uint32_t> oups {inp_elems, const_seq<uint32_t>(0u)};
  umem<int64_t> times {2, const_seq<int64_t>(0)};

  if (os.debug()) {
    vss << "INPS:\n";
    inps.str(vss, 8);
  }

  int64_t min_c = std::numeric_limits<int64_t>::max();
  for (int i = 0; i < os.iterations; i++) {
    load_latency_l1<<<NBS,TPB>>>(times, oups, inps, 0);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
        " (", cudaGetErrorString(e), "): unexpected error");
    }

    min_c = std::min(times[0], min_c);
    if (os.verbose()) {
      print_col(vss, format(test_name,".run[", i ,"]"), times[0]);
    }
    /*
    times.read([&](const int64_t *times) {
      min_c = std::min(times[0], min_c);
      if (os.verbose()) {
        print_col(vss, format(test_name,".run[", i ,"]"), times[0]);
      }
    });
    */
  } // for

  print_col(std::cout, test_name, min_c);

  if (os.debug()) {
    vss << "OUPS:\n";
    oups.str(vss, 8);
    vss << "TIMES:\n";
    times.str(vss, 8);
  }
  std::cout << vss.str();
}


// using handler = std::function<void (const opts &os)>;
using host_launcher = void (*)(const opts &, const char *nm);
using test = std::tuple<const char *,host_launcher>;

static const test ALL_TESTS[] {
  {"l1",  launch_latency_test},
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
        "usage: flush-cost [OPTS] TESTS\n"
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
        " % flush-cost all -f=test.csr\n"
        "  tests all algorithms on test.csr\n"
        " % flush-cost naive -f=test.csr\n"
        "  tests naive algorithm on test.csr\n"
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
        for (const auto [nm, fn] : ALL_TESTS) {
          tests.emplace_back(nm, fn);
        }
      } else {
        bool found = false;
        for (const auto [nm, fn] : ALL_TESTS) {
          if (nm == arg) {
            found = true;
            tests.emplace_back(nm, fn);
            break;
          }
        }
        if (!found) {
          fatal("invalid test name (try -h)");
        }
      }
    }
  } // for
  if (tests.empty()) {
    fatal("expected at least one test (use -h)");
  }

  print_col_headers(std::cout);
  for (const auto [nm,fn] : tests) {
    fn(os, nm);
  }

  return EXIT_SUCCESS;
} // main
