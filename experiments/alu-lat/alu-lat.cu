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

// number of blocks, threads per block
static const size_t NBS = 64, TPB = 32;

/*
static __device__ ushort2 &operator +=(ushort2 &lhs, const ushort2 &rhs) {
  lhs.x += rhs.x;
  lhs.y += rhs.y;
  return lhs;
}
static __device__ ushort2 operator *(const ushort2 &lhs, const ushort2 &rhs) {
  return make_ushort2(lhs.x * rhs.x, lhs.y * rhs.y);
}
static __device__ ushort2 operator +(const ushort2 &lhs, const ushort2 &rhs) {
  return make_ushort2(lhs.x + rhs.x, lhs.y + rhs.y);
}
*/

static __device__ int64_t get_globaltimer()
{
  int64_t t;
  asm volatile ("mov.u64 %0, %globaltimer;" : "=l"(t));
  return t;
}
//  int64_t st;
//  asm volatile ("mov.u64 %0, %clock64;" : "=l"(st) :: "memory");


static const uint32_t WALKS = 128;

template <typename T>
__global__ void mad_latency(int64_t *times, T *oups, T zero)
{
  T sum = T(threadIdx.x);
  if (threadIdx.x == 0)
    sum += zero;
  if (zero > T(0))
    sum += 1;
  for (int i = 0; i < int(zero) + 2; i++) {
    const auto st = clock64();
    const auto st_ns = get_globaltimer();
    for (uint32_t i = 0; i < WALKS; i++) {
      sum += sum * T(zero);
    }
    const auto ed = clock64();
    const auto ed_ns = get_globaltimer();
    const size_t tid = blockIdx.x * blockDim.x + threadIdx.x;
    oups[tid] = sum;
    if (threadIdx.x == 0) {
      times[4 * blockIdx.x + 0] = ed - st;
    } else if (threadIdx.x == 1) {
      times[4 * blockIdx.x + 1] = ed_ns - st_ns;
    } else if (threadIdx.x == 2) {
      times[4 * blockIdx.x + 2] = ed;
    } else if (threadIdx.x == 3) {
      times[4 * blockIdx.x + 3] = st;
    }
  }
}

static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << " " <<
        colr("Latency(c)", 16) << " " <<
        colr("Latency(ns)", 16) << "\n";
}

static void print_col(std::ostream &os, std::string tnm, int64_t c, int64_t ns) {
  os << coll(tnm, 16) << " " <<
        colr(c, 16) << " " <<
        colr(ns, 16) << "\n";
}


// using handler = std::function<void (const opts &os)>;
using device_launcher = std::function<void(int64_t *,void *)>;
using host_launcher = void (*)(const opts &, const char *, device_launcher);
using test = std::tuple<const char *,host_launcher,device_launcher>;


template <typename T>
static void launch_latency_test(
    const opts &os, const char *test_name, device_launcher dl)
{
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  size_t inp_elems = NBS * TPB;
  umem<T> oups {inp_elems, const_seq<T>(T(0))};
  umem<int64_t> times {4 * NBS, const_seq<int64_t>(0)};

  int64_t min_c = std::numeric_limits<int64_t>::max();
  int64_t min_ns = std::numeric_limits<int64_t>::max();
  for (int i = 0; i < os.iterations; i++) {
    dl(times, oups);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
            " (", cudaGetErrorString(e), "): unexpected error");
    }

    min_c = std::min(times[0] / WALKS, min_c);
    min_ns = std::min(times[1] / WALKS, min_ns);
    if (os.verbose()) {
      print_col(vss, format(test_name, ".run[", i ,"]"),
                times[0] / WALKS,
                times[1] / WALKS);
    }
  } // for

  print_col(std::cout, test_name, min_c, min_ns);

  if (os.debug()) {
    vss << "OUPS:\n";
    oups.str(vss, 12, 8, 1);
    vss << "TIMES:\n";
    times.str(vss, 12, 8, 1);
  }
  std::cout << vss.str();
}



static const test ALL_TESTS[] {
  {"mad-i32",  launch_latency_test<int32_t>, [] (int64_t *times, void *inps) {
    mad_latency<int32_t><<<1,32>>>(times, (int32_t *)inps, 0u);
  }},
/*
  {"mad-i64",  launch_latency_test<int64_t>, [] (int64_t *times, void *inps) {
    mad_latency<int64_t><<<1,32>>>(times, (int64_t *)inps, 0);
  }},
  {"mad-f32",  launch_latency_test<float>, [] (int64_t *times, void *inps) {
    mad_latency<float><<<1,32>>>(times, (float *)inps, 0);
  }},
  {"mad-f64",  launch_latency_test<double>, [] (int64_t *times, void *inps) {
    mad_latency<double><<<1,32>>>(times, (double *)inps, 0);
  }},
  */
  // TODO: i16x2, f16x2
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
        "usage: alu-lat [OPTS] TESTS\n"
        "where [OPTS]:\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "                        (defaults to " << DFT_OPTS.iterations << ")\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       // runs a large set of sizes\n"
        "";
      for (const auto [nm,_,__] : ALL_TESTS) {
        uss << "        | " << nm << "\n";
      }
      uss << "  where [NUMZEROS] is an integer number of zeros in the warp message (0..32)\n"
        "EXAMPLES:\n"
        " % alu-lat\n"
        "  runs all tests\n"
        " % alu-lat -i=3\n"
        "  runs all tests (with 3 iterations)\n"
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
        for (const auto x : ALL_TESTS) {
          tests.push_back(x);
        }
      } else {
        bool found = false;
        for (const auto [nm, hf, df] : ALL_TESTS) {
          if (nm == arg) {
            found = true;
            tests.emplace_back(nm, hf, df);
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
  for (const auto [nm,hf,df] : tests) {
    hf(os, nm, df);
  }

  return EXIT_SUCCESS;
} // main
