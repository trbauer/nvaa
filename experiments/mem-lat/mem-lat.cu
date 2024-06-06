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


static __device__ int64_t get_globaltimer()
{
  int64_t t;
  asm volatile ("mov.u64 %0, %globaltimer;" : "=l"(t));
  return t;
}

using device_launcher = std::function<void(int64_t *,uint32_t *,const uint32_t *)>;
using host_launcher = void (*)(const opts &, std::string, device_launcher);
using test = std::tuple<const char *,host_launcher,device_launcher>;

static const int WALKS = 1024*1024;

////////////////////////////////////////////////////////////////////////////////
__global__ void load_latency_l1(
          int64_t *__restrict__ times,
          uint32_t *__restrict__ oups,
    const uint32_t *__restrict__ inps,
          uint32_t zero)
{
  const size_t tid = blockIdx.x * blockDim.x + threadIdx.x;
  extern __shared__ uint32_t smem[];

  const uint32_t *ptr = inps + tid;
  // prefetch this 128B and force the dependency
  if (*ptr > 0) {
    ptr++;
    // assert(0 && "whoops, *ptr should be zero!");
  }
  const auto st_c = clock64();
  const auto st_ns = get_globaltimer();
  for (int i = 0; i < WALKS; i++) {
    auto val = *ptr;
    ptr += val; // should be zero
  }
  const auto ed_c = clock64();
  const auto ed_ns = get_globaltimer();
  //
  if (threadIdx.x == 0) {
    times[4 * blockIdx.x + 0] = st_c;
    times[4 * blockIdx.x + 1] = ed_c;
    times[4 * blockIdx.x + 2] = st_ns;
    times[4 * blockIdx.x + 3] = ed_ns;
  }
  oups[tid] = *ptr;
}

////////////////////////////////////////////////////////////////////////////////
__global__ void load_latency_smem(
          int64_t *__restrict__ times,
          uint32_t *__restrict__ oups,
    const uint32_t *__restrict__ inps,
          uint32_t zero)
{
  const size_t tid = blockIdx.x * blockDim.x + threadIdx.x;
  extern __shared__ uint32_t smem[];

  smem[threadIdx.x] = inps[tid];
  __syncthreads();
  const uint32_t *ptr = smem + threadIdx.x;

  const auto st_c = clock64();
  const auto st_ns = get_globaltimer();
  for (int i = 0; i < WALKS; i++) {
    auto val = *ptr;
    ptr += val; // should be zero
  }
  const auto ed_c = clock64();
  const auto ed_ns = get_globaltimer();
  //
  if (threadIdx.x == 0) {
    times[4 * blockIdx.x + 0] = st_c;
    times[4 * blockIdx.x + 1] = ed_c;
    times[4 * blockIdx.x + 2] = st_ns;
    times[4 * blockIdx.x + 3] = ed_ns;
  }
  oups[tid] = *ptr;
}

static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << " " <<
        colr("Latency(c)", 16) << " " <<
        colr("Latency(ns)", 16) << "\n";
}

static void print_col(std::ostream &os,
                      std::string test, double val_c, double val_ns)
{
  os << coll(test, 16) << " " <<
        colr(frac(val_c, 1), 16) << " " <<
        colr(frac(val_c, 1), 16) << "\n";
}

#ifdef USE_UMEM
template <typename T> using mem = umem<T>;
#else // USE_DMEM
template <typename T> using mem = dmem<T>;
#endif // USE_UMEM

static const size_t NBS = 1, TPB = 32;


static void latency_test(
    const opts &os,
    std::string test_name,
    device_launcher dev)
{
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  size_t inp_elems = NBS * TPB;
  mem<uint32_t> inps {inp_elems, const_seq<uint32_t>(0u)};
  mem<uint32_t> oups {inp_elems, const_seq<uint32_t>(0u)};
  mem<int64_t> times {NBS * 4, const_seq<int64_t>(0)};

  if (os.debug()) {
    vss << "INPS:\n";
    inps.str(vss, 8);
  }

  double min_c = std::numeric_limits<int64_t>::max();
  double min_ns = std::numeric_limits<int64_t>::max();
  for (int i = 0; i < os.iterations; i++) {
    float s = time_dispatch_s([&] {dev(times, oups, inps);});

    const auto ts = times.to_vector();
    const int64_t i_this_c = ts[1] - ts[0];
    const int64_t i_this_ns = ts[3] - ts[2];
    const double this_c = i_this_c / (double)WALKS;
    const double this_ns = i_this_ns / (double)WALKS;
    min_c = std::min(this_c, min_c);
    min_ns = std::min(this_ns, min_ns);
    if (os.verbose()) {
      vss << frac(s * 1e9 / WALKS, 4) << " ns / walk\n";
      print_col(vss, format(test_name,".run[", i ,"]"), this_c, this_ns);
    }
  } // for

  print_col(std::cout, test_name, min_c, min_ns);

  if (os.debug()) {
    vss << "OUPS:\n";
    oups.str(vss, 8);
  }
  if (os.verbose()) {
    vss << "TIMES:\n";
    times.str(vss, 8);
  }
  std::cout << vss.str();
}

static const test ALL_TESTS[] {
  {"l1",  latency_test, [](int64_t *ts, uint32_t *os, const uint32_t *is) {
    load_latency_l1<<<NBS,TPB,TPB*sizeof(uint32_t)>>>(ts, os, is, 0);
  }},
  {"smem",  latency_test, [](int64_t *ts, uint32_t *os, const uint32_t *is) {
    load_latency_smem<<<NBS,TPB,TPB*sizeof(uint32_t)>>>(ts, os, is, 0);
  }},
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
        "usage: mem-lat [OPTS] TESTS\n"
        "where [OPTS]:\n"
        "  --check               referee the output on CPU\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       // runs a large set of sizes\n"
        "";
      for (const auto [nm,_,__] : ALL_TESTS) {
        uss << "        | " << nm << "\n";
      }
      uss << "\n"
        "EXAMPLES:\n"
        " % mem-lat all\n"
        "  tests all algorithms\n"
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
