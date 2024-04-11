/// OPTIONS nvcc: --expt-relaxed-constexpr
#include "mincu.hpp"

#include <array>
#include <bit>
#include <tuple>


using namespace mincu;


////////////////////////////////////////////////////////////////////////////////
struct opts {
  int verbosity = 0;
  int iterations = 2;
  bool check = false;
  size_t blocks_per_grid = 1024;
  size_t threads_per_block = 256;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

constexpr opts DFT_OPTS;

////////////////////////////////////////////////////////////////////////////////
static __device__ uint32_t get_lane_id() {
  uint32_t __id;
  asm ("mov.u32 %0, %laneid;" : "=r"(__id));
  return __id;
}
static __device__ bool elect() {
  return get_lane_id() == __ffs(__activemask()) - 1;
}

////////////////////////////////////////////////////////////////////////////////
// all algorithms

// number of times we repeat each atomic operation (with the same value)
static const unsigned ITRS = 16;

static const unsigned NUM_BINS = 32;
static_assert(std::popcount(NUM_BINS) == 1, "NUM_BINS must be a power of 2");

static __host__ __device__ unsigned get_bin(unsigned x) {
  return x & (NUM_BINS - 1);
}

////////////////////////////////////////////////////////////////////////////////
// algorithm::glb_hw
extern "C"
__global__ void count_bins_glb_hw(
    uint32_t *bins, const uint32_t *inps, uint32_t zero)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t val = inps[gid];
  // hammer the atomic ITRS times with the same value
  for (unsigned i = 0; i < ITRS; i++) {
    const unsigned b = get_bin(val + i * zero);
    atomicAdd(&bins[b], 1);
  }
}

////////////////////////////////////////////////////////////////////////////////
// algorithm::glb_sw0
extern "C"
__global__ void count_bins_glb_sw0(
    uint32_t *bins, const uint32_t *inps, uint32_t zero)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t val = inps[gid];
  const auto lane_id = get_lane_id();
  // hammer the atomic ITRS times with the same value
  for (unsigned i = 0; i < ITRS; i++) {
    const unsigned b = get_bin(val + i * zero);
    if (b == 0) { // manually collapse all to bin zero
      // auto zeros = std::popcount(__activemask()); // generates nasty emu code
      auto num_zeros = __popc(__activemask()); // generates POPC
      if (elect()) { // elect a lane
        // printf("elect() on 0 suppressed %d other cases 0x%x %d\n",
        //    num_zeros - 1, __activemask(), num_zeros);
        atomicAdd(&bins[0], num_zeros);
      }
    } else {
      atomicAdd(&bins[b], 1);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// algorithm::shm_hw
extern "C"
__global__ void count_bins_shm_hw(
    uint32_t *bins, const uint32_t *inps, uint32_t zero)
{
  __shared__ uint32_t sh_bins[NUM_BINS];
  for (int i = threadIdx.x; i < NUM_BINS; i += blockDim.x) {
    sh_bins[i] = 0;
  }
  __syncthreads();

  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t val = inps[gid];
  // hammer the atomic ITRS times with the same value
  for (unsigned i = 0; i < ITRS; i++) {
    const unsigned b = get_bin(val + i * zero);
    atomicAdd(&sh_bins[b], 1);
  }
  __syncthreads();

  for (int i = threadIdx.x; i < NUM_BINS; i += blockDim.x) {
    atomicAdd(&bins[i], sh_bins[i]);
  }
}



////////////////////////////////////////////////////////////////////////////////
enum class algorithm {glb_hw, glb_sw0, shm_hw};
static const algorithm ALL_ALGORITHMS[] {
  algorithm::glb_hw,
  algorithm::glb_sw0,
  algorithm::shm_hw,
};
std::string str(algorithm a) {
  switch (a) {
  case algorithm::glb_hw: return "glb-hw";
  case algorithm::glb_sw0: return "glb-sw0";
  case algorithm::shm_hw: return "shm-hw";
  default: fatal("invalid algorithm");
  }
  return "???";
}

static void dispatch(
    const opts &os, algorithm a, uint32_t *oups, const uint32_t *inps)
{
  const auto bpg = os.blocks_per_grid;
  const auto tpb = os.threads_per_block;
  switch (a) {
  case algorithm::glb_hw:
    count_bins_glb_hw<<<bpg,tpb>>>(oups, inps, 0);
    break;
  case algorithm::shm_hw:
    count_bins_shm_hw<<<bpg,tpb>>>(oups, inps, 0);
    break;
  case algorithm::glb_sw0:
    count_bins_glb_sw0<<<bpg,tpb>>>(oups, inps, 0);
    break;
  default:
    fatal("invalid algorithm");
  }
}

////////////////////////////////////////////////////////////////////////////////
// datasets
static const int ALL_ZERO_COUNTS[] {
  0, 1,
//  4,  8, 12,
  16,
//  20, 24, 28,
  31, 32
  };

////////////////////////////////////////////////////////////////////////////////
static void referee_bins(
  const opts &os, uint32_t *bins, const uint32_t *inps, unsigned delta)
{
  std::array<uint32_t, NUM_BINS> ref_bins = {};
  size_t total_threads = os.blocks_per_grid * os.threads_per_block;

  int errs = 0;
  for (size_t gid = 0; gid < total_threads; gid++) {
    auto val = inps[gid];
    for (unsigned i = 0; i < ITRS; i++) {
      const unsigned b = get_bin(val + i * delta);
      ref_bins[b]++;
    }
  }
  for (auto i = 0u; i < ref_bins.size(); i++) {
    if (bins[i] != ref_bins[i]) {
      std::cerr << format("bins[", i, "]: has # ",
          bins[i], ", but we expected ", ref_bins[i], "\n");
      errs++;
    }
  }
  if (errs)
    fatal("mismatched");
}

static void print_headers()
{
  std::cout << coll("algorithm", 16) << "  ";
  std::cout << coll("input", 16) << "  ";
  std::cout << colr("rate(B*atom/s)", 16);
  std::cout << "\n";

}
static void run_test(
    const opts &os,
    algorithm alg,
    unsigned num_zeros_per_message)
{
  std::cout << coll(str(alg), 16) << " ";

  std::stringstream iss;
  iss << "zs" << std::setfill('0') << std::setw(2) << num_zeros_per_message;
  std::cout << coll(format(iss.str()), 16) << "  ";

  std::function<unsigned(size_t)> init =
      [&] (size_t ix) {
        // lower [num_zeros_per_message] lanes will have 0's and the rest
        // will have lane_id + 1 (e.g. 1,2,3...) clamped to 32
        // (so we will not hit bin 0)
        auto lane_id = ix % 32;
        auto non_zero = std::min(32u, (unsigned)lane_id + 1);
        return lane_id < (size_t)num_zeros_per_message ? 0 : non_zero;
      };

  umem<uint32_t> bins {NUM_BINS};
  const umem<uint32_t> inps {os.blocks_per_grid * os.threads_per_block, init};

  const double total_atms = inps.size() * ITRS;

  std::stringstream vss;
  float min_t = 0.0f;
  for (int i = 0; i < os.iterations; i++) {
    bins.init(const_seq(0u));
    float t = time_dispatch_s([&] {dispatch(os, alg, bins, inps);});
    if (os.debug()) {
      vss << "run[" << i << "]: " << frac(total_atms / t, 3) << "  ATM/s\n";
    }
    min_t = i == 0 ? t : std::min(min_t, t);
  }

  const double max_rate = total_atms / min_t;
  std::cout <<
    colr(frac(max_rate / 1e9, 3), 16) << "  billion*atomic/s\n";

  std::cout << vss.str();
  if (os.verbose()) {
    const double total_mb =
        (double)(inps.size() * sizeof(inps[0])) / 1024.0 / 1024.0;
    std::cout << "  elems:  " << inps.size() << " elems\n";
    std::cout << "  mem:    " << frac(total_mb, 3) << " MB (input buffer size)\n";
    std::cout << "  time:   " << frac(min_t, 6) << " s\n";
  }

  if (os.verbose_debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 8);
  }
  if (os.verbose_debug()) {
    std::cout << "BINS:\n";
    bins.str(std::cout, 8);
  }

  if (os.check) {
    referee_bins(os, bins, inps, 0);
    if (os.normal()) {
      std::cout << "referee check passed\n";
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
using test = std::tuple<algorithm,unsigned>;

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
        "usage: atomic-same-address.exe OPTS TESTS\n"
        "where OPTS:\n"
        "  -bpg=INT              blocks per grid (defaults to " << DFT_OPTS.blocks_per_grid << ")\n"
        "  -tpb=INT              threads per blocks (defaults to " << DFT_OPTS.threads_per_block << ")\n"
        "  --check               referee the output on CPU\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       // runs a large set of sizes\n"
        "        | ALL       // runs all sizes \n"
        "";
      for (algorithm a : ALL_ALGORITHMS) {
        uss << "        | " << str(a) << "-zs[NUMZEROS]\n";
      }
      uss << "  where [NUMZEROS] is an integer number of zeros in the warp message (0..32)\n"
        "EXAMPLES:\n"
        " % atomic-same-address all\n"
        "  run everything\n"
        " % atomic-same-address -bpg=4k all\n"
        "  generates and processes 4k blocks per grid on all tests\n"
        "";
      std::cout << uss.str();
      return EXIT_SUCCESS;
    } else if (arg == "--check") {
      os.check = true;
    } else if (key == "-bpg=") {
      os.blocks_per_grid = parse_integral_positive<size_t>(val, true);
    } else if (key == "-tpb=") {
      os.threads_per_block = parse_integral_positive<size_t>(val, true);
    } else if (key == "-i=" || key == "--iterations=") {
      os.iterations = parse_integral_positive<int>(val, false);
    } else if (arg == "-v0" || arg == "-v=0") {
      os.verbosity = 0;
    } else if (arg == "-v" || arg == "-v1" || arg == "-v=1") {
      os.verbosity = 1;
    } else if (arg == "-v2" || arg == "-v=2") {
      os.verbosity = 2;
    } else if (arg == "-v3" || arg == "-v=3") {
      os.verbosity = 3;
    } else if (arg == "all") {
      for (algorithm a : ALL_ALGORITHMS) {
        for (int z : ALL_ZERO_COUNTS) {
          tests.emplace_back(a, z);
        }
      }
    } else if (arg == "ALL") {
      for (algorithm a : ALL_ALGORITHMS) {
        for (int z = 0; z <= 32; z++) {
          tests.emplace_back(a, z);
        }
      }
    } else if (arg.size() > 0 && arg[0] == '-') {
        bad_opt("invalid option");
    } else {
      // e.g. hw-glb-2 or sw0-glb-3
      auto pos = arg.rfind('-');
      if (pos == std::string::npos) {
        bad_opt("invalid test name; should be ");
      }
      auto pfx = arg.substr(0, pos);
      bool found_alg = false;

      algorithm alg;
      for (algorithm a : ALL_ALGORITHMS) {
        if (str(a) == pfx) {
          alg = a;
          found_alg = true;
          break;
        }
      }
      if (!found_alg)
        bad_opt("invalid algorithm");

      auto numstr = arg.substr(pos + 1);
      if (numstr.size() < 2 || numstr[0] != 'z' || numstr[1] != 's')
        bad_opt("invalid data set name; should be zs[INT]");
      auto num = parse_integral<unsigned>(numstr.substr(2), false);
      if (num > 32) {
        bad_opt("invalid number of zeros");
      }
      tests.emplace_back(alg, num);
    }
  } // for

  if (tests.empty()) {
    fatal("expected test name (try -h)");
  }

  print_headers();
  for (const auto &[alg,nzeros] : tests) {
    run_test(os, alg, nzeros);
  }

  return EXIT_SUCCESS;
}
