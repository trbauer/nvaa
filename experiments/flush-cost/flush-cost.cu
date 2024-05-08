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

// loads 'bytes' bytes into the cache and then time the flush (a fence)
__global__ void flush_latency(
          int64_t *times,
          uint4 *oups,
   /* const */ uint4 *inps,
          int bytes,
          int zero)
{
  const size_t tid0 = (size_t)bytes * blockIdx.x * blockDim.x;
  const size_t tid = tid0 + threadIdx.x;
  uint4 *inps_walk = inps + tid;

  // load up the cache
  uint4 sum = make_uint4(0, 0, 0, 0);
  // for (int i = 0, b = 0; b < bytes; i++, b += 512)
  for (int b = 0; b < bytes; b += 512) {
    // if (threadIdx.x < 2) {printf("tid%02d: offset: %p\n", threadIdx.x, inps_walk - inps);}
    // sum ^= inps[tid + i * blockDim.x]; // 512 B = 32 x int4
    auto val = *inps_walk;
    inps_walk += blockDim.x;
    sum ^= val;
  }
  // place dependency on the sum
  if ((sum.x % 2) == 1) {
    sum += sum;
  }
  const auto st = clock64();
  // https://docs.nvidia.com/cuda/parallel-thread-execution/#cache-operators
  // issue the flush
  // __threadfence() and reload
  // asm volatile("membar.gl;" ::: "memory");
  //
  const auto st_ld = clock64();
  // zero bytes breaks here because the fence isn't necessarily committed
  if (bytes > 0) {
    // this load forces the fence to be completed so the end sample time
    // doesn't include the fence in flight
    sum.x ^= inps[tid + zero * blockDim.x].x; // reload the first block
  }
  //
  const auto ed = clock64();
  //
  auto c_elapsed = ed - st;
  auto c_elapsed_ld = ed - st_ld; // should be long latency
  //
  if (threadIdx.x == 0) {
    times[blockIdx.x * blockDim.x + threadIdx.x] = c_elapsed;
  } else if (threadIdx.x == 1) {
    times[blockIdx.x * blockDim.x + threadIdx.x] = c_elapsed_ld;
  }
  oups[blockIdx.x * blockDim.x + threadIdx.x] = sum;
}

const int NUM_FLUSHES = 64;

__global__ void flush_loop(
          int64_t *times,
          uint4 *oups,
   /* const */ uint4 *inps,
          int bytes,
          int zero)
{
  const auto st = clock64();
  for (int i = 0; i < NUM_FLUSHES; i++) {
    asm volatile("membar.gl;" ::: "memory");
  }
  const auto ed = clock64();
  if (threadIdx.x == 0) {
    times[blockIdx.x * blockDim.x + threadIdx.x] = (int64_t)(ed - st);
  }
}

static void print_col_headers(std::ostream &os) {
  os << coll("Test", 16) << " " <<
        colr("Flush(c)", 16) << " " <<
        colr("LoadLat(c)", 16) << "\n";
}

static void print_col(std::ostream &os, std::string key, int64_t flush, int64_t l1) {
  os << coll(key, 16) << " " <<
        colr(flush, 16) << " " <<
        colr(l1, 16) << "\n";
}

static void launch_flush_latency(const opts &os, int kb)
{
  auto test_name = format((kb / 1024), "-kb");
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  const size_t NBS = 1, TPB = 32; // need 32 to fill the memory faster

  size_t inp_elems = std::max<size_t>(1, NBS * kb / sizeof(uint4));
  dmem<uint4> inps {inp_elems, const_seq<uint4>(make_uint4(0,1,2,3))};
  if (os.debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 2);
  }

  dmem<uint4> oups {NBS * TPB, const_seq<uint4>(make_uint4(0,0,0,0))};
  dmem<int64_t> times {2, const_seq<int64_t>(0)};

  for (int i = 0; i < os.iterations; i++) {
    flush_latency<<<NBS,TPB>>>(times, oups, inps, kb, 0);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
        " (", cudaGetErrorString(e), "): unexpected error");
    }
    if (os.verbose()) {
      times.read([&](const int64_t *times) {
        print_col(vss, format(test_name,".run[", i ,"]"), times[0], times[1]);
      });
    }
  }

  times.read([&](const int64_t *times) {
    print_col(std::cout, test_name, times[0], times[1]);
  });

  if (os.debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 2);
    std::cout << "TIMES:\n";
    times.str(std::cout, 2);
  }
}



static void launch_flush_loop(const opts &os, int kb)
{
  auto test_name = "flush-loop";
  if (os.verbose()) {
    std::cout << "starting " << test_name << "\n";
  }

  std::stringstream vss;

  const size_t NBS = 1, TPB = 32; // need 32 to fill the memory faster

  size_t inp_elems = std::max<size_t>(1, NBS * kb / sizeof(uint4));
  dmem<uint4> inps {inp_elems, const_seq<uint4>(make_uint4(0,1,2,3))};
  if (os.debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 2);
  }

  dmem<uint4> oups {NBS * TPB, const_seq<uint4>(make_uint4(0,0,0,0))};
  dmem<int64_t> times {2, const_seq<int64_t>(0)};

  for (int i = 0; i < os.iterations; i++) {
    flush_loop<<<NBS,TPB>>>(times, oups, inps, kb, 0);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e),
        " (", cudaGetErrorString(e), "): unexpected error");
    }
    if (os.verbose()) {
      times.read([&](const int64_t *times) {
        print_col(vss, format(test_name,".run[", i ,"]"), times[0] / NUM_FLUSHES, 0);
      });
    }
  }

  times.read([&](const int64_t *times) {
    print_col(std::cout, test_name, times[0] / NUM_FLUSHES, 0);
  });

  if (os.debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 2);
    std::cout << "TIMES:\n";
    times.str(std::cout, 2);
  }
}

// using handler = std::function<void (const opts &os)>;
using host_launcher = void (*)(const opts &, int);
using test = std::tuple<int,host_launcher>;

static const test ALL_TESTS[] {
  {0,       launch_flush_latency},
  {16*1024, launch_flush_latency},
  {32*1024, launch_flush_latency},
  {64*1024, launch_flush_latency},
  {96*1024, launch_flush_latency},
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
        "        | ALL       // runs all sizes \n"
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
        for (const auto [sz,fn] : ALL_TESTS) {
          tests.emplace_back(sz, fn);
        }
        tests.emplace_back(0, launch_flush_loop);
      } else if (arg == "flush-loop") {
        tests.emplace_back(0, launch_flush_loop);
      } else {
        int bytes = parse_integral<int>(arg, true);
        if (bytes % 512 != 0) {
          fatal("test size must be a multiple of 512 bytes");
        }
        tests.emplace_back(bytes, launch_flush_latency);
      }
    }
  } // for
  if (tests.empty()) {
    fatal("expected at least one test (use -h)");
  }

  print_col_headers(std::cout);
  for (const auto [sz,fn] : tests) {
    fn(os, sz);
  }

  return EXIT_SUCCESS;
} // main
