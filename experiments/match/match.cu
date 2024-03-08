#include <cuda_runtime.h>

// #include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx
#include <cstdio>
#include <iostream>
#include <string>
#include <set>
#include <tuple>
#include <vector>

#include "mincu.hpp"

using namespace mincu;

static int verbosity = 0;

//  asm volatile ("atom.global.v2.f32.add  {%0, %1}, [%2], {%3, %4};"
//    : "=f"(ret.x), "=f"(ret.y) : "l"(dst + gid), "f"(src.x), "f"(src.y));

/*
__global__ void early_return_and_partial_subset(unsigned *dsts, const float *srcs)
{
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto val = srcs[gid];
  if (threadIdx.x >= 8) {
    return;
  }
  unsigned x = __match_any_sync(0x0000FFFFu, val);
  dsts[gid] = x;
}

  umem<unsigned> oups(32, init_const<unsigned>(0u));
  umem<float> inps(32, init_const<float>(0.0f));
  inps[1] = -0.0f;
  inps[3] = 1.0f;
  inps[8] = 2.0f;
  inps[9] = 2.0f;
  inps.str(std::cout, 8, 3);

  match_latency<<<1,32>>>(oups, inps);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  oups.str(std::cout, 8);

*/
static const int MATCH_ITRS = 64;

// __device__ uint32_t match_any_emu(uint32_t value) { }

__global__ void match_any_latency(
    uint64_t *times,
    uint32_t *oups,
    const uint32_t *srcs,
    uint32_t zero)
{
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t x0 = srcs[gid];
  uint32_t x = x0;
//  if (threadIdx.x == 31)
//    printf("%08x\n", x);
  auto st = clock64();
  for (int i = 0; i < MATCH_ITRS; i++) {
    // Create sequence of b2b dependency
    // Force the compiler to use the initial value only in matching values.
    // This inserts an IMAD into the pipe, but that's lower latency
    // than a fake compare.
    // int z = 0; x = __match_all_sync(0xFFFFFFFFu, x, &z) * zero + x0;
    x = __match_any_sync(0xFFFFFFFFu, x) * zero + x0;
//    if (threadIdx.x == 31) // ensure the values are not changing
//      printf("%08x\n", x);
  }
  auto en = clock64();
  oups[gid] = x;
  times[gid] = en - st;
}
__global__ void match_all_latency(
    uint64_t *times,
    uint32_t *oups,
    const uint32_t *srcs,
    uint32_t zero)
{
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t x0 = srcs[gid];
  uint32_t x = x0;
  auto st = clock64();
  for (int i = 0; i < MATCH_ITRS; i++) {
    // Create sequence of b2b dependency
    // Force the compiler to use the initial value only in matching values.
    // This inserts an IMAD into the pipe, but that's lower latency
    // than a fake compare.
    int z = 0; x = __match_all_sync(0xFFFFFFFFu, x, &z) * zero + x0;
  }
  auto en = clock64();
  oups[gid] = x;
  times[gid] = en - st;
}

static const int ITERATIONS = 2; // drop the first run to hide for warmup costs
static const size_t BLOCKS = 1; // 1 warp only
static const size_t TPB = 32; // threads per block (1 warp)

enum class test_op {ANY, ALL};

// test - label (no spaces)
// comment - what is this test showing (if anything); can be nullptr
// setter - a lambda function that sets each element based on index
static void test_latency_on(
    test_op op,
    std::string test,
    std::function<uint32_t(size_t)> setter,
    const char *comment = nullptr)
{
  std::cout << coll<std::string>(test, 24) << "  ";

  // Compute how many equivalence classes there are in the input;
  // this is to check how many times the HW must iterate.
  std::set<uint32_t> represenative_set;
  for (size_t i = 0; i < 32; i++) {
    represenative_set.insert(setter(i));
  }
  std::cout << colr<size_t>(represenative_set.size(), 12) << " ";

  umem<uint64_t> times(BLOCKS * TPB, const_seq<uint64_t>(0ull));
  umem<uint32_t> inps(BLOCKS * TPB, setter);
  umem<uint32_t> oups(BLOCKS * TPB, const_seq<uint32_t>(0u));

  std::stringstream vss;
  if (verbosity >= 1) {
    inps.str(vss, 8);
  }

  for (int i = 0; i < ITERATIONS; i++) {
    if (op == test_op::ANY) {
      match_any_latency<<<BLOCKS,TPB>>>(times, oups, inps, 0u);
    } else {
      match_all_latency<<<BLOCKS,TPB>>>(times, oups, inps, 0u);
    }
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
    }
  }

  std::cout << colr<frac>(frac(times[0]/(double)MATCH_ITRS, 1), 12);
  if (comment) {
    std::cout << " " << comment;
  }
  std::cout << "\n";

  if (verbosity >= 1) {
    vss << "oups:\n";
    oups.str(vss, 8); // 8 per row

    vss << "times:\n";
    times.str(vss, 8);

    std::cout << vss.str();
  }
}

static void test_latency(test_op op)
{
  std::cout << "========= tests for " <<
      (op == test_op::ANY ? "match_any" : "match_all") << "\n";
  std::cout <<
      coll<std::string>("test", 24) << " " <<
      colr<std::string>("equiv.classes", 12) << " " <<
      colr<std::string>("latency(c/match)",12) << " " <<
      coll<std::string>("hint", 24) << "\n";
  if (verbosity >= 1) {
    test_latency_on(op, "1-all-zeros", [](size_t ix){return 0u;});
    test_latency_on(op, "1-all-ones", [](size_t ix){return ~0u;},
                    "perf is value-insensitive");
    test_latency_on(op, "1-all-42", [](size_t ix){return 42u;},
                    "perf is value-insensitive");
  }

  for (uint32_t i = 1; i <= 32; i++) {
    auto tnm = i == 1 ? format(i, "-class") : format(i, "-classes");
    test_latency_on(op, tnm, [&](size_t ix){return (uint32_t)ix % i;});
    if (verbosity >= 1)
      test_latency_on(op,
                      tnm + "-rotated",
                      [&](size_t ix){return (uint32_t)(ix + 1) % i;},
                      "perf is value-insensitive");
  }
/*
  test_latency_on("1-equivalence-classes-ones", inps_1s);
  umem<uint32_t> inps_42s(BLOCKS * TPB, const_seq<uint32_t>(42u));
  test_latency_on("1-equivalence-classes-42", inps_42s);

  umem<uint32_t> inps_disj2(BLOCKS * TPB, cyc_seq<uint32_t>({0u, 1u}));
  test_latency_on("2-equivalence-classes", inps_disj2);

  umem<uint32_t> inps_disj3(BLOCKS * TPB, cyc_seq<uint32_t>({0u, 1u, 2u}));
  test_latency_on("3-equivalence-classes", inps_disj3);

  // repeat of four distinct elements
  umem<uint32_t> inps_disj4(BLOCKS * TPB, cyc_seq<uint32_t>({0u, 1u, 2u, 3u}));
  test_latency_on("4-equivalence-classes", inps_disj4);

  umem<uint32_t> inps_disj4b(BLOCKS * TPB, cyc_seq<uint32_t>({3u, 0u, 2u, 1u}));
  test_latency_on("4-equivalence-classes (permuted)", inps_disj4b);

  // repeat of eight distinct elements
  umem<uint32_t> inps_disj8(BLOCKS * TPB, cyc_seq<uint32_t>({3u, 0u, 2u, 1u, 4u, 5u, 6u, 7u}));
  test_latency_on("8-equivalence-classes", inps_disj8);

  // repeat of eight distinct elements
  umem<uint32_t> inps_disj16(BLOCKS * TPB,
      cyc_seq<uint32_t>({0x3u, 0x0u, 0x2u, 0x1u, 0x4u, 0x5u, 0x6u, 0x7u,
                         0xEu, 0x9u, 0xAu, 0xFu, 0xCu, 0xDu, 0x8u, 0xBu}));
  test_latency_on("16-equivalence-classes", inps_disj16);
  umem<uint32_t> inps_disj16b(BLOCKS * TPB, arith_seq<uint32_t>(0u, 2u)); // 0, 2, ...
  test_latency_on("16-equivalence-classes-b", inps_disj16b);

  umem<uint32_t> inps_disj32(BLOCKS * TPB, arith_seq<uint32_t>(0u, 1u)); // 0, 1, ..., 31
  test_latency_on("32-equivalence-classes", inps_disj32);
*/
}


int main(int argc, const char **argv)
{
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    std::string key = arg, val;
    auto eq = arg.find('=');
    if (eq != std::string::npos) {
      key = arg.substr(0, eq + 1); // include the =
      val = arg.substr(eq + 1);
    }
    auto bad_opt = [&](std::string msg0, std::string msg1 = "", std::string msg2 = "") {
      fatal(arg, ": ", msg0, msg1, msg2);
    };

    if (arg == "-h" || arg == "--help") {
      std::cout <<
        "usage: match.exe OPTS\n"
        "where OPTS are:\n"
        "  -v/-v2                  sets verbosity\n"
        "EXAMPLES:\n"
        " % match\n"
        "";
      return EXIT_SUCCESS;
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (!arg.empty() && arg[0] == '-') {
      bad_opt("unexpected option");
    } else {
      bad_opt("unexpected argument");
    }
  } // for args

  test_latency(test_op::ANY);
  test_latency(test_op::ALL);
  return EXIT_SUCCESS;
}


/*
__global__ void copy_unif(float *dsts, const float *srcs)
{
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  __shared__ float smem[32];

  float val;
  asm volatile ("ldu.global.f32  %0, [%1];"
    : "=f"(val) : "l"(srcs + gid));


  smem[(threadIdx.x + 1) % 32] = val;

  __syncthreads();

  val = smem[threadIdx.x];
  // asm volatile ("ldu.f32  %0, [%1];"
  //  : "=f"(val) : "l"(smem + threadIdx.x));

  dsts[gid] = val;
}
*/