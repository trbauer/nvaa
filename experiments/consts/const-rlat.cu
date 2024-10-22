#include "mincu.hpp"


using namespace mincu;


static __device__ uint64_t get_time()
{
  uint64_t out = 0;
  asm volatile("mov.u64 %0, %%clock64;" : "=l"(out) :: "memory");
  return out;
}

/*
static __device__ inline uint32_t get_time32()
{
  uint32_t mclk;
  asm volatile("mov.u32 %0, %%clock;" : "=r"(mclk));
  return mclk;
}
*/
typedef uint64_t ulong;
typedef uint32_t uint;

static const int WALKS = 64 * 1024;

static __global__ void ffma_r(
        ulong *out_time,
        float  *out,
  const float4 *in0)
{
  // uint gid = get_global_id(0);
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  float reg = (float)threadIdx.x;
  float sum = 0.0f;
  ulong rt_sum = 0;
  const float4 *in_ptr = in0 + gid;
  // #pragma nounroll
  #pragma unroll 1
  for (uint i = 0; i < WALKS / 16; i++) {
    // load16; time(use16)
    float4 acc[4];
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      acc[k] = *in_ptr++;
    }
    ulong rt_stt = get_time();
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      sum += acc[k].x * reg;
      sum += acc[k].y * reg;
      sum += acc[k].z * reg;
      sum += acc[k].w * reg;
    }
    rt_sum += get_time() - rt_stt;
  }
  out[gid] = sum;
  // printf("T: %lld\n", rt_sum / K);
  out_time[0] = rt_sum / WALKS;
}

static __global__ void ffma_c(
        ulong *out_time,
        float  *out,
  const float4 *in0,
        float konst)
{
  // uint gid = get_global_id(0);
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  float sum = 0.0f;
  ulong rt_sum = 0;
  const float4 *in_ptr = in0 + gid;
  // #pragma nounroll
  #pragma unroll 1
  for (uint i = 0; i < WALKS / 16; i++) {
    // load16; time(use16)
    float4 acc[4];
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      acc[k] = *in_ptr++;
    }
    ulong rt_stt = get_time();
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      sum += acc[k].x * konst;
      sum += acc[k].y * konst;
      sum += acc[k].z * konst;
      sum += acc[k].w * konst;
    }
    rt_sum += get_time() - rt_stt;
  }
  out[gid] = sum;
  // printf("T: %lld\n", rt_sum / K);
  out_time[0] = rt_sum / WALKS;
}


struct test {
  enum class ordinal {R, C} code;
  const char *symbol;
  explicit test (ordinal c, const char *sym) : code(c), symbol(sym) { }
};
static const test R {test::ordinal::R, "r"};
static const test C {test::ordinal::C, "c"};
static const test *all_tests[] {&R, &C};

static void run_test(test t, int itrs, int verbosity)
{
  std::cout << "run_ffma_" << t.symbol << "\n";
  umem<float> oups(1, const_seq<float>(0.0f));
  umem<float4> inps(1, arith_seq<float4>());
  std::vector<umem<uint64_t>> out_times;
  out_times.reserve(itrs);
  std::vector<std::function<void()>> funcs;
  funcs.reserve(itrs);
  //
  for (int i = 0; i < itrs; i++) {
    out_times.emplace_back(1, const_seq<uint64_t>(0));
    if (t.code == test::ordinal::R) {
      funcs.push_back([&,i] {ffma_r<<<1,1>>>(out_times[i], oups, inps);});
    } else if (t.code == test::ordinal::C) {
      funcs.push_back([&,i]{ffma_c<<<1,1>>>(out_times[i], oups, inps, 0.5f);});
    } else {
      fatal("unreachable");
    }
  }
  std::vector<float> elapsed_ss = time_dispatches_s(funcs);
  auto e0 = cudaDeviceSynchronize();
  if (e0 != cudaSuccess) {
    fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
  }
  for (size_t i = 0; i < elapsed_ss.size(); i++) {
    auto elapsed_s = elapsed_ss[i];
    auto clocks = (int64_t)out_times[i][0];
    std::cout << format("elapsed_s[", i, "]: ",
                        frac(elapsed_s, 4), " s, ", clocks, " c\n");
  }
}

int main(int argc, char **argv)
{
  if (argc == 1) {
    fatal("use -h for help");
  }
  const int dft_itrs = 3;
  int itrs = dft_itrs;
  int verbosity = 0;
  std::vector<test> tests;
  for (int ai = 1; ai < argc; ai++) {
    std::string arg = argv[ai];
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
        "usage: " << argv[0] << " [OPTS] TESTS\n"
        "where [OPTS]:\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "                        (defaults to " << dft_itrs << ")\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          a       // runs all tests\n";
      for (const auto *t : all_tests) {
        uss << "        | " << t->symbol << "\n";
      }
      std::cout << uss.str();
      exit(EXIT_SUCCESS);
    } else if (key == "-i=") {
      itrs = parse_integral_positive<int>(val, false);
    } else if (arg == "-v") {
      verbosity = 1;
    } else if (arg == "-v2") {
      verbosity = 2;
    } else if (arg == "-v3") {
      verbosity = 3;
    } else if (arg.substr(0, 1) == "-") {
      bad_opt("invalid option");
    } else if (arg == "a") {
      for (const test *t : all_tests) {
        tests.push_back(*t);
      }
    } else {
      bool found = false;
      for (const test *t : all_tests) {
        if (arg == t->symbol) {
          tests.push_back(*t);
          found = true;
          break;
        }
      }
      if (!found) {
        bad_opt("invalid test name");
      }
    }
  } // for
  for (test t : tests) {
    run_test(t, itrs, verbosity);
  }

  return EXIT_SUCCESS;
}
