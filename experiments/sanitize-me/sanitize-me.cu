/// OPTIONS nvcc: -g -G
//
// Tests various compute sanitizer tools
// MemCheck
// https://docs.nvidia.com/compute-sanitizer/ComputeSanitizer/index.html#memcheck-tool
// TODO: test more memcheck tools
//    - mis-alignment
//    - uninit on device side (last elem and first elem)
#include "mincu.hpp"

#include <tuple>

using namespace mincu;


////////////////////////////////////////////////////////////////////////////////
struct opts {
  int verbosity = 0;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};
constexpr opts DFT_OPTS;

////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
extern "C"
__global__ void oob_glb_rd(uint32_t *oups, const uint32_t *inps)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto oob_val = inps[gid + 1]; // last one will be OOB
  oups[gid] = oob_val;
}
extern "C"
__global__ void oob_glb_wr(uint32_t *oups, const uint32_t *inps)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto val = inps[gid]; // last one will be OOB
  oups[gid + 1] = val;
}
extern "C"
__global__ void oob_shm_rd(uint32_t *oups, const uint32_t *inps)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;

  __shared__ uint32_t smem[64];

  smem[threadIdx.x] = inps[gid];
  __syncthreads();

  auto oob_val = smem[threadIdx.x + 1]; // last one will be OOB
  oups[gid] = oob_val;
}
extern "C"
__global__ void oob_shm_wr(uint32_t *oups, const uint32_t *inps)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;

  __shared__ uint32_t smem[64];

  smem[threadIdx.x - 1] = inps[gid];
  __syncthreads();

  auto oob_val = smem[threadIdx.x]; // last one will be OOB
  oups[gid] = oob_val;
}

extern "C"
__global__ void sqaure_kernel(uint32_t *oups, const uint32_t *inps)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto val = inps[gid];
  oups[gid] = val * val;
}

////////////////////////////////////////////////////////////////////////////////
using device_wrapper_t =
    std::function<void (size_t, size_t, uint32_t *, const uint32_t *)>;
using host_launcher_t = void (*)(const opts &, const char *, device_wrapper_t);
using test_t = std::tuple<const char *,host_launcher_t,device_wrapper_t>;

////////////////////////////////////////////////////////////////////////////////
static void launch_kernel(
    const opts &os, const char *tnm, device_wrapper_t wrapper)
{
  const size_t TPB = 64;
  const size_t BPG = 1;

  std::cout << "================ running " << tnm << "\n";

  const umem<uint32_t> inps {BPG * TPB, arith_seq<uint32_t>(0)};
  if (os.verbose_debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 8);
  }

  umem<uint32_t> oups {BPG * TPB};
  wrapper(BPG, TPB, oups, inps);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  if (os.debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 8);
  }
}

////////////////////////////////////////////////////////////////////////////////
static void launch_kernel_uninit(
    const opts &os, const char *tnm, device_wrapper_t wrapper)
{
  const size_t TPB = 64;
  const size_t BPG = 1;

  std::cout << "================ running " << tnm << "\n";

  // const umem<uint32_t> inps {BPG * TPB, arith_seq<uint32_t>(0)};
  const umem<uint32_t> inps {BPG * TPB};
  if (os.verbose_debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 8);
  }

  umem<uint32_t> oups {BPG * TPB};
  wrapper(BPG, TPB, oups, inps);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  if (os.debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 8);
  }
}

////////////////////////////////////////////////////////////////////////////////
static void launch_kernel_leak(
    const opts &os, const char *tnm, device_wrapper_t wrapper)
{
  const size_t TPB = 64;
  const size_t BPG = 1;

  std::cout << "================ running " << tnm << "\n";

  // const umem<uint32_t> inps {BPG * TPB, arith_seq<uint32_t>(0)};
  void *d_inps;
  CUDA_API(cudaMalloc, &d_inps, BPG * TPB * sizeof(uint32_t));
  CUDA_API(cudaMemset, d_inps, 42, BPG * TPB * sizeof(uint32_t));
  if (os.verbose_debug()) {
    std::cout << "INPS:\n" << "[[cannot show INPS on this test]]";
  }

  umem<uint32_t> oups {BPG * TPB};
  wrapper(BPG, TPB, oups, (const uint32_t *)d_inps);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  if (os.debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 8);
  }
  // OOPS!
  // CUDA_API(cudaFree, d_inps);
}


////////////////////////////////////////////////////////////////////////////////
static const test_t ALL_TESTS[] {
  {"oob-glb-rd", launch_kernel,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      oob_glb_rd<<<bpg,tpb>>>(oups, inps);
    }},
  {"oob-glb-wr", launch_kernel,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      oob_glb_wr<<<bpg,tpb>>>(oups, inps);
    }},
  {"oob-shm-rd", launch_kernel,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      oob_shm_rd<<<bpg,tpb>>>(oups, inps);
    }},
  {"oob-shm-wr", launch_kernel,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      oob_shm_wr<<<bpg,tpb>>>(oups, inps);
    }},
  {"uninit-glb", launch_kernel_uninit,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      sqaure_kernel<<<bpg,tpb>>>(oups, inps);
    }},
  {"leak-glb", launch_kernel_leak,
    [] (size_t bpg, size_t tpb, uint32_t *oups, const uint32_t *inps) {
      sqaure_kernel<<<bpg,tpb>>>(oups, inps);
    }},
};


////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char* argv[])
{
  std::string exe = argv[0]; // foo\\bar\\sanitize_me....exe
  auto last_slash = exe.find_last_of("/\\");
  if (last_slash != std::string::npos) {
    exe = exe.substr(last_slash + 1);
  }

  struct opts os = DFT_OPTS;
  std::vector<test_t> tests_to_run;

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
        "usage: " << exe << " [OPTS] TESTS\n"
        "where [OPTS]:\n"
        "  --list-tests          list all tests and exit 0\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are:\n"
        "          all       (runs all tests: will probably crash before running all)\n"
        "";
      for (auto [nm,_,__] : ALL_TESTS) {
        uss << "        | " << nm << "\n";
      }
      auto [first_nm,_,__] = ALL_TESTS[0];
      uss <<
        "EXAMPLES:\n"
        " % " << exe << " " << first_nm << "\n"
        "  runs this one test without sanitizer support\n"
        " % compute-sanitizer  --tool memcheck " << argv[0] << " " << first_nm << "\n"
        "  runs with sanitizer support\n"
        "";
      std::cout << uss.str();
      return EXIT_SUCCESS;
    } else if (arg == "--list-tests") {
      for (auto [tnm,_,__] : ALL_TESTS) {
        std::cout << tnm << "\n";
      }
      return EXIT_SUCCESS;
    } else if (arg == "-v0" || arg == "-v=0") {
      os.verbosity = 0;
    } else if (arg == "-v" || arg == "-v1" || arg == "-v=1") {
      os.verbosity = 1;
    } else if (arg == "-v2" || arg == "-v=2") {
      os.verbosity = 2;
    } else if (arg == "-v3" || arg == "-v=3") {
      os.verbosity = 3;
    } else if (arg == "all") {
      for (auto  t : ALL_TESTS) {
        tests_to_run.emplace_back(t);
      }
    } else if (arg.size() > 0 && arg[0] == '-') {
        bad_opt("invalid option");
    } else {
      bool found = false;
      for (auto [tnm,launcher,func] : ALL_TESTS) {
        if (tnm == arg) {
          tests_to_run.emplace_back(tnm, launcher, func);
          found = true;
          break;
        }
      }
      if (!found)
        fatal(arg, ": invalid test name (try -h)");
    }
  } // for

  if (tests_to_run.empty()) {
    fatal("expected test name (try -h)");
  }

  for (auto [tnm,launcher,func] : tests_to_run) {
    launcher(os, tnm, func);
  }

  return EXIT_SUCCESS;
}
