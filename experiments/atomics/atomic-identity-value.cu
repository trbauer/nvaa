#include "mincu.hpp"

#include <array>
#include <bit>
#include <tuple>

using namespace mincu;

static const unsigned ITRS = 16;

extern "C"
__global__ void count_oups_hw(
    uint32_t *oups, const uint32_t *inps, uint32_t zero)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t inp = inps[gid];
  // hammer the atomic ITRS times with the same value
  for (unsigned i = 0; i < ITRS; i++) {
    auto inp_i = inp + i * zero;
    atomicAdd(&oups[gid + i * zero], inp_i);
  }
}

extern "C"
__global__ void count_oups_sw(
    uint32_t *oups, const uint32_t *inps, uint32_t zero)
{
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  const uint32_t inp = inps[gid];
  // hammer the atomic ITRS times with the same value
  for (unsigned i = 0; i < ITRS; i++) {
    auto inp_i = inp + i * zero;
    if (inp_i != 0) {
      atomicAdd(&oups[gid + i * zero], inp_i);
    }
  }
}


struct opts {
  int verbosity = 0;
  int iterations = 1;
  bool check = false;
  size_t blocks_per_grid = 1024;
  size_t threads_per_block = 256;

  bool normal() const {return verbosity >= 0;}
  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
  bool verbose_debug() const {return verbosity >= 3;}
};

constexpr opts DFT_OPTS;


static void referee_oups(
  const opts &os, uint32_t *oups, const uint32_t *inps, unsigned delta)
{
  size_t total_threads = os.blocks_per_grid * os.threads_per_block;

  int errs = 0;
  for (size_t gid = 0; gid < total_threads; gid++) {
    auto ref_oup = inps[gid] * ITRS;
    if (oups[gid] != ref_oup) {
      std::cerr << format("oups[", gid, "]: has # ",
          oups[gid], ", but we expected ", ref_oup, "\n");
      errs++;
    }
  }
  if (errs)
    fatal("mismatched");
}


using dispatch_wrapper =
    std::function<void (size_t,size_t,uint32_t *,const uint32_t *)>;


static void run_test(
    const opts &os,
    std::string tnm,
    std::function<unsigned(size_t)> init,
    dispatch_wrapper dispatch)
{
  if (os.debug()) {
    std::cout << "============= " << tnm << " starting\n";
  }

  umem<uint32_t> oups {os.blocks_per_grid * os.threads_per_block};
  const umem<uint32_t> inps {os.blocks_per_grid * os.threads_per_block, init};

  const double mb = (double)(inps.size() * sizeof(inps[0])) / 1024.0 / 1024.0;

  float min_t = 0.0f;
  for (int i = 0; i < os.iterations; i++) {
    oups.init(const_seq(0u));
    float t =
        time_dispatch_s(
          [&] {
            dispatch(os.blocks_per_grid, os.threads_per_block, oups, inps);
          });
    min_t = i == 0 ? t : std::min(min_t, t);
    if (os.debug()) {
      std::cout << "run[" << i << "]: " << frac(mb / min_t, 3) << "  MB/s\n";
    }
  }

  const double min_mb_s = mb / min_t;
  std::cout << coll<std::string>(tnm + ": ", 12) <<
      frac(min_mb_s, 3) << "  MB/s\n";

  if (os.verbose()) {
    std::cout << "  elems:  " << inps.size() << " elems\n";
    std::cout << "  mem:    " << frac(mb, 3) << " MB\n";
    std::cout << "  time:   " << frac(min_t, 5) << " s\n";
  }

  if (os.verbose_debug()) {
    std::cout << "INPS:\n";
    inps.str(std::cout, 8);
  }
  if (os.verbose_debug()) {
    std::cout << "OUPS:\n";
    oups.str(std::cout, 8);
  }

  if (os.check) {
    referee_oups(os, oups, inps, 0);
    if (os.normal()) {
      std::cout << "referee check passed\n";
    }
  }
}

using test = std::tuple<std::string,std::function<unsigned(size_t)>,dispatch_wrapper>;
static const test ALL_TESTS[] {
  {"hw-00-zeros",
      [] (size_t ix) {return 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-01-zeros",
      [] (size_t ix) {return ix % 32 == 0 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-08-zeros",
      [] (size_t ix) {return ix % 32 < 8 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-16-zeros",
      [] (size_t ix) {return ix % 32 < 16 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-24-zeros",
      [] (size_t ix) {return ix % 32 < 24 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-31-zeros",
      [] (size_t ix) {return ix % 32 < 31 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"hw-32-zeros",
      [] (size_t ix) {return 0;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_hw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-00-zeros",
      [] (size_t ix) {return 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-01-zeros",
      [] (size_t ix) {return ix % 32 == 0 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-08-zeros",
      [] (size_t ix) {return ix % 32 < 8 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-16-zeros",
      [] (size_t ix) {return ix % 32 < 16 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-24-zeros",
      [] (size_t ix) {return ix % 32 < 24 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-31-zeros",
      [] (size_t ix) {return ix % 32 < 31 ? 0 : 1;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
      }},
  {"sw-32-zeros",
      [] (size_t ix) {return 0;},
      [] (size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
        count_oups_sw<<<blocks_per_grid,block_size>>>(oups, inps, 0);
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
      std::stringstream tss;
      tss << "all";
      for (const auto &[tnm,_,__] : ALL_TESTS) {
        tss << " | " << tnm;
      }
      std::stringstream uss;
      uss <<
        "usage: atomic-identity-value.exe [OPTS] TESTS+\n"
        "where OPTS:\n"
        "  -bpg=INT              blocks per grid (defaults to " << DFT_OPTS.blocks_per_grid << ")\n"
        "  -tpb=INT              threads per blocks (defaults to " << DFT_OPTS.threads_per_block << ")\n"
        "  --check               referee the output on CPU\n"
        "  -i/--iterations=INT   number of runs to take the min of\n"
        "  -v/-v2/-v3            verbosity/debug\n"
        "and TESTS are: " << tss.str() << "\n"
        "EXAMPLES:\n"
        " % ... -bpg=1k all\n"
        "  generates and processes 1k blocks per grid on all tests\n"
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
    } else if (arg == "-v") {
      os.verbosity = 1;
    } else if (arg == "-v2") {
      os.verbosity = 2;
    } else if (arg == "-v3") {
      os.verbosity = 3;
    } else if (arg == "all") {
      for (const auto &t : ALL_TESTS) {
        tests.push_back(t);
      }
    } else {
      bool found = false;
      for (const auto &[tnm,init,disp] : ALL_TESTS) {
        if (tnm == arg) {
          tests.emplace_back(tnm, init, disp);
          found = true;
          break;
        }
      }
      if (!found) {
        bad_opt("invalid argument (try -h)");
      }
    }
  } // for

  if (tests.empty()) {
    fatal("expected test name (try -h)");
  }

  for (const auto &[tnm,init,disp] : tests) {
    run_test(os, tnm, init, disp);
  }

  return EXIT_SUCCESS;
}
