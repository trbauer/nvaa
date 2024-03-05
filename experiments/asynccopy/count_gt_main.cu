#include <iostream>
#include <ostream>
#include <functional>

#include "mincu.hpp" // order matters :(
#include "count_gt.h"

using namespace mincu;

static const size_t BLOCK_SIZE = TPB;

static void referee_ngte(size_t blocks_per_grid, uint32_t *oups, const uint32_t *inps)
{
  int errs = 0;
  for (size_t gr_gid = 0;
       gr_gid < BLOCK_SIZE * blocks_per_grid;
       gr_gid += BLOCK_SIZE)
  {
    for (size_t tid = 0; tid < BLOCK_SIZE; tid++) {
      const size_t gid = gr_gid + tid;
      size_t ngreater_than = 0;
      for (int i = 0; i < EPT * TPB; i++) {
        if (inps[gr_gid + i] % (EPT * TPB) > tid) {
          ngreater_than++;
        }
      }
      if (ngreater_than != oups[gid]) {
        std::cerr << format("oup[", gid, "]: has #> to be ",
            oups[gid], ", but we expected ", ngreater_than, "\n");
        errs++;
      }
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
    dispatch_wrapper dispatch)
{
  if (os.debug()) {
    std::cout << "============= " << tnm << " starting\n";
  }

  umem<uint32_t> oups {os.blocks_per_grid * BLOCK_SIZE, const_seq(0u)};
  umem<uint32_t> inps {os.blocks_per_grid * EPT * BLOCK_SIZE, arith_seq(0u, 1u)};

  const double mb = (double)(inps.size() * sizeof(inps[0])) / 1024.0 / 1024.0;

  float min_t = 0.0f;
  for (int i = 0; i < os.iterations; i++) {
    oups.init(const_seq(0u));
    float t =
        time_dispatch_s([&] {dispatch(os.blocks_per_grid, BLOCK_SIZE, oups, inps);});
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
    referee_ngte(os.blocks_per_grid, oups, inps);
    if (os.normal()) {
      std::cout << "referee check passed\n";
    }
  }
}

static const std::pair<std::string,dispatch_wrapper> ALL_TESTS[] {
  {"async",
    [](size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
      count_gt_async<<<blocks_per_grid,BLOCK_SIZE>>>(oups, inps);
    }},
  {"sync",
    [](size_t blocks_per_grid, size_t block_size, uint32_t *oups, const uint32_t *inps) {
      count_gt_sync<<<blocks_per_grid,BLOCK_SIZE>>>(oups, inps);
    }},
};



static std::string usage()
{
  std::stringstream tss;
  tss << "all";
  for (const auto &[tnm,_] : ALL_TESTS) {
    tss << " | " << tnm;
  }
  std::stringstream uss;
  uss <<
    "usage: ngt_tile.exe OPTS TESTS\n"
    "where OPTS:\n"
    "  -bpg=INT              blocks per grid (defaults to " << DFT_OPTS.blocks_per_grid << ")\n"
    "  --check               referee the output on CPU\n"
    "  -i/--iterations=INT   number of runs to take the min of\n"
    "  -v/-v2/-v3            verbosity/debug\n"
    "and TESTS are: " << tss.str() << "\n"
    "EXAMPLES:\n"
    " % ngt_tile.exe -bpg=1k all\n"
    "  generates and processes 1k blocks per grid on all tests\n"
    "";
  return uss.str();
}


int main(int argc, const char* argv[])
{
  struct opts os;
  std::vector<std::pair<std::string,dispatch_wrapper>> tests;

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
      std::cout << usage();
      return EXIT_SUCCESS;
    } else if (arg == "--check") {
      os.check = true;
    } else if (key == "-bpg=") {
      os.blocks_per_grid = mincu::parse_positive_uint64(val, true, "int");
    } else if (key == "-i=" || key == "--iterations=") {
      os.iterations = (int)mincu::parse_positive_uint64(val, false, "int");
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
      for (const auto &[tnm,disp] : ALL_TESTS) {
        if (tnm == arg) {
          tests.emplace_back(tnm, disp);
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

  for (const auto &[tnm,disp] : tests) {
    run_test(os, tnm, disp);
  }

  return EXIT_SUCCESS;
}
