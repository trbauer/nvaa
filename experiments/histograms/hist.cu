#include <cuda_runtime.h>

// #include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx
#include <array>
#include <cstdio>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include "mincu.hpp"

using namespace mincu;

struct opts {
  int               iterations = 8;
  int               verbosity = 0;
  bool              check = false;

  bool verbose() const {return verbosity >= 1;}
  bool debug() const {return verbosity >= 2;}
};


// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#atomic-functions
// "The atomic functions described in this section have ordering cuda::memory_order_relaxed and are only atomic at a particular scope:"


// native atomic functions
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#atomic-functions

static __device__ __host__
unsigned classify(unsigned x, unsigned NBINS) {
  return x % NBINS;
}

// Classical reduction via shared memory.
template <unsigned NBINS>
__global__ void hist_satom_gatom(
  // cuda::atomic<unsigned,cuda::thread_scope_system> *g_hist,
  unsigned *g_hist,
  const unsigned *inps,
  unsigned per_wi)
{
  // static_assert((NBINS & (NBINS - 1)) == 0, "NBINS must be power of two");
  const unsigned gid = blockIdx.x * blockDim.x + threadIdx.x;
//  __shared__ cuda::atomic<unsigned,cuda::thread_scope_block> s_bins[NBLOCKS];
  __shared__ unsigned s_hist[NBINS];
  for (int bin_idx = threadIdx.x; bin_idx < NBINS; bin_idx += blockDim.x) {
    s_hist[bin_idx] = 0;
    // s_hist[i].store(0, cuda::memory_order_relaxed);
  }
  __syncthreads();
  // __threadfence_block();
  //   same as
  //  cuda::atomic_thread_fence(cuda::memory_order_seq_cst, cuda::thread_scope_block)

  for (unsigned i = 0; i < per_wi; i++) {
    auto datum = inps[gid * per_wi + i];
    auto bin_idx = classify(datum, NBINS);
    atomicAdd(s_hist + bin_idx, 1u);
    // s_bins[val].fetch_add(1, cuda::memory_order_relaxed);
  }

  __syncthreads();

  for (int bin_idx = threadIdx.x; bin_idx < NBINS; bin_idx += blockDim.x) {
    auto sval = s_hist[bin_idx];
    atomicAdd(g_hist + bin_idx, sval);
    // auto sval = s_bins[threadIdx.x].load(cuda::memory_order_relaxed);
    // g_hist[threadIdx.x].fetch_add(sval, cuda::memory_order_relaxed);
  }
}

// Thundering herd global atomic reduction
template <unsigned NBINS>
__global__ void hist_gatom(
  unsigned *g_hist,
  const unsigned *inps,
  unsigned per_wi)
{
  // static_assert((NBINS & (NBINS - 1)) == 0, "NBINS must be power of two");
  const unsigned gid = blockIdx.x * blockDim.x + threadIdx.x;

  for (unsigned i = 0; i < per_wi; i++) {
    auto datum = inps[gid * per_wi + i];
    auto bin_idx = classify(datum, NBINS);
    atomicAdd(g_hist + bin_idx, 1u);
  }
}

// Using local memory (terrible idea)
template <unsigned NBINS>
__global__ void hist_lgatom(
  unsigned *g_hist,
  const unsigned *inps,
  unsigned per_wi)
{
  const unsigned gid = blockIdx.x * blockDim.x + threadIdx.x;
  unsigned l_hist[NBINS] { };

  for (unsigned i = 0; i < per_wi; i++) {
    auto datum = inps[gid * per_wi + i];
    auto bin_idx = classify(datum, NBINS);
    l_hist[bin_idx]++;
    // atomicAdd(l_hist + bin_idx, 1); (VERY ILLEGAL)
  }

  // T __ldlu(const T* address);
  for (int bin_idx = 0; bin_idx < NBINS; bin_idx++) {
    // atomicAdd(g_hist + bin_idx, __ldlu(l_hist + bin_idx)); // creates LDG.E.LU!!!ma
    atomicAdd(g_hist + bin_idx, l_hist[bin_idx]);
  }
}





struct result {
  const double value;
  const std::string error;

  result(std::string s) : error(s), value(0.0) { }
  result(double v) : value(v) { }

  bool is_error() const {return !error.empty();}
};

using call_kernel_wrapper =
    std::function<void(size_t,size_t,unsigned *,const unsigned *,unsigned)>;

static result run_test(
  const opts &os,
  const unsigned nbins,
  const size_t blocks_per_grid,
  const size_t thrs_per_block,
  const size_t data_per_thr,
  const call_kernel_wrapper &call_kernel,
  std::stringstream &vss)
{
  umem<unsigned> oups {(size_t)nbins, const_seq(0u)};

  const size_t buffer_elems = blocks_per_grid * thrs_per_block * data_per_thr;
  vss << "buffer is " << frac(sizeof(unsigned)*buffer_elems/1024.0/1024.0, 3) << " MB\n";
  if (os.debug()) {
    vss << "blocks_per_grid = " << blocks_per_grid << "; "
           "threads_per_block = " << thrs_per_block << "; "
           "data_per_thr = " << data_per_thr <<"\n";
  }

  random_state rs {2024u};
  const umem<unsigned> inps {buffer_elems, rnd_seq<unsigned>(rs)};
#ifndef _WIN32
  inps.prefetch_to_device();
#endif

  float min_ms = std::numeric_limits<float>::infinity();
  for (int i = 0; i < os.iterations; i++) {
    oups.init(const_seq(0u));
    // std::cerr << "\n: run[" << i << "]: oups is\n"; oups.str(std::cerr);
#ifndef _WIN32
    oups.prefetch_to_device();
#endif
    float ms =
      time_dispatch_ms(
          [&] {
            call_kernel(blocks_per_grid,
                        thrs_per_block,
                        oups,
                        inps,
                        (unsigned)data_per_thr);
          });
    vss << "run[" << i << "]: " << frac(ms / 1000.0f, 6) << " s\n";
    min_ms = std::min(min_ms, ms);
  }

  const double min_s = min_ms / 1000.0f;
  if (os.debug()) {
    for (size_t i = 0; i < oups.size(); i += 8) {
      for (size_t c = 0; c < 8 && i + c < oups.size(); c++) {
        size_t n = oups[i + c];
        double pct = 100.0 * n / (double)buffer_elems;
        vss << colr<unsigned>(n, 12) << colr<std::string>(format("(", frac(pct,2), "%)"), 12);
      }
      vss << "\n";
    }
  }
  if (os.check) {
    std::vector<unsigned> ref_bins;
    ref_bins.resize(nbins, 0);
    for (size_t i = 0; i < inps.size(); i++) {
      ref_bins[classify(inps[i], nbins)]++;
    }
    bool mismatched = false;
    for (unsigned i = 0; i < nbins; i++) {
      if (ref_bins[i] != oups[i]) {
        vss << "bin[" << i << "] = " << oups[i] << " "
               "mismatches reference " << ref_bins[i] << "\n";
        mismatched = true;
      }
    }
    if (mismatched)
      return result("mismatches reference");
  }
  double mb = sizeof(inps[0]) * buffer_elems / 1024.0 / 1024.0;
  return (mb / min_s);
}


const static std::tuple<std::string,unsigned,std::string,call_kernel_wrapper> ALL_TESTS[] {
#define MAKE_GATOM(NBINS) \
  {"gatom-h" #NBINS, \
   (unsigned)(NBINS), \
   #NBINS "-bin atomic reduction directly to global memory bins", \
   [](size_t bpg, size_t tpb, unsigned *oups, const unsigned *inps, unsigned dpt) -> void { \
     hist_gatom<NBINS><<<bpg,tpb>>>(oups, inps, dpt); \
   }}
/*
  MAKE_GATOM(2),
  MAKE_GATOM(4),
  MAKE_GATOM(8),
*/
  MAKE_GATOM(16),
/*
  MAKE_GATOM(32),
  MAKE_GATOM(48),
  MAKE_GATOM(64),
  MAKE_GATOM(96),
  MAKE_GATOM(128),
  MAKE_GATOM(192),
*/
  MAKE_GATOM(256),
/*
  MAKE_GATOM(320),
  MAKE_GATOM(384),
  MAKE_GATOM(448),
  MAKE_GATOM(512),
*/
#define MAKE_SGATOM(NBINS) \
  {"sgatom-h" #NBINS, \
   (unsigned)(NBINS), \
   #NBINS "-bin atomic reduction in shared memory bins then out to global memory bins", \
   [](size_t bpg, size_t tpb, unsigned *oups, const unsigned *inps, unsigned dpt) -> void { \
     hist_satom_gatom<NBINS><<<bpg,tpb>>>(oups, inps, dpt); \
   }}
//  MAKE_SGATOM(2),
//  MAKE_SGATOM(4),
//  MAKE_SGATOM(8),
  MAKE_SGATOM(16),
//  MAKE_SGATOM(32),
//  MAKE_SGATOM(48),
//  MAKE_SGATOM(64),
//  MAKE_SGATOM(96),
//  MAKE_SGATOM(128),
//  MAKE_SGATOM(320),
  MAKE_SGATOM(256),
//  MAKE_SGATOM(384),
//  MAKE_SGATOM(448),
//  MAKE_SGATOM(512),
#define MAKE_LGATOM(NBINS) \
  {"lgatom-h" #NBINS, \
   (unsigned)(NBINS), \
   #NBINS "-bin reduction in local memory bins then out to global memory bins", \
   [](size_t bpg, size_t tpb, unsigned *oups, const unsigned *inps, unsigned dpt) -> void { \
     hist_lgatom<NBINS><<<bpg,tpb>>>(oups, inps, dpt); \
   }}
  MAKE_LGATOM(2),
  MAKE_LGATOM(4),
  MAKE_LGATOM(8),
  MAKE_LGATOM(16),
  MAKE_LGATOM(32),
  MAKE_LGATOM(48),
  MAKE_LGATOM(64),
  MAKE_LGATOM(96),
  MAKE_LGATOM(128),
  MAKE_LGATOM(256),
  MAKE_LGATOM(320),
  MAKE_LGATOM(384),
  MAKE_LGATOM(448),
  MAKE_LGATOM(512),
};





int main(int argc, const char **argv)
{
  static const size_t DEFAULT_BPG = 128;
  static const size_t DEFAULT_TPB = 1024;
  static const size_t DEFAULT_DPT = 64;

  opts os;
  std::vector<std::tuple<std::string,unsigned,call_kernel_wrapper,size_t,size_t,size_t>> tests;

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
      std::stringstream ss;
      ss << "       all -- runs all tests under a given configuration\n";
      for (auto [tnm,_,tdesc,__] : ALL_TESTS) {
        ss << "     | " << tnm << "  -- " << tdesc << "\n";
      }
      const auto eg_test = std::get<0>(ALL_TESTS[0]);
      std::cout <<
        "usage: hist.exe OPTS CONFIG+\n"
        "where\n"
        " CONFIG = KERNEL(:BPG,TPB,DPT)?\n"
        "   KERNEL =\n" <<
        ss.str() <<
        "   BPG - blocks per grid - defaults to " << DEFAULT_BPG << "\n"
        "   TPB - threads per block - defaults to " << DEFAULT_TPB << "\n"
        "   DPT - data per thread - defaults to " << DEFAULT_DPT << "\n"
        "and where OPTS are:\n"
        "  -e/--check-errors       checks errors\n"
        "  -i/--iterations=INT     number of iterations to run per test\n"
        "  -v/-v2                  sets verbosity\n"
        "EXAMPLES:\n"
        " % hist " << eg_test << "\n"
        "  runs " << eg_test << " with default parameters\n"
        " % hist " << eg_test << ":256,1k,16\n"
        "  runs " << eg_test << " with 256 blocks, 1024 threads per block, and 16 per data per thread\n";
      return EXIT_SUCCESS;
    } else if (key == "-i=" || key == "--iterations=") {
      os.iterations = (int)mincu::parse_positive_uint64(val, false, "iterations");
    } else if (arg == "-e" || arg == "--check-errors") {
      os.check = true;
    } else if (arg == "-v") {
      os.verbosity = 1;
    } else if (arg == "-v2") {
      os.verbosity = 2;
    } else if (arg == "default") {
      bad_opt("\"default\" test set not setup yet");
    } else {
      auto col = arg.find(':');
      std::string test_name;
      std::string sfx;
      if (col != std::string::npos) {
        test_name = arg.substr(0, col);
        sfx = arg.substr(col + 1);
      } else {
        test_name = arg;
      }
      size_t bpg = DEFAULT_BPG, tpb = DEFAULT_TPB, dpt = DEFAULT_DPT;
      if (!sfx.empty()) {
        auto comm0 = sfx.find(',');
        if (comm0 == std::string::npos) {
          bad_opt(sfx, ": malformed config suffix (expected comma)");
        }
        auto comm1 = sfx.find(',', comm0 + 1);
        if (comm1 == std::string::npos) {
          bad_opt(sfx, ": malformed config suffix (expected second comma)");
        }
        bpg = parse_positive_uint64(sfx.substr(0, comm0), true, "bpg");
        tpb = parse_positive_uint64(sfx.substr(comm0 + 1, comm1 - comm0 - 1), true, "tpb");
        dpt = parse_positive_uint64(sfx.substr(comm1 + 1), true, "dpt");
      }

      if (test_name == "all") {
        for (auto [tnm,tbins,desc,func] : ALL_TESTS) {
          tests.emplace_back(tnm, tbins, func, bpg, tpb, dpt);
        }
      } else {
        bool found_test = false;
        for (auto [tnm,tbins,desc,func] : ALL_TESTS) {
          if (tnm == test_name) {
            found_test = true;
            tests.emplace_back(tnm, tbins, func, bpg, tpb, dpt);
            break;
          }
        }
        if (!found_test) {
          bad_opt("unrecognized test (-h lists tests)");
        }
      }
    } // else
  } // for
  if (tests.empty()) {
    fatal("expected test config (try -h)");
  }

  int e = EXIT_SUCCESS;
  std::cout <<
      coll<std::string>("Test", 32) << " " <<
      colr<std::string>("InputSize(MB)", 16) << " " <<
      colr<std::string>("Rate(MB/s)", 16) << "\n";
  for (auto [tnm,nbins,func,bpt,tpb,dpt] : tests) {
    std::stringstream vss;
    auto nm = format(tnm, "<", bpt, ",", tpb, ",", dpt, ">");
    auto sz = sizeof(unsigned) * bpt * tpb * dpt;
    std::cout <<
      coll<std::string>(nm, 32) << " " <<
      colr<std::string>(format(frac(sz / 1024.0 / 1024.0, 3)), 16) << " ";
    auto r = run_test(os, nbins, bpt, tpb, dpt, func, vss);
    std::string rcol;
    if (r.is_error()) {
      rcol = format("ERROR(", r.error, ")");
      e = EXIT_FAILURE;
    } else {
      rcol = format(frac(r.value, 3));
    }
    std::cout << colr<std::string>(rcol, 16);
    std::cout << "\n";
    if (os.verbose() || r.is_error()) {
      std::cout << vss.str();
    }
  }
  return e;
}
