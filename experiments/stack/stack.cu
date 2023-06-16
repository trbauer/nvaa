#include "mincu.hpp"

#include <cuda_runtime_api.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <thread>
#include <tuple>
#include <vector>



template <typename T>
static void format_histogram_with(
  std::ostream &os,
  std::function<void(std::ostream&,const T &)> fmt_key,
  const std::map<T,size_t> &h,
  const char *units)
{
  size_t n = 0;
  for (const auto &e : h) {
    n += e.second;
  }
  os << "============= " << n << " samples in " << h.size() << " bins\n";
  auto fmtPct = [&](size_t k) {
    std::stringstream ss;
    double pct = (100.0 * k / n);
    ss << "%" << std::fixed << std::setprecision(3) << pct;
    return ss.str();
  };

  for (const auto &e : h) {
    std::stringstream ss;
    fmt_key(ss, e.first);
    os << "  " << std::setw(12) << ss.str();
    if (units)
      os << " " << units;
    os << "   " << std::setw(12) << e.second <<
      "   " << std::setw(7) << fmtPct(e.second) << "\n";
  }
}

template <typename T>
std::map<T,size_t> create_histogram(const T *samples, size_t n)
{
  std::map<T,size_t> h;
  for (size_t i = 0; i < n; i++) {
    h[samples[i]]++;
  }
  return h;
}

template <typename T>
static void format_histogram(
  std::ostream &os,
  std::function<void(std::ostream&,const T &)> fmt_key,
  const std::vector<T> &samples,
  const char *units = nullptr)
{
  auto h = create_histogram<T>(samples.data(), samples.size());
  format_histogram_with<T>(os, fmt_key, h, units);
}

template <typename T>
static void format_histogram(
  std::ostream &os,
  const std::vector<T> &samples,
  const char *units = nullptr)
{
  auto h = create_histogram<T>(samples.data(), samples.size());
  format_histogram_with<T>(os, [&](std::ostream &os, const T &t) {os << t;}, h, units);
}


using namespace mincu;

/*
static std::string fmtSmid(smid_t smid) {
  return "@" + std::to_string(smid);
}
template <typename T>
static std::string fmtKey(T t) {
    std::stringstream ss;
    ss << t << ":";
    return ss.str();
}
*/



struct statistics {
  int64_t n = 0;
  int64_t sm = 0;
  double av = 0.0;
  double md = 0.0;
  double mn = 0.0, mx = 0.0;
  double va = 0.0;

  template <typename T>
  static statistics construct(const umem<T> &oup)
  {
    return construct<T>(oup, oup.size());
  }
  template <typename T>
  static statistics construct(const T *oup, size_t _n)
  {
    statistics s;
    s.add_all(oup, _n);
    return s;
  }

  template <typename T>
  void add_all(const T *oup, size_t _n) {
    n = _n;
    if (n == 0)
      return;

    sm = 0;
    T _mx = oup[0], _mn = oup[0];
    //
    std::vector<T> vals;
    vals.reserve(n);
    //
    for (size_t i = 0; i < n; i++) {
      T e = oup[i];
      sm += e;
      vals.push_back(e);
      _mn = std::min<T>(_mn, e);
      _mx = std::max<T>(_mx, e);
    }
    mn = (double)_mn;
    mx = (double)_mx;
    av = (double)sm/n;

    std::sort(vals.begin(), vals.end());
    if (n == 0) {
      md = av;
    } else if (n % 2) {
      md = vals[n/2];
    } else {
      md = (vals[n/2 - 1] + vals[n/2])/2.0;
    }

    int64_t dvsm = 0;
    for (size_t i = 0; i < n; i++) {
      auto e = oup[i];
      dvsm += (e - av)*(e - av);
    }
    va = (double)dvsm/n;
  }

  /////////////////////////////////////
  // average
  double avg() const {return av;}
  // sum
  int64_t sum() const {return sm;}

  /////////////////////////////////////
  // ordering
  //
  // minimum
  double min() const {return mn;}
  // median
  double med() const {return md;}
  // maximum
  double max() const {return mx;}

  /////////////////////////////////////
  // spread
  // variance
  double var() const {return va;}
  // standard deviation
  double sdv() const {return sqrt(var());}
  // standard error of the mean
  double sem() const {return sdv()/sqrt((double)n);}
  // realtive standard error
  double rse() const {return sem()/avg();}

  /////////////////////////////////////
  void str(std::ostream &os) const {
    os << "statistics\n";
    os << " n: " << n << "\n";
    os << " min: " << min() << "\n";
    os << " med: " << med() << "\n";
    os << " avg: " << avg() << "\n";
    os << " max: " << max() << "\n";
    os << " rse: " << rse() << "\n";
  }
};

static int verbosity = 0;

extern "C" static __device__ int get_tid()
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  return id;
}
static const int LOADS_PER_THREAD = 256;

template <size_t B>
__global__ void test_stack_lat_device(int32_t *oup, uint64_t *times)
{
  static const size_t N = B / 4;
  static_assert(B % 4 == 0, "B must be multiple of 4");

  int32_t stack[N];

  for (int i = 0; i < N; i++) {
    stack[i] = i + threadIdx.x;
  }
  int idx = threadIdx.x;
  auto st = clock64();
  for (int i = 0; i < LOADS_PER_THREAD; i++) {
    idx = stack[idx % N];
  }
  auto en = clock64();

  oup[get_tid()] = idx;
  times[get_tid()] = en - st;
}

template <size_t B>
__global__ void test_stack_tpt_device(int32_t *oup)
{
  static const size_t N = B / 4;
  static_assert(B % 4 == 0, "B must be multiple of 4");

  int32_t stack[N];

  for (int i = 0; i < N; i++) {
    stack[i] = i + threadIdx.x;
  }
  int32_t sum = 0.0;
  for (int i = 0; i < LOADS_PER_THREAD; i++) {
    sum += stack[(threadIdx.x + i) % N];
  }

  oup[get_tid()] = sum;
}

template <size_t B>
static uint64_t test_stack_lat()
{
//  std::cout << "running test_stack_lat<" << B << ">()\n";
  static_assert(B % 4 == 0, "B must be multiple of 4");

  // https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__TYPES.html#group__CUDART__TYPES_1g4c4b34c054d383b0e9a63ab0ffc93651
  size_t limit = 0;
  auto e = cudaThreadGetLimit(&limit, cudaLimitStackSize);
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error in cudaThreadGetLimit");
  }
//  std::cout << "cudaLimitStackSize: " << limit << "\n";

  umem<int32_t> mem(1);
  umem<uint64_t> times(1);

  // warm up
  test_stack_lat_device<B><<<1,1>>>(mem, times);
  e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  test_stack_lat_device<B><<<1,1>>>(mem, times);
  e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  return times[0];
}

// return GB/s
template <size_t B>
static double test_stack_tpt()
{
  static_assert(B % 4 == 0, "B must be multiple of 4");
  umem<int32_t> mem(1);

  static const auto NBLOCKS = 1024;
  static const auto BLOCKSIZE = 32;

  // warm up
  test_stack_tpt_device<B><<<NBLOCKS,BLOCKSIZE>>>(mem);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
    // <<<NBLOCKS, BLOCKSIZE, SLM, STREAMINDEX >>>
  auto st = std::chrono::steady_clock::now();
  test_stack_tpt_device<B><<<NBLOCKS,BLOCKSIZE>>>(mem);
  e = cudaDeviceSynchronize();
  auto ed = std::chrono::steady_clock::now();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }

  // return std::chrono::duration_cast<std::chrono::microseconds>(ed - st).count();
  uint64_t total_bytes_accessed = BLOCKSIZE * NBLOCKS *
    ((uint64_t)4 * LOADS_PER_THREAD + B);
  double elapsed = std::chrono::duration<double>(ed - st).count();
  // double elapsed = std::chrono::duration_cast<std::chrono::seconds>(ed - st).count();
  if (verbosity >= 1)
    std::cout << "elapsed: " << elapsed << "\n";
  return (total_bytes_accessed / elapsed) / 1024.0 / 1024.0 / 1024.0;
}

static void test_stack_emit_header()
{
  std::cout <<
    std::setw(24) << "stack_size (B/lane)" <<
    " " <<
    std::setw(24) << "latency (c/read)"
    " " <<
    std::setw(24) << "tpt (GB/s)" <<
    "\n";
}
static void test_stack_emit_row(size_t B, uint64_t lat, double tpt)
{
  std::stringstream ss;
  ss <<
    std::setw(24) << B <<
    " " <<
    std::setw(24) << std::fixed << std::setprecision(1) << (lat / (double)LOADS_PER_THREAD) <<
    " " <<
    std::setw(24) << std::fixed << std::setprecision(1) << tpt <<
    "\n";
  std::cout << ss.str();
}

using lat_func = uint64_t (*)();
using tpt_func = double (*)();
using test_case = std::tuple<uint64_t,lat_func,tpt_func>;


template <size_t B>
static test_case mk_test_case() {
  return std::make_tuple(B, test_stack_lat<B>, test_stack_tpt<B>);
}
static test_case test_cases[] {
  mk_test_case<16>(),
  mk_test_case<32>(),
  mk_test_case<48>(),
  mk_test_case<64>(),
  mk_test_case<96>(),
  mk_test_case<128>(),
  mk_test_case<256>(),
  mk_test_case<512>(),
  mk_test_case<1*1024>(),
  mk_test_case<2*1024>(),
  mk_test_case<4*1024>(),
  mk_test_case<8*1024>(),
  mk_test_case<12*1024>(),
  mk_test_case<16*1024>(),
  mk_test_case<32*1024>(),
  mk_test_case<48*1024>(),
  mk_test_case<64*1024>(),
  mk_test_case<96*1024>(),
  mk_test_case<128*1024>(),
  mk_test_case<160*1024>(),
  mk_test_case<192*1024>(),
  mk_test_case<224*1024>(),
  mk_test_case<256*1024>(),
  mk_test_case<384*1024>(),
  mk_test_case<480*1024>(),
  mk_test_case<512*1024>(),
};

static void test_stack_all()
{
  test_stack_emit_header();
  for (auto p : test_cases) {
    auto N = std::get<0>(p);
    auto lat_func = std::get<1>(p);
    auto lat = lat_func();
    auto tpt_func = std::get<2>(p);
    auto tpt = tpt_func();
    test_stack_emit_row(N, lat, tpt);
  }
}

// cudaDeviceSetLimit
// https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__DEVICE.html#group__CUDART__DEVICE_1g05956f16eaa47ef3a4efee84563ccb7d

int main(int argc, const char* argv[])
{
  std::stringstream ss_cs;
  for (auto lc : test_cases) {
    auto B = std::get<0>(lc);
    std::stringstream ss_sz;
    if (B % (1024*1024) == 0) {
      ss_sz << B/1024/1024 << "M";
    } else if (B % 1024 == 0) {
      ss_sz << B/1024 << "K";
    } else {
      ss_sz << B;
    }
    ss_cs << "  case" << ss_sz.str() << "\n";
  }

  // test_timer_latency(0, block_counts);
  if ((argc != 2 && argc != 3) || (argc == 1 &&
    (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")))
  {
    std::cerr <<
      "usage: stack.exe [-v|-v2] TESTNAME\n"
      "where TESTNAME =\n"
      "  all - runs all tests\n"
      "  -- or --\n" <<
      ss_cs.str();
    return EXIT_FAILURE;
  }
  int arg_ix = 1;
  if (std::string(argv[1]) == "-v") {
    verbosity = 1;
    arg_ix = 2;
  } else if (std::string(argv[1]) == "-v2") {
    verbosity = 2;
    arg_ix = 2;
  }

  if (arg_ix >= argc) {
    fatal("usage: stack [-v|-v2|-h] TESTNAME\n");
  }
  std::string test_name = argv[arg_ix];
  if (test_name == "all") {
    test_stack_all();
  } else if ((test_name.substr(0, 4) == "case") &&
             std::isdigit(test_name[4])) {
    size_t scale = 1;
    auto sz_str = test_name.substr(4);
    if (sz_str[sz_str.size() - 1] == 'K') {
      scale = 1024;
      sz_str = sz_str.substr(0, sz_str.size() - 1);
    } else if (sz_str[sz_str.size() - 1] == 'M') {
      scale = 1024*1024;
      sz_str = sz_str.substr(0, sz_str.size() - 1);
    }
    const size_t B = std::atoi(sz_str.c_str()) * scale;
    if (B == 0) {
      fatal(test_name, ": malformed test");
    }
    const test_case *t = nullptr;
    for (const auto &p : test_cases) {
      if (std::get<0>(p) == B) {
        t = &p;
        break;
      }
    }
    if (t == nullptr) {
      fatal(test_name, ": cannot find test");
    }

    test_stack_emit_header();
    auto lat = (std::get<1>(*t))();
    auto tpt = (std::get<2>(*t))();
    test_stack_emit_row(std::get<0>(*t), lat, tpt);
  } else {
    fatal(test_name, ": unsupported test");
  }

  return EXIT_SUCCESS;
}


// also