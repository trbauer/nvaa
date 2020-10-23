#include "mincu.hpp"
#include "timers.cuh"

#include <cuda_runtime_api.h>

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <thread>
#include <vector>


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
    std::vector<T> ord(n);
    for (size_t i = 0; i < n; i++) {
      auto e = oup[i];
      sm += e;
      ord.push_back(e);
      _mn = std::min<T>(_mn, e);
      _mx = std::max<T>(_mx, e);
    }
    mn = (double)_mn;
    mx = (double)_mx;
    av = (double)sm/n;

    std::sort(ord.begin(), ord.end());
    if (n % 2) {
      md = ord[n/2];
    } else {
      md = (ord[n/2 - 1] + ord[n/2])/2.0;
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
};


// %globaltimer
//   https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-globaltimer

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-warpid
//   %warpid  %nwarpid

// %smid and %nsmid  (may not be contiguous)
// %gridid

// __nanosleep
// The sleep duration is approximated, but guaranteed to be in the
// interval [0, 2*t].  The implementation may reduce the sleep duration for
// individual threads within a warp such that all sleeping threads in the
// warp wake up together.


using block_dist_map_t = std::map<smid_t,std::vector<std::pair<block_id_t,ticks_t>>>;



// dispatched block id with value
struct dbiv {
  dbi       id;
  ticks_t   value;

  dbiv(block_id_t bid, smid_t smid, ticks_t v) : id(bid, smid), value(v) { }
  dbiv(uint32_t idbits, ticks_t v) : id(idbits), value(v) { }
};

struct rec {
  static const int BLOCK_SIZE_COLUMN = 8;
  static const int DATA_COLUMN = 12;

  int                   blocks;
  std::vector<dbiv>     elems;

  rec(int _blocks,
      const umem<uint32_t> &dbids_buf,
      const umem<ticks_t> *times_buf = nullptr)
      : blocks(_blocks)
  {
    elems.reserve(blocks);
    for (size_t i = 0; i < blocks; i++) {
      auto idbits = dbids_buf[i];
      if (times_buf) {
        auto t = (*times_buf)[i];
        elems.emplace_back(idbits, t);
      } else {
        elems.emplace_back(idbits, 0);
      }
    }
  }
  rec(int _blocks,
      const umem<uint32_t> &ids_buf,
      const umem<ticks_t> &times_buf) : rec(_blocks, ids_buf, &times_buf) { }

  block_dist_map_t block_distribution() const {
    block_dist_map_t m;
    for (dbiv d : elems) {
      // const auto &elems = m[d.id.sm];
      // std::vector<std::pair<block_id_t,ticks_t>> &es = m[d.id.sm];
      block_dist_map_t::mapped_type &es = m[d.id.sm];
      // auto &es = m[d.id.sm];
      es.push_back(std::pair<block_id_t,ticks_t>(d.id.block, d.value));
    }
    return m;
  }
  void format_block_distribution(std::ostream &os) const {
    const auto m = block_distribution();
    // for (const std::pair<smid_t,std::vector<std::pair<block_id_t,ticks_t>>> &bin : m) {
    for (const auto &bin : m) {
      auto smid = bin.first;
      const block_dist_map_t::mapped_type &elems = bin.second;
      os << std::setw(BLOCK_SIZE_COLUMN) << "@" + std::to_string(smid) + ":";
      for (const auto &val : elems) {
        os << "  " << std::setw(5) << std::right << "#" + std::to_string(val.first);
        // os << " = " << std::setw(12) << std::right << val.second;
      }
      os << "\n";
    }
  }

  void format_block_distribution_json(std::ostream &os) const {
    bool first = true;
    os << "[";

    const auto m = block_distribution();
    for (const auto &bin : m) {
      auto smid = bin.first;
      const block_dist_map_t::mapped_type &elems = bin.second;
      for (const auto &val : elems) {
        if (first) {
          first = false;
        } else {
          os << "\n,";
        }
        auto block_id = val.first;
        auto ts = val.second;
        os << "{\"pid\":" << smid << ", \"ph\":\"I\", \"name\":\"#" << block_id << "\", \"ts\":" << ts << "}";
      }
    }

    os << "]";
  }

  void str(std::ostream &os) const {
    if (!elems.empty()) {
      os << std::setw(BLOCK_SIZE_COLUMN) << (std::to_string(blocks) + ":");
      for (size_t i = 0; i < elems.size(); i++) {
        os << "  " << std::setw(DATA_COLUMN) << elems[i].value;
      }
      os << "\n";
    }

    os << std::setw(BLOCK_SIZE_COLUMN) << (std::to_string(blocks) + ":");
    for (size_t i = 0; i < elems.size(); i++) {
      std::stringstream ss;
      ss << elems[i].id.block << "@" << elems[i].id.sm;
      os << "  " << std::setw(DATA_COLUMN) << ss.str();
    }
    os << "\n";
  }
};



extern "C" __global__ void glob_get_nsmid(int *nsmid);

static int query_nsmid()
{
  umem<int> nsmid_buf(1);
  glob_get_nsmid<<<1,1>>>(nsmid_buf);
  return nsmid_buf[0];
}



extern "C" __global__ void glob_block_dist(int64_t *times, uint32_t *ids, long delay);


#define ARRLEN(A) (sizeof(block_counts)/sizeof(block_counts[0]))

static void test_block_dist()
{
  static const int block_counts[] {30, 60, 120, 1024};
  const int max_block_count = block_counts[ARRLEN(block_counts) - 1];
  // static const int block_counts[] {4096};
  // static const int block_counts[] {3, 10, 100};
  // const std::array<int,3> block_counts {1, 10, 100, 1000};
  const long delay = 10*1000; // 10 us

  const int nsmid = query_nsmid();
  std::cout << "  %nsmid = " << nsmid << "\n";

  umem<ticks_t> tbuf(max_block_count);
  umem<uint32_t> idbuf(max_block_count);

  for (int block_count : block_counts) {
    std::cout << "============================================================\n";
    std::cout << block_count << " blocks\n";
    glob_block_dist<<<block_count,1>>>(tbuf, idbuf, delay); // warmup
    glob_block_dist<<<block_count,1>>>(tbuf, idbuf, delay);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e), " (" ,cudaGetErrorString(e), "): unexpected error");
    }
    const rec r(block_count, idbuf, tbuf);

    r.format_block_distribution(std::cout);
    std::cout << "\n";

    std::ofstream of("events.json");
    r.format_block_distribution_json(of);
    // idbuf.str(std::cout, 8);
    // tbuf.str(std::cout, 4);

    std::cout << "\n";
  }
}

std::ostream &operator<<(std::ostream &os, const tstamps &ts) {
  os <<
    "{"
    "smid:" << ts.smid << ","
    "block_idx:" << ts.block_idx << ","
    "ticks:" << ts.ticks << ","
    "globaltimer:" << ts.globaltimer <<
    "}";
  return os;
}

//////////////////////////////////////////////////////////////////
// are timers persistant over dispatches?
// TEST: fetch time stamps, delay K, re-fetch; diff by SM
extern "C" __global__ void glob_get_times(tstamps *tss);


static void test_persist_dispatch()
{
  const int WARP_COUNT = 30;

  auto dispatch = [&](umem<tstamps> &tss) {
    glob_get_times<<<WARP_COUNT,1>>>(tss);
    auto e = cudaDeviceSynchronize();
    if (e != cudaSuccess) {
      fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
    }
  };

  const int DISPATCHES = 10;
  std::vector<std::vector<tstamps>> tss;
  tss.reserve(DISPATCHES);
  // tss.emplace_back(WARP_COUNT);
  for (int i = 0; i < DISPATCHES; i++) {
    umem<tstamps> ts_buf(WARP_COUNT);
    if (i != 0) {
      // std::this_thread::sleep_for(2s);
      const int S = 1; // 60;
      std::cout << "waiting " << S << "s...";
      std::cout.flush();
      std::this_thread::sleep_for(std::chrono::seconds(S));
      std::cout << "\n";
    }
    std::cout << "dispatch" << i << "\n";

    dispatch(ts_buf);

    tss.emplace_back((std::vector<tstamps>)ts_buf);
  }

  auto findStamp = [&](smid_t smid, const std::vector<tstamps> &tss) -> const tstamps * {
    for (const auto &ts : tss) {
      if (ts.smid == smid) {
        return &ts;
      }
    }
    return nullptr;
  };

  const int KEY_COLS = 32;
  const int VAL_COLS = 12;
  auto emitStampTicks = [&](smid_t smid, bool globaltimer = true)
  {
    std::cout << std::setw(KEY_COLS) <<
      ("%smid=" + std::to_string(smid) + "/" + (globaltimer ? "%globaltimer" : "clock64()"));


    auto fmtDelta = [&](const tstamps *ts0, const tstamps *ts1) {
      std::cout << "  ";
      if (ts0 == nullptr || ts1 == nullptr) {
        std::cout << std::setw(VAL_COLS) << "???";
      } else {
        if (globaltimer) {
          std::cout << std::setw(VAL_COLS) << (ts1->globaltimer - ts0->globaltimer);
        } else {
          std::cout << std::setw(VAL_COLS) << (ts1->ticks - ts0->ticks);
        }
      }
    };

    for (int i = 1; i < DISPATCHES; i++) {
      const tstamps *ts0 = findStamp(smid, tss[i-1]);
      const tstamps *ts1 = findStamp(smid, tss[i-0]);
      fmtDelta(ts0, ts1);
    }
    std::cout << "\n";
  };

//  for (size_t i = 0; i < tss_buf0.size(); i++) {
//    std::cout << "==> " << tss_buf0[i] << "\n";
//  }

  auto fmtHeader = [&]() {
    std::cout << std::setw(KEY_COLS) << "deltas";
    for (int i = 0; i < DISPATCHES - 1; i++) {
      std::cout << "  " << std::setw(VAL_COLS) <<
        ("d" + std::to_string(i + 1) + "-d" + std::to_string(i));
    }
    std::cout << "\n";
  };

  // FIXME: assumes virtual SM ids are [0..WARP_COUNT-1] and persist to physical across calls
  std::cout << "============clock64()==========================================\n";
  fmtHeader();
  for (smid_t id = 0; id < WARP_COUNT; id++) {
    emitStampTicks(id, false);
  }
  std::cout << "\n";
  std::cout << "============%globaltimer=======================================\n";
  fmtHeader();
  for (smid_t id = 0; id < WARP_COUNT; id++) {
    emitStampTicks(id, true);
  }
}

static void test_persist_dispatch_proc()
{
  umem<tstamps> ts(1);
  glob_get_times<<<1,1>>>(ts);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  std::cout << "%globaltimer: " << std::setw(18) << ts[0].globaltimer << "\n";
}

static void test_delayed_clock64(int s)
{
  // stall, then init and call clock64
  // we should see about the same satartup time
  if (s > 0)
    std::this_thread::sleep_for(std::chrono::seconds(s));

  umem<tstamps> ts(1);
  glob_get_times<<<1,1>>>(ts);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  std::cout << "clock64(): " << std::setw(18) << ts[0].ticks << "\n";
}

static void test_print_clocks(int startup_delay)
{
  // stall, then init and call clock64
  // we should see about the same satartup time
  if (startup_delay > 0)
    std::this_thread::sleep_for(std::chrono::seconds(startup_delay));

  umem<tstamps> ts(1);
  glob_get_times<<<1,1>>>(ts);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  std::cout << "%globaltimer|%clock64:  " <<
    std::setw(18) << ts[0].globaltimer <<
    "  |  " <<
    std::setw(12) << ts[0].ticks <<
    "\n";
}

extern "C" __global__ void glob_globaltimer_cost(int *cost, int count, uint64_t *sum);
static void test_globaltimer_cost()
{
  const int WARP_COUNT = 30;
  const int LOOP_TRIPS = 32;

  umem<uint64_t> dummy(1);
  umem<int> deltas_buf(WARP_COUNT);

  glob_globaltimer_cost<<<WARP_COUNT,1>>>(deltas_buf, LOOP_TRIPS, dummy);

  auto st = statistics::construct<int>(deltas_buf);
  std::cout << "%globaltimer: takes " <<
    std::fixed << std::setprecision(3) << st.avg() <<
    " clocks on average\n";
}

// emit JSON
// https://www.gamasutra.com/view/news/176420/Indepth_Using_Chrometracing_to_view_your_inline_profiling_data.php
//
// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/edit
//  ph=event type {B=begin,E=end,X=complete evetn,C=counter event,...,P=sample event}
//  ts=time, tts=thread clock timestamp
//  pid=process id
//  tid=thread id
//  args=[extra arguments]
//  {name:"warp start", }
// {"name": "myFunction", "cat": "foo", "ph": "X", "ts": 123, "dur": 234, "pid": 2343, "tid": 2347,
// "args": {
//   "first": 1
// }


/*
extern "C" __global__ void glob_block_dist_atomic(
  int64_t *times, uint32_t *ids, uint32_t *index, long delay);

static void test_block_dist_atomics()
{
}

extern "C" __global__ void glob_init(int64_t *times, uint32_t *ids, long delay);
extern "C" __global__ void glob_nanosleep(int64_t *times, uint32_t *ids, long delay);
extern "C" __global__ void glob_clock_value(int64_t *times, uint32_t *ids);

static void test_timer_latency()
{
  long delays[] = {0, 1000};
  const int block_counts[] {1, 10, 20};
  const int max_block_count = block_counts[ARRLEN(block_counts) - 1];

  std::vector<rec> records;

  umem<ticks_t> tbuf(max_block_count);
  umem<uint32_t> idbuf(max_block_count);
  for (long delay : delays) {
    // warmup
    for (int block_count : block_counts) {
  //    tbuf.prefetch_to_device();
  //    idbuf.prefetch_to_device();
      if (delay) {
        glob_nanosleep<<<block_count,1>>>(tbuf, idbuf, delay); // warmup
        glob_nanosleep<<<block_count,1>>>(tbuf, idbuf, delay);
      } else {
        glob_init<<<block_count,1>>>(tbuf, idbuf, delay); // warmup
        glob_init<<<block_count,1>>>(tbuf, idbuf, delay);
      }
      auto e = cudaDeviceSynchronize();
      if (e != cudaSuccess) {
        fatal(cudaGetErrorName(e), " (" ,cudaGetErrorString(e), "): unexpected error");
      }
      records.emplace_back(block_count, tbuf, idbuf);
      records.back().str(std::cout);
    }
  }
}
*/

int main(int argc, const char* argv[])
{
  // test_timer_latency(0, block_counts);
  if ((argc != 2 && argc != 3) || (argc == 1 &&
    (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")))
  {
    std::cerr <<
      "usage: timers.exe TESTNAME\n"
      "where TESTNAME =\n"
      "    clocks [INT]\n"
      "  | delayed-clock64  INT\n"
      "  | dist\n"
      "  | globaltimer-cost\n"
      "  | persist\n"
      "  | print-timers\n"
      ;
    return EXIT_FAILURE;
  }
  int arg2 = 0;
  if (argc == 3) {
    size_t pos = 0;
    arg2 = std::stoi(argv[2], &pos, 10);
    if (pos == 0) {
      fatal(argv[2], "malformed integer argument");
    }
  }
  std::string test_name = argv[1];
  if (test_name == "clocks") {
    if (argc != 3) {
      arg2 = 0;
    }
    test_print_clocks(arg2);
  } else if (test_name == "delayed-clock64") {
    if (argc != 3) {
      std::cerr << "test requires INT argument (seconds)\n";
      return EXIT_FAILURE;
    }
    test_delayed_clock64(arg2);
  } else if (test_name == "dist") {
    test_block_dist();
  } else if (test_name == "globaltimer-cost") {
    test_globaltimer_cost();
  } else if (test_name == "persist") {
    test_persist_dispatch();
  } else if (test_name == "print-timers") {
    test_persist_dispatch_proc();
  } else {
    fatal(test_name, ": unsupported test");
  }

  return EXIT_SUCCESS;
}


// also