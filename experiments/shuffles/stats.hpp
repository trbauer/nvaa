#ifndef STATS_HPP
#define STATS_HPP

#include <algorithm>
#include <cstdint>
#include <functional>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
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


struct statistics {
  int64_t n = 0;
  int64_t sm = 0;
  double av = 0.0;
  double md = 0.0;
  double mn = 0.0, mx = 0.0;
  double va = 0.0;

  template <typename T>
  static statistics construct(const std::vector<T> &v) {
    return construct((const T *)v.data(), v.size());
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
    av = (double)sm / n;

    std::sort(vals.begin(), vals.end());
    if (n == 0) {
      md = av;
    } else if (n % 2) {
      md = vals[n / 2];
    } else {
      md = (vals[n / 2 - 1] + vals[n / 2]) / 2.0;
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
  std::string str() const {
    std::stringstream ss;
    str(ss);
    return ss.str();
  }
};

#endif // STATS_HPP
