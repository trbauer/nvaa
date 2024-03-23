#ifndef MINCU_HPP
#define MINCU_HPP

// #if __cplusplus < 201703L
// #error "Need at least -std=c++17"
// #endif
#ifdef _WIN32
#define NOMINMAX
#include <Windows.h>
#undef NOMINMAX
#endif

#include <cmath>
#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <optional>
#include <ostream>
#include <random>
#include <sstream>
#include <string>
#include <type_traits>

// CUDA includes
//   cuda.h - driver API
//   cuda_runtime_api.h - public host functions
//   cuda_runtime.h - includes the above, function overlays, and device intrinsics
// SOURCE: https://stackoverflow.com/questions/6302695/difference-between-cuda-h-cuda-runtime-h-cuda-runtime-api-h
//
//   "If you are writing code which will be compiled using nvcc, it is all irrelevant,
//    because nvcc takes care of inclusion of all the required headers automatically
//    without programmer intervention."
//
// should probably be this (OR none!)
#include <cuda_runtime.h>
// #include <cuda_runtime_api.h>

namespace mincu
{
template <typename T>
__device__ __host__
static inline T align_up(T n, T a) {
    return (n + a - 1) - ((n + a - 1) % a);
}

///////////////////////////////////////////////////////////////////////////////
// text formatting
template <typename...Ts>
static void format_to(std::ostream &os) { }
template <typename T, typename...Ts>
static void format_to(std::ostream &os, T t, Ts...ts);
template <typename...Ts>
static std::string format(Ts...ts) {
  std::stringstream ss; format_to(ss, ts...); return ss.str();
}

struct hex
{
  uint64_t value;
  int columns;
  template <typename T>
  hex(T v, int cls = 2 * sizeof(T)) : value((uint64_t)v), columns(cls) { }
};
static inline std::ostream &operator <<(std::ostream &os, hex h) {
  std::stringstream ss;
  ss << std::setw(h.columns) <<
    std::setfill('0') << std::hex << std::uppercase << h.value;
  os << ss.str();
  return os;
}

template <typename T>
static inline void fmt_hex_digits(std::ostream &os, T t, int digs = -1) {
  if (digs < 0)
    digs = 2 * sizeof(T);
  if (sizeof(T) == 1) {
    os << std::uppercase << std::setw(digs) << std::setfill('0') << std::hex
       << ((uint16_t)t & 0xFF);
  } else {
    os << std::uppercase << std::setw(digs) << std::setfill('0') << std::hex
       << t;
  }
}
template <typename T>
static inline std::string fmt_hex_digits(T t, int digs = -1) {

  std::stringstream ss;
  fmt_hex_digits(ss, t, digs);
  return ss.str();
}

// pads out to the size of the underlying type filling
// '0' and then ' ' after that
// E.g. fmt_hex<uint8_t>(..., 5) => " 0x05"
template <typename T>
static inline void fmt_hex(std::ostream &os, T t, int digs = -1) {
  os << "0x"; fmt_hex_digits(os, t, digs);
}
template <typename T>
static inline std::string fmt_hex(T t, int digs = -1) {
  std::stringstream ss;
  fmt_hex(ss, t, digs);
  return ss.str();
}
template <typename T>
static inline std::string fmt_dec(T t, int cw = -1) {
  if (sizeof(t) == 1) {
    if constexpr (std::is_signed<T>()) {
      return fmt_dec((int16_t)t, cw); // avoid alphachar
    } else if constexpr (std::is_floating_point<T>()) {
      static_assert("whoops: fmt_dec called on a floating point type");
    } else {
      return fmt_dec((uint16_t)t, cw); // avoid alphachar
    }
  }
  std::stringstream ss;
  if (cw > 0) {
    ss << std::setw(cw) << std::dec << t;
  } else {
    ss << std::dec << t;
  }
  return ss.str();
}

///////////////////////////////////////////////////////////////////////////////
enum class pad {L, R};

template <pad P, typename T>
struct col
{
  const T &value;
  int width;
  char pad_fill;
  col(const T &val, int wid, char f = ' ')
    : value(val), width(wid), pad_fill(f) { }
}; // col
template <typename T>
using coll = col<pad::R,T>;
template <typename T>
using colr = col<pad::L,T>;

template <pad P,typename T>
static inline std::ostream &operator<<(std::ostream &os, const col<P,T> &p) {
  auto s = format(p.value);
  if (p.width <= 0) {
    os << s;
    return os;
  }
  std::stringstream ss;
  if (P == pad::L) {
    for (size_t i = s.size(); i < (size_t)p.width; i++)
      ss << p.pad_fill;
  }
  ss << s;
  if (P == pad::R) {
    for (size_t i = s.size(); i < (size_t)p.width; i++)
      ss << p.pad_fill;
  }
  os << ss.str();
  return os;
}

template <typename...Ts>
static void fatal(Ts...ts) {
  // format_to(std::cerr, ts...); std::cerr << "\n";
  std::cerr << format(ts...) << "\n";
  exit(EXIT_FAILURE);
}

///////////////////////////////////////////////////////////////////////////////
// CUDA API wrapper

#define CUDA_API(__CUDA_API__,...) \
  do { \
    auto __cuda_api_err = __CUDA_API__(__VA_ARGS__); \
    if (__cuda_api_err != cudaSuccess) { \
      mincu::fatal(#__CUDA_API__, " near line ", __LINE__, " failed with ", \
          cudaGetErrorName(__cuda_api_err), \
          " (", cudaGetErrorString(__cuda_api_err), ")"); \
    } \
  } while(0)

// broadcast conversion
// static inline operator float2(float x) const { return make_float2(x, x); }
struct frac {
  const int prec;
  union {
    uint16_t f16;
    uint16_t bf16;
    float    f32;
    double   f64;
    uint64_t b64;
  } v;
  const enum frac_tag {F16,BF16,F32,F64} tag;

  frac(float f, int _prec = -1) : tag(F32), prec(_prec) {
    v.f32 = f;
  }
  frac(double f, int _prec = -1) : tag(F64), prec(_prec) {
    v.f64 = f;
  }

  /*
  static inline frac from_bf16(uint16_t fb, int _prec = -1) {
    frac f {frac::BF16, prec};
    f.v.bf16 = fb;
    return f;
  }
  static inline frac from_f16(uint16_t fb, int _prec = -1) {
    frac f {frac::F16, prec};
    f.v.f16 = fb;
    return f;
  }
  */
private:
  frac(frac_tag t, int _prec) : tag(t), prec(_prec) { }

}; // frac

static inline float hf_to_f(uint16_t u16);
static inline float bf_to_f(uint16_t u16) {
  union {float f; uint32_t i;} u;
  u.i = (uint32_t)u16 << 16;
  return u.f;
}
static inline std::ostream &operator <<(std::ostream &os, frac f) {
  std::stringstream ss;
  if (f.prec >= 0)
    ss << std::setprecision(f.prec);
  ss << std::fixed;
  switch (f.tag) {
  case frac::F16:
    ss << hf_to_f(f.v.f16);
    break;
  case frac::BF16:
    ss << bf_to_f(f.v.bf16);
    break;
  case frac::F32:
    ss << f.v.f32;
    break;
  case frac::F64:
    ss << f.v.f64;
    break;
  default:
    fatal("mincu: << not defined for this tag\n");
  }
  os << ss.str();
  return os;
}

///////////////////////////////////////////////////////////////////////////////
// ansi colors

// [(0|1)
static const char *ANSI_RESET = "\033[0m";
static const char *ANSI_RED = "\033[1;31m";
static const char *ANSI_GREEN = "\033[1;32m";
static const char *ANSI_YELLOW = "\033[1;33m";
static const char *ANSI_BLUE = "\033[1;34m";
static const char *ANSI_MAGENTA = "\033[1;35m";
static const char *ANSI_CYAN = "\033[1;36m";
static const char *ANSI_WHITE = "\033[1;37m";
static const char *ANSI_NVMOD = "\033[38;2;94;182;0m";

template <typename T>
struct ansi_esc {
  const char *esc;
  const T value;
  ansi_esc(const char *e, const T &val) : esc(e), value(val) { }
};
template <typename T>
static ansi_esc<T> ansi_red(const T &val) {return ansi_esc(ANSI_RED, val);}
template <typename T>
static ansi_esc<T> ansi_blue(const T &val) {return ansi_esc(ANSI_BLUE, val);}
template <typename T>
static ansi_esc<T> ansi_yellow(const T &val) {return ansi_esc(ANSI_YELLOW, val);}
template <typename T>
static ansi_esc<T> ansi_green(const T &val) {return ansi_esc(ANSI_GREEN, val);}
template <typename T>
static ansi_esc<T> ansi_cyan(const T &val) {return ansi_esc(ANSI_CYAN, val);}
template <typename T>
static ansi_esc<T> ansi_magenta(const T &val) {return ansi_esc(ANSI_MAGENTA, val);}
// ...

template <typename T>
static inline std::ostream &operator<<(std::ostream &os, const ansi_esc<T> &e) {
  try {
    os << e.esc << e.value << ANSI_RESET;
  } catch (...) {
    os << ANSI_RESET;
    throw;
  }
  return os;
}

///////////////////////////////////////////////////////////////////////////////
// ansi colors

struct fmt_opts {
  int         cols_per_elem;
  int         frac_prec; // decimal precision
  bool        force_hex; // force hex (even for float)
  bool        force_dec;
  const char *ansi_color;
  constexpr fmt_opts(int cpe = -1, int pr = 3, bool fh = false, bool fd = false,
                     const char *ansi = nullptr)
      : cols_per_elem(cpe), frac_prec(pr), force_hex(fh), force_dec(fd),
        ansi_color(nullptr) {}
  //
  constexpr fmt_opts cols(int cpe) const {auto c = *this; c.cols_per_elem = cpe; return c;}
  constexpr fmt_opts hex() const {auto c = *this; c.force_hex = true; return c;}
  constexpr fmt_opts dec() const {auto c = *this; c.force_dec = true; return c;}
  constexpr fmt_opts color(const char *a) const {auto c = *this; c.ansi_color = a; return c;}
  constexpr fmt_opts prec(int p) const {auto c = *this; c.frac_prec = p; return c;}
};



template <typename T>
static void format_elem_unsigned(std::ostream &os, T t, const fmt_opts &fos) {
  if (fos.ansi_color) {
    os << fos.ansi_color;
  }
  if (fos.force_dec && !fos.force_hex) {
    os << mincu::colr<std::string>(mincu::fmt_dec(t), fos.cols_per_elem);
  } else {
    os << mincu::colr<std::string>(mincu::fmt_hex(t), fos.cols_per_elem);
  }
  if (fos.ansi_color) {
    os << ANSI_RESET;
  }
}
template <typename T>
static void format_elem_signed(std::ostream &os, T t, const fmt_opts &fos) {
  if (fos.ansi_color) {
    os << fos.ansi_color;
  }
  if (fos.force_hex && !fos.force_dec) {
    os << mincu::colr<std::string>(mincu::fmt_hex(t), fos.cols_per_elem);
  } else {
    os << mincu::colr<std::string>(mincu::fmt_dec(t), fos.cols_per_elem);
  }
  if (fos.ansi_color) {
    os << ANSI_RESET;
  }
}
template <typename T>
static void format_elem_frac(std::ostream &os, T t, const fmt_opts &fos) {
  if (fos.ansi_color) {
    os << fos.ansi_color;
  }
  if (fos.force_hex) {
    if constexpr (sizeof(T) == 8) {
      format_elem_unsigned(os, *(const uint64_t *)&t, fos);
    } else if constexpr (sizeof(T) == 4) {
      format_elem_unsigned(os, *(const uint32_t *)&t, fos);
    } else if constexpr (sizeof(T) == 2) {
      format_elem_unsigned(os, *(const uint16_t *)&t, fos);
    } else if constexpr (sizeof(T) == 1) {
      format_elem_unsigned(os, *(const uint8_t *)&t, fos);
    } else {
      static_assert("invalid float type for this function");
    }
  } else {
    os << mincu::colr<mincu::frac>(mincu::frac(t, fos.frac_prec), fos.cols_per_elem);
  }
  if (fos.ansi_color) {
    os << ANSI_RESET;
  }
}

template <typename T>
static void format_elem_prim(std::ostream &os, const T &t, const fmt_opts &fos) {
  if constexpr (std::is_floating_point<T>()) {
    format_elem_frac<T>(os, t, fos);
  } else if constexpr (std::is_signed<T>()) {
    format_elem_signed<T>(os, t, fos);
  } else if constexpr (std::is_unsigned<T>()) {
    format_elem_unsigned<T>(os, t, fos);
  } else {
    static_assert("unsupported type");
  }
}

template <typename V, typename E>
static inline void format_elem_v(
    std::ostream &os,
    const V &v,
    const fmt_opts &fos)
{
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    if (fos.ansi_color) {
      os << fos.ansi_color;
    }
    // make a copy so the element formatter doesn't apply coloring
    fmt_opts fos_copy = fos;
    fos_copy.ansi_color = nullptr;
    os << '{';
    const auto *ep = (const E *)&v;
    format_elem_prim<E>(os, ep[0], fos_copy);
    for (int i = 1; i < channels; i++) {
      os << ',';
      format_elem_prim<E>(os, ep[i], fos_copy);
    }
    os << '}';
    if (fos.ansi_color) {
      os << ANSI_RESET;
    }
  } else {
    format_elem_prim<E>(os, v, fos);
  }
}

template <typename V, typename E>
static constexpr V default_broadcast(E e) {
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V v;
    for (int i = 0; i < channels; i++) {
      ((E *)&v)[i] = e;
    }
    return v;
  } else{
    return e;
  }
}

// Functions for mincu vector types and elements
template <typename T>
static const char *type_name();
template <typename T>
static void format_elem(std::ostream &os, const T &t, const fmt_opts &fos);
template <typename T>
static T zero();
// template <typename T> static T min_val();
// template <typename T> static T max_val();
// static inline V broadcast(const E &e);

// Relations for mincu vector types and elements
template <typename V>
struct mc_type {
//  using elem_type = E;
//  static const int N = #;
//  static const V bcast(E) {...};
//  static const V zero() {...};
//  static const V min_value {...};
//  static const V max_value {...};
};
template <typename E,int N>
struct mc_elem_type {
//  using vec_type = V;
};

// TODO: remove broadcast() and zero() (use mc_type functions)
#define MAKE_MC_TYPE_V(ST, VT, VN) \
  template <> static const char *type_name<VT ## VN>() { return #VT #VN; } \
  template <> struct mc_type<VT ## VN> {\
    using elem_type = ST; \
    static const int N = VN; \
    static constexpr VT ## VN bcast(ST s) {return default_broadcast<VT ## VN, ST>(s);}; \
    static constexpr VT ## VN zero() {return default_broadcast<VT ## VN, ST>(ST(0));}; \
    static constexpr VT ## VN min() {return default_broadcast<VT ## VN, ST>(std::numeric_limits<ST>::min());}; \
    static constexpr VT ## VN max() {return default_broadcast<VT ## VN, ST>(std::numeric_limits<ST>::max());}; \
  }; \
  template <> struct mc_elem_type<ST,VN> { \
    using vec_type = VT ## VN; \
  }; \
  template <> static void format_elem<VT ## VN>(\
      std::ostream & os, const VT##VN &v, const fmt_opts &fos) \
  { \
    format_elem_v<VT ## VN,ST>(os, v, fos); \
  } \
  template <> static VT ## VN broadcast<VT ## VN>(ST x) {return default_broadcast<VT ## VN, ST>(x);} \
  template <> static VT ## VN zero<VT ## VN>() {return broadcast<VT ## VN>(ST(0));} \
  static inline std::ostream &operator<<(std::ostream &os, const VT ## VN &v) { \
    format_elem<VT ## VN>(os, v, fmt_opts()); return os; \
  } \


#define MAKE_MC_TYPE(ST, VT) \
  template <> static const char *type_name<ST>() {return #ST;} \
  template <> struct mc_type<ST> { \
    using elem_type = ST; \
    static constexpr int N = 1;\
    static constexpr ST bcast(ST s) {return ST(s);}; \
    static constexpr ST zero() {return ST(0);}; \
    static constexpr ST min() {return std::numeric_limits<ST>::min();}; \
    static constexpr ST max() {return std::numeric_limits<ST>::max();}; \
  }; \
  template <> struct mc_elem_type<ST,1> { \
    using vec_type = ST; \
  }; \
  template <> static void format_elem<ST>(\
    std::ostream &os, const ST &v, const fmt_opts &fos) \
  { \
    format_elem_v<ST,ST>(os, v, fos); \
  } \
  template <typename V> static V broadcast(ST x); \
  template <> static ST broadcast<ST>(ST x) {return ST(x);} \
  template <> static ST zero<ST>() {return ST(0);} \
  MAKE_MC_TYPE_V(ST, VT, 2) \
  MAKE_MC_TYPE_V(ST, VT, 3) \
  MAKE_MC_TYPE_V(ST, VT, 4)

////////////////////////////////////////////////
//
MAKE_MC_TYPE(uint8_t, uchar)
MAKE_MC_TYPE(uint16_t, ushort)
MAKE_MC_TYPE(uint32_t, uint)
MAKE_MC_TYPE(uint64_t, ulonglong)

MAKE_MC_TYPE(int8_t, char)
MAKE_MC_TYPE(int16_t, short)
MAKE_MC_TYPE(int32_t, int)
MAKE_MC_TYPE(int64_t, longlong)

MAKE_MC_TYPE(float, float)
MAKE_MC_TYPE(double, double)


//////////////////////////////////////////////////////////
// My first attempt at using concepts.
template <typename V, typename E>
concept is_mc_vec_elem_pair = std::is_same_v<E,typename mc_type<V>::elem_type>;

template <typename V>
concept is_mc_type_vec =
  !std::is_same_v<V,typename mc_type<V>::elem_type>;
template <typename V>
concept is_mc_trivial_type =
  std::is_same_v<V,typename mc_type<V>::elem_type>;
template <typename V>
concept is_mc_type =
  is_mc_trivial_type<V> || is_mc_type_vec<V>;


template <typename V> requires is_mc_type_vec<V>
static inline bool operator==(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    for (int i = 0; i < channels; i++) {
      if (e1[i] != e2[i]) {
        return false;
      }
    }
    return true;
  } else{
    return v1 == v2;
  }
}

template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline bool operator==(const V &v1, const E &e2) {
  return v1 == default_broadcast<V,E>(e2);
}
template <typename V> requires is_mc_type_vec<V>
static inline bool operator!=(const V &v1, const V &v2) {
  return !(v1 == v2);
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline bool operator!=(const V &v1, const E &e2) {
  return !(v1 == e2);
}
template <typename V> requires is_mc_type_vec<V>
static inline V operator+(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = e1[i] + e2[i];
    }
    return r;
  } else{
    return v1 + v2;
  }
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator+(const V &v1, const E &e2) {
  return v1 + default_broadcast<V,E>(e2);
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator+(const E &e2, const V &v1) {
  return v1 + default_broadcast<V,E>(e2);
}

template <typename V> requires is_mc_type_vec<V>
static inline V operator-(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = e1[i] - e2[i];
    }
    return r;
  } else{
    return v1 - v2;
  }
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator-(const V &v1, const E &e2) {
  return v1 - default_broadcast<V,E>(e2);
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator-(const E &e2, const V &v1) {
  return default_broadcast<V,E>(e2) - v1;
}

template <typename V> requires is_mc_type_vec<V>
static inline V operator*(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = e1[i] * e2[i];
    }
    return r;
  } else{
    return v1 * v2;
  }
}
template <typename E, typename V> requires is_mc_vec_elem_pair<V, E>
static inline V operator*(const E &e1, const V &v2) {
  return default_broadcast<V,E>(e1) * v2;
}
template <typename E, typename V> requires is_mc_vec_elem_pair<V, E>
static inline V operator*(const V &v1, const E &e2) {
  return v1 * default_broadcast<V,E>(e2);
}

template <typename V> requires is_mc_type_vec<V>
static inline V operator/(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = e1[i] / e2[i];
    }
    return r;
  } else{
    return v1 / v2;
  }
}
template <typename E, typename V> requires is_mc_vec_elem_pair<V, E>
static inline V operator/(const E &e1, const V &v2) {
  return default_broadcast<V,E>(e1) / v2;
}
template <typename E, typename V> requires is_mc_vec_elem_pair<V, E>
static inline V operator/(const V &v1, const E &e2) {
  return v1 / default_broadcast<V,E>(e2);
}

template <typename E>
  requires is_mc_trivial_type<E>
static inline E mc_mod_helper(E e0, E e1)
{
  if constexpr (std::is_floating_point_v<E>) {
    return std::fmod(e0, e1);
  } else {
    return e0 % e1;
  }
}
template <typename V> requires is_mc_type<V>
static inline V mc_mod(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = mc_mod_helper(e1[i], e2[i]);
    }
    return r;
  } else {
    return mc_mod_helper(v1, v2);
  }
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_mod(const V &v1, const E &e2) {
  return mc_mod(v1, default_broadcast<V,E>(e2));
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_mod(const E &e1, const V &v2) {
  return mc_mod(default_broadcast<V,E>(e1), v2);
}
template <typename V> requires is_mc_type_vec<V>
static inline V operator%(const V &v1, const V &v2) {
  return mc_mod(v1, v2);
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator%(const V &v1, const E &e2) {
  return v1 % default_broadcast<V,E>(e2);
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V operator%(const E &e2, const V &v1) {
  return default_broadcast<V,E>(e2) % v1;
}

// negation
template <typename V> requires is_mc_type_vec<V>
static inline V operator-(const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    for (int i = 0; i < channels; i++) {
      ((E *)&r)[i] = -((E *)&v)[i];
    }
    return r;
  } else{
    return -v;
  }
}

template <typename V> requires is_mc_type_vec<V>
static inline V &operator+=(V &lhs, const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    for (int i = 0; i < channels; i++) {
      ((E *)&lhs)[i] += ((const E *)&v)[i];
    }
  } else{
    lhs += v;
  }
  return lhs;
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V &operator+=(V &lhs, const E &e) {
  return lhs += default_broadcast<V,E>(e);
}
template <typename V> requires is_mc_type_vec<V>
static inline V &operator-=(V &lhs, const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    for (int i = 0; i < channels; i++) {
      ((E *)&lhs)[i] -= ((const E *)&v)[i];
    }
  } else{
    lhs += v;
  }
  return lhs;
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V &operator-=(V &lhs, const E &e) {
  return lhs -= default_broadcast<V,E>(e);
}
template <typename V> requires is_mc_type_vec<V>
static inline V &operator*=(V &lhs, const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    for (int i = 0; i < channels; i++) {
      ((E *)&lhs)[i] *= ((const E *)&v)[i];
    }
  } else{
    lhs *= v;
  }
  return lhs;
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V &operator*=(V &lhs, const E &e) {
  return lhs *= default_broadcast<V,E>(e);
}
template <typename V> requires is_mc_type_vec<V>
static inline V &operator/=(V &lhs, const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    for (int i = 0; i < channels; i++) {
      ((E *)&lhs)[i] /= ((const E *)&v)[i];
    }
  } else{
    lhs /= v;
  }
  return lhs;
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V &operator/=(V &lhs, const E &e) {
  return lhs /= default_broadcast<V,E>(e);
}
template <typename V> requires is_mc_type_vec<V>
static inline V &operator%=(V &lhs, const V &v) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    for (int i = 0; i < channels; i++) {
      ((E *)&lhs)[i] = mc_mod_helper<E>(((E *)&lhs)[i], ((const E *)&v)[i]);
    }
  } else{
    lhs %= v;
  }
  return lhs;
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V &operator%=(V &lhs, const E &e) {
  return lhs %= default_broadcast<V,E>(e);
}

template <typename V> requires is_mc_type<V>
static inline V mc_max(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = std::max<E>(e1[i], e2[i]);
    }
    return r;
  } else{
    return std::max<V>(v1, v2);
  }
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_max(const V &v1, const E &e2) {
  return mc_max(v1, default_broadcast<V,E>(e2));
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_max(const E &e1, const V &v2) {
  return mc_max(default_broadcast<V,E>(e1), v2);
}
template <typename V> requires is_mc_type<V>
static inline V mc_min(const V &v1, const V &v2) {
  using E = typename mc_type<V>::elem_type;
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    const E *e1 = (const E *)&v1, *e2 = (const E *)&v2;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = std::min<E>(e1[i], e2[i]);
    }
    return r;
  } else{
    return std::min<V>(v1, v2);
  }
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_min(const V &v1, const E &e2) {
  return mc_min(v1, default_broadcast<V,E>(e2));
}
template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_min(const E &e1, const V &v2) {
  return mc_min(default_broadcast<V,E>(e1), v2);
}
template <typename OT, typename IT>
  requires is_mc_type<OT> && is_mc_type<IT> &&
           (mc_type<OT>::N == mc_type<IT>::N) &&
           std::is_convertible_v<typename mc_type<IT>::elem_type, typename mc_type<OT>::elem_type>
static inline OT mc_cvt(const OT &in) {
  using OTE = typename mc_type<OT>::elem_type;
  using ITE = typename mc_type<IT>::elem_type;
  constexpr auto channels = sizeof(OT) / sizeof(OTE);
  if constexpr (channels > 1) {
    OT r;
    const ITE *ein = (const ITE *)&in;
    OTE *er = (OTE *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = OTE(ein);
    }
    return r;
  } else{
    return OTE(in);
  }
}

template <typename V, typename E> requires is_mc_vec_elem_pair<V, E>
static inline V mc_cons(std::function<E(size_t)> func) {
  constexpr auto channels = sizeof(V) / sizeof(E);
  if constexpr (channels > 1) {
    V r;
    E *er = (E *)&r;
    for (int i = 0; i < channels; i++) {
      er[i] = func((size_t)i);
    }
    return r;
  } else{
    return V(func(0));
  }
}
///////////////////////////////////////////////////////////////////////////////
// random number generation

struct random_state {
  std::mt19937 gen;

  explicit random_state() : gen(std::random_device()()) { }
  explicit random_state(unsigned seed) : gen(seed) { }
  explicit random_state(const char *seed) {
    std::string str = seed;
    std::seed_seq ss(str.begin(), str.end());
    gen.seed(ss);
  }
}; // random_state

//////////////////////////////////////////////////////////////
// generic randomization function for all mincu types
// D - distribution e.g. std::uniform_int_distribution<int16_t>
// V - is vector type; e.g. int8_t and int16_t will use above D
// ER - representation element type
template <typename V, typename ER, typename D>
  requires is_mc_type<V>
static void mc_randomize_with(
  random_state &rnd,
  V *vs,
  size_t nvs,
  V lo,
  V hi)
{
  using E = typename mc_type<V>::elem_type;
  const E *los = (const E *)&lo;
  const E *his = (const E *)&hi;

  // creates per channel std::uniform_{int,real}_distribution<RE>
  D dists[mc_type<V>::N];
  for (size_t i = 0; i < mc_type<V>::N; i++) {
    dists[i] = D(E(los[i]), ER(his[i]));
  }

  for (size_t i = 0; i < nvs; i++) {
    E *val_chans = (E *)(vs + i);
    for (size_t i = 0; i < mc_type<V>::N; i++) {
      val_chans[i] = E(dists[i](rnd.gen));
    }
  }
}

template <typename V>
  requires is_mc_type<V>
static void mc_randomize(
  random_state &rs,
  V *vs,
  size_t nvs,
  V lo = mc_type<V>::min(),
  V hi = mc_type<V>::max())
{
  using E = typename mc_type<V>::elem_type;

  if constexpr (std::is_same<E,int8_t>()) {
    // int8_t use rep type of int16_t
    //   case handles int8_t, char2, char3, char4
    mc_randomize_with<V, int16_t, std::uniform_int_distribution<int16_t>>(
        rs, vs, nvs, lo, hi);
  } else if constexpr (std::is_same<E,uint8_t>()) {
    // uint8_t use rep type of uint16_t
    //   case handles uint8_t, uchar2, uchar3, uchar4
    mc_randomize_with<V, uint16_t, std::uniform_int_distribution<uint16_t>>(
        rs, vs, nvs, lo, hi);
  } else if constexpr (std::is_floating_point<E>()) {
    mc_randomize_with<V, E, std::uniform_real_distribution<E>>(rs, vs, nvs, lo, hi);
  } else {
    mc_randomize_with<V, E, std::uniform_int_distribution<E>>(rs, vs, nvs, lo, hi);
  }
}

///////////////////////////////////////////////////////////////////////////////
// buffer formatting

template <typename V>
static int default_format_elem_preferred_columns() {
  using E = typename mc_type<V>::elem_type;
  if constexpr (std::is_floating_point<E>()) {
    return std::numeric_limits<E>::max_digits10;
  } else if constexpr (std::is_signed<E>()) {
    return std::numeric_limits<E>::max_digits10;
  } else {
    return 2 + 2 * sizeof(E); // 0x+...
  }
}


template <typename T>
static void format_buffer(
    std::ostream &os,
    const T *ptr,
    size_t elems,
    int elems_per_line,
    std::function<void(std::ostream&,const T&)> fmt_elem,
    std::function<const char*(const T&)> color_elem_ansi,
    std::function<void(std::ostream&,size_t)> fmt_addr)
{
  os << type_name<T>() << "[" << elems << "]: " <<
    "0x" << fmt_hex_digits<uint64_t>((uintptr_t)ptr) << ":\n";
  elems_per_line = elems_per_line < 0 ? 8 : elems_per_line;
  size_t i = 0;
  while (i < elems) {
    fmt_addr(os, i);
    os << ": ";
    for (size_t c = 0; c < elems_per_line && i < elems; c++, i++) {
      os << "  ";
      auto color = color_elem_ansi(ptr[i]);
      if (color)
          os << color;
      fmt_elem(os, ptr[i]);
      if (color)
          os << ANSI_RESET;
    }
    os << "\n";
  }
}
template <typename T>
static void format_buffer(
    std::ostream &os,
    const T *ptr,
    size_t elems,
    int elems_per_line,
    std::function<void(std::ostream&,const T&)> fmt_elem,
    std::function<const char*(const T&)> color_elem)
{
  const int addr_cols =
      sizeof(T) * elems <= 0xFFFFull ? 4 :
      sizeof(T) * elems <= 0xFFFFFFull ? 6 :
      sizeof(T) * elems <= 0xFFFFFFFFull ? 8 : -1;
  std::function<void(std::ostream&,size_t)> fmt_addr =
      [&](std::ostream &os, size_t ix) {
        os << mincu::fmt_hex_digits<uint64_t>(sizeof(T) * ix, addr_cols);
      };
  mincu::format_buffer<T>(os, ptr, elems, elems_per_line,
                          fmt_elem, color_elem, fmt_addr);
}


///////////////////////////////////////////////////////////////////////////////
template <typename T>
static std::function<const char*(const T&)> color_elems_none() {
  std::function<const char*(const T&)> fmt_no_color =
      [](const T&) -> const char * {return nullptr;};
  return fmt_no_color;
}
template <typename T>
static std::function<void(std::ostream &, const T &)> default_elem_formatter(
    const fmt_opts &fos) {
  std::function<void(std::ostream &, const T &)> fmt =
      [&](std::ostream &os, const T &t) {
        mincu::format_elem(os, t, fos);
      };
  return fmt;
}

///////////////////////////////////////////////////////////////////////////////
// memory initialization generators
template <typename E>
struct const_seq {
  const E value;

  const_seq(E e) : value(e) { }

  void apply(E *vals, size_t n) const {
    for (size_t i = 0; i < n; i++) {vals[i] = value;}
  }
};

// bounded linear arithmetic sequence
//   x[i] = (x[i-1] + seq_delta)            [ when seq_mod absent ]
//   x[i] = (x[i-1] + seq_delta) % seq_mod  [ when seq_mod != 0 ]
template <typename E>
struct arith_seq {
  const E seq_init, seq_delta;
  const std::optional<E> seq_mod;
  arith_seq(E _init = mc_type<E>::bcast(0),
            E _delta = mc_type<E>::bcast(1),
            std::optional<E> mod = std::nullopt)
      : seq_init(_init), seq_delta(_delta), seq_mod(mod)
  {
    if (mod && *mod == zero<E>())
      fatal("arith_seq: zero modulus (division by zero)");
  }
  arith_seq(E _init, E _delta, E mod)
      : arith_seq(_init, _delta, std::make_optional(mod)) {}

  void apply(E *vals, size_t n) const {
    E val = seq_init;
    auto next_val =
        [&]() {
          if (seq_mod) {
            val = mc_mod<E>(val, *seq_mod);
          }
          return val;
        };
    for (size_t k = 0; k < n; k++) {
      vals[k] = next_val();
      val += seq_delta;
    }
  } // apply
}; // arith_seq

// cyclic sequence
template <typename E>
struct cyc_seq {
  const std::initializer_list<E> ilist;

  cyc_seq(std::initializer_list<E> _elems) : ilist(_elems) {
    if (ilist.size() == 0)
      fatal("mincu: cyc_seq empty list");
  }

  void apply(E *vals, size_t n) const {
    size_t i = 0;
    while (true) {
      for (const E &e : ilist) {
        if (i == n) {
          break;
        }
        vals[i++] = e;
      }
      if (i == n)
        break;
    }
  }
}; // cyc_seq

// E - the element type
// R - representation type (usually the same)
//    e.g. E=half uses R=float;  E=int8_t uses R=int16_t
template <typename E>
  requires is_mc_type<E>
struct rnd_seq {
  mincu::random_state &rndst;
  const E rnd_lo, rnd_hi;

  rnd_seq(
      mincu::random_state &_rndst,
      E _rnd_lo = mc_type<E>::min(),
      E _rnd_hi = mc_type<E>::max())
    : rndst(_rndst), rnd_lo(_rnd_lo), rnd_hi(_rnd_hi) { }

  void apply(E *vals, size_t n) const {
    mc_randomize<E>(rndst, vals, n, rnd_lo, rnd_hi);
  }
}; // rnd_seq

// TODO: other possible sequences.
//  - derived seqs:
//     * alt_seq(seq1,seq2) flips between them
//     * sub_seq
//     * map_seq
//     * fold_seq
//  - file_seq reads from a file with optional (stoff, len)
//

///////////////////////////////////////////////////////////////////////////////
// umem - unified memory buffer
struct umem_alloc {
  void *mem;
  size_t mem_size;

  umem_alloc(size_t size) : mem_size(size) {
    // size = align_up<size_t>(size, 4096);
    CUDA_API(cudaMallocManaged, (void **)&mem, size);
    // mem = _aligned_malloc(size, 4096);
  }
  ~umem_alloc() {
    // std::cout << "~umem_alloc " << mem << "\n";
    if (mem) {
      CUDA_API(cudaFree, mem);
      // _aligned_free(mem);
      mem = nullptr;
      mem_size = 0x0;
    }
  }
};

// template <typename E>
// using init_function_type = std::function<E(size_t);

// genericize umem
template <typename E>
  // requires(is_trivially_copyable_v<E>
class umem // unified memory buffer
{
  // destructor deallocs umem_alloc, if it's the last ref
  std::shared_ptr<umem_alloc> ptr;
  size_t                      elems;

public:
  explicit umem(size_t _elems)
    : elems(_elems), ptr(std::make_shared<umem_alloc>(_elems * sizeof(E))) { }
  explicit umem(std::shared_ptr<umem_alloc> &_ptr, size_t _elems)
    : elems(_elems), ptr(_ptr) { }

  explicit umem(size_t _elems, const const_seq<E> &g)
    : umem<E>(_elems) {init(g);}
  explicit umem(size_t _elems, const arith_seq<E> &g)
    : umem<E>(_elems) {init(g);}
  explicit umem(size_t _elems, const cyc_seq<E> &g)
    : umem<E>(_elems) {init(g);}
  explicit umem(size_t _elems, const rnd_seq<E> &g)
    : umem<E>(_elems) {init(g);}
  explicit umem(size_t _elems, std::function<E(size_t)> g)
    : umem<E>(_elems) {init(g);}

  void init(const const_seq<E> &g) {g.apply(get_ptr(), size());}
  void init(const arith_seq<E> &g) {g.apply(get_ptr(), size());}
  void init(const cyc_seq<E> &g) {g.apply(get_ptr(), size());}
  void init(const rnd_seq<E> &g) {g.apply(get_ptr(), size());}

  void init(std::function<E(size_t)> g) {
    for (size_t i = 0; i < size(); i++) {
      get_ptr()[i] = g(i);
    }
  }

  size_t size() const {return elems;}

  operator std::vector<E> () const {return to_vector();}

  std::vector<E> to_vector() const {
    std::vector<E> es;
    es.reserve(size());
    for (size_t i = 0; i < size(); i++) {
      es.emplace_back(get_cptr()[i]);
    }
    return es;
  }

  // template <typename U>
  // umem<U> as() {
  //   return umem<U>(ptr, elems*sizeof(E)/sizeof(U));
  // }

  void prefetch_to_device() const {
    // fails on Windows: https://stackoverflow.com/questions/50717306/invalid-device-ordinal-on-cudamemprefetchasync
    CUDA_API(cudaMemPrefetchAsync, get_cptr(), elems * sizeof(E), 0);
  }
  void prefetch_to_host() const {
    CUDA_API(cudaMemPrefetchAsync, get_cptr(), elems * sizeof(E), cudaCpuDeviceId);
  }

   // elements
   operator       E *()        {return get_ptr();}
                  E *get_ptr() {return const_cast<E *>(get_cptr());}
   operator const E *()         const {return get_cptr();}
            const E *get_cptr() const {return (const E *)ptr.get()->mem;}
   //      E &operator[](size_t ix)       {return get_ptr()[ix];} // TODO: check bounds
   //const E &operator[](size_t ix) const {return get_ptr()[ix];}


  void str(
    std::function<void(std::ostream&,const E&)> fmt_elem,
    std::ostream &os = std::cout,
    int elems_per_line = -1) const
  {
    format_buffer(os, get_cptr(), size(),
                  elems_per_line,
                  fmt_elem, color_elems_none<E>());
  }
  void str(
    std::ostream &os = std::cout,
    int elems_per_line = -1,
    int cols_per_elem = default_format_elem_preferred_columns<E>(),
    int prec = -1) const
  {
    fmt_opts fos{cols_per_elem, prec};
    str(default_elem_formatter<E>(fos), os, elems_per_line);
  }
  std::string str(int elems_per_line = -1,
                  int cols_per_elem = 0,
                  int prec = -1) const {
    std::stringstream ss;
    str(ss, elems_per_line, cols_per_elem, prec);
    return ss.str();
  }
}; // struct umem

///////////////////////////////////////////////////////////////////////////////
// fp16 support
static const int F32_BITS = 32;
static const int F32_MNT_BITS = 23;
static const int F32_EXP_BITS = F32_BITS - 1 - F32_MNT_BITS; // 23
static const int F32_BIAS = (1 << (F32_EXP_BITS - 1)) - 1; // 127
static const uint32_t F32_SIGN_BIT  = 1u << (F32_BITS - 1); // 0x80000000
static const uint32_t F32_EXP_MASK =
  ((1 << F32_EXP_BITS) - 1) << F32_MNT_BITS;
static const uint32_t F32_MANT_MASK = (1u << F32_MNT_BITS) - 1; // 0x007FFFFF
static const uint32_t F32_QNAN_BIT = (F32_MANT_MASK + 1) >> 1; // 0x00400000

static const int F16_BITS = 16;
static const int F16_MNT_BITS = 10;
static const int F16_EXP_BITS = F16_BITS - F16_MNT_BITS - 1;
static const int F16_BIAS = (1 << (F16_EXP_BITS - 1)) - 1; // 15
static const uint16_t F16_SIGN_BIT = 1 << (F16_BITS - 1); // 0x8000
static const uint16_t F16_EXP_MASK =
  ((1 << F16_EXP_BITS) - 1) << F16_MNT_BITS; // 0x7C00
static const uint16_t F16_MANT_MASK = (1 << F16_MNT_BITS) - 1; // 0x03FF
static const uint16_t F16_QNAN_BIT = (F16_MANT_MASK + 1) >> 1; // 0x0200

static const int F32_F16_BIAS_DIFF = F32_BIAS - F16_BIAS;

static const int F32_F16_MNT_DIFF = 23 - 10;

constexpr static inline uint32_t float_to_bits(float f) {
  union{float f; uint32_t i;} u{f};
  return u.i;
}

constexpr static inline float float_from_bits(uint32_t f) {
  union{uint32_t i; float f;} u{f};
  return u.f;
}

static inline float half_bits_to_float_impl(uint16_t u16, bool set_qnan)
{
  uint16_t u16_u = u16 & 0x7FFF;
  uint32_t s32 = ((uint32_t)u16 & F16_SIGN_BIT) << 16;
  uint32_t m16 = u16 & F16_MANT_MASK;
  if (u16_u > F16_EXP_MASK) {
    // preserve qNaN bit disposition
    // initially match whatever the fp16 qNaN bit was
    uint32_t m32 =
      (u16 & F16_QNAN_BIT) << F32_F16_MNT_DIFF;
    if (set_qnan) {
      m32 |= F32_QNAN_BIT; // ensure it's set irrespective of input
    }
    if (m32 == 0) {
      m32 = 1; // ensure still NaN
    }
    return float_from_bits(s32 | F32_EXP_MASK | m32);
  }
  uint32_t e16 = (u16 & F16_EXP_MASK) >> F16_MNT_BITS;
  uint32_t e32, m32;
  if (u16_u == F16_EXP_MASK) {
    // +-infinity
    e32 = F32_EXP_MASK >> F32_MNT_BITS;
    m32 = 0;
  } else if (e16 != 0 && e16 < 0x1F) {
    //  normal number
    e32 = e16 + F32_F16_BIAS_DIFF; // bias difference; // 0x70
    m32 = m16 << F32_F16_MNT_DIFF; // (23 - 10);
  } else if (e16 == 0 && m16 != 0) {
    // denorm/subnorm number (e16 == 0) => renormalize it
    // shift the mantissa left until the hidden one gets set
    for (e32 = F32_F16_BIAS_DIFF + 1;
      (m16 & (F16_MANT_MASK + 1)) == 0;
      m16 <<= 1, e32--)
      ;
    m32 = (m16 << F32_F16_MNT_DIFF) & F32_MANT_MASK;
  } else { // if (e16 == 0) // +/- 0.0
    e32 = 0;
    m32 = 0;
  }
  return float_from_bits(s32 | (e32 << F32_MNT_BITS) | m32);
}
static inline float hf_to_f(uint16_t u16) {
  return half_bits_to_float_impl(u16, true);
}

///////////////////////////////////////////////////////////////////////////////
// integer parsing
template <typename T>
static T parse_uintT(
  const std::string &s,
  bool allow_suffix,
  const char *what)
{
  try {
    if (s.size() >= 1 && s[0] == '-')
      mincu::fatal(s,": must be positive ", what);

    const char *str = s.c_str();

    T sign = 1;
    if (std::is_signed<T>::value) {
      if (s.size() > 0 && s[0] == '-') {
        str++;
        sign = T(-1);
      }
    }
    T val = 0;

    // std::strtoull saturates at max value; so we do our own parsing here
    if (s.size() > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
      str += 2;
      int nds = 0;
      while (isxdigit(*str)) {
        nds++;
        T d =
          *str >= 'A' && *str <= 'F' ? *str - 'A' + 10 :
          *str >= 'a' && *str <= 'f' ? *str - 'a' + 10 :
          *str - '0';
        T new_val = 16 * val + d;
        if (new_val < val) {
          mincu::fatal(s, ": overflows ", what);
        }
        val = new_val;
        str++;
      }
      if (nds == 0) {
        mincu::fatal(s, ": malformed ", what);
      }
    } else {
      int nds = 0;
      while (isdigit(*str)) {
        nds++;
        T new_val = 10 * val + *str - '0';
        if (new_val < val) {
          mincu::fatal(s, ": overflows ", what);
        }
        val = new_val;
        str++;
      }
      if (nds == 0) {
        mincu::fatal(s, ": malformed ", what);
      }
    }
    while (*str == ' ')
      str++;
    if (allow_suffix && *str) {
      T scale = 1;
      if (*str == 'K' || *str == 'k')
        scale = 1024;
      else if (*str == 'M' || *str == 'm')
        scale = 1024*1024;
      else if (*str == 'G' || *str == 'g')
        scale = 1024*1024*1024;
      else
        mincu::fatal(s, ": malformed ", what, " suffix (expected K, M, or G)");
      str++;

      T new_val = val * scale;
      if (new_val < val) {
        fatal(s, ": value overflows type size");
      }
      val = new_val;
    }
    if (*str)
      mincu::fatal(s, ": malformed ", what);
    if (val == std::numeric_limits<T>::min() && sign == T(-1))
      mincu::fatal(s, ": negation of min value has no effect");

    return sign * val;
  } catch (...) {
    mincu::fatal(s, ": malformed ", what);
    return T(-1);
  }
}
static uint64_t parse_uint64(
  const std::string &s,
  bool allow_suffix = false,
  const char *what = "uint64")
{
  return parse_uintT<uint64_t>(s, allow_suffix, what);
}
static uint64_t parse_positive_uint64(
  const std::string &s,
  bool allow_suffix = false,
  const char *what = "uint64")
{
  auto x = parse_uint64(s, allow_suffix, what);
  if (x == 0) {
    mincu::fatal(s, ": ", what, " must be positive");
  }
  return x;
}
static int64_t parse_int64(
  const std::string &s,
  bool allow_suffix = false,
  const char *what = "int64")
{
  return parse_uintT<int64_t>(s, allow_suffix, what);
}

///////////////////////////////////////////////////////////////////////////////
// times dispatches
static float time_dispatch_ms(std::function<void()> func)
{
  cudaEvent_t start, stop;
  CUDA_API(cudaEventCreate, &start);
  CUDA_API(cudaEventCreate, &stop);
  CUDA_API(cudaEventRecord, start);
  func();
  CUDA_API(cudaEventRecord, stop);
  CUDA_API(cudaEventSynchronize, stop);
  float time = 0.0f;
  CUDA_API(cudaEventElapsedTime, &time, start, stop);
  CUDA_API(cudaEventDestroy, start);
  CUDA_API(cudaEventDestroy, stop);
  return time;
}
static float time_dispatch_s(std::function<void()> func) {
  return time_dispatch_ms(func) / 1000.0f;
}



///////////////////////////////////////////////////////////////////////////////
// needs to be deferred until ostream << instances are created
template <typename T, typename...Ts>
static void format_to(std::ostream &os, T t, Ts...ts) {
  if constexpr (std::is_pointer<T>()) {
    if (t == nullptr) {
      os << "nullptr";
    } else {
      os << t;
    }
  } else if constexpr (std::is_unsigned<T>()) {
    os << fmt_hex(t, 0);
  } else {
    os << t;
  }
  format_to(os, ts...);
}

#ifdef _WIN32
static void mincu_enable_colored_io()
{
  static bool enabled = false;
  if (enabled)
    return;
  // TODO: should only do this on Windows 10 Threshold 2 (TH2),
  // "November Update": version 1511 and has the build number 10586
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
  // https://docs.microsoft.com/en-us/windows/console/setconsolemode
  // https://bugs.php.net/bug.php?id=72768
  // https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
  auto enable_on_handle = [](DWORD H_CODE) {
    DWORD mode;
    HANDLE h = GetStdHandle(H_CODE);
    GetConsoleMode(h, &mode);
    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(h, mode);
  };
  enable_on_handle(STD_ERROR_HANDLE);
  enable_on_handle(STD_OUTPUT_HANDLE);
  enabled = true;
}

// This should only be expanded once, but it shouldn't hurt.
// So if big projects have multiple copies, big deal.
#define MINCU_ENABLE_COLOR_IO_VIA_STATIC_CONSTRUCTOR() \
  struct mincu_dummy_enable_color { \
    mincu_dummy_enable_color() {mincu_enable_colored_io();} \
  }; \
  static mincu_dummy_enable_color __mincu_dummy;
#else // !_WIN32
#define MINCU_ENABLE_COLOR_IO_VIA_STATIC_CONSTRUCTOR()
#endif // !_WIN32

} // namespace mincu::

#endif // MINCU_HPP