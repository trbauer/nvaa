#ifndef MINCU_HPP
#define MINCU_HPP

// #if __cplusplus < 201703L
// #error "Need -std=c++17"
// #endif

#include <cmath>
#include <cstdint>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <ostream>
#include <random>
#include <sstream>
#include <string>
#include <type_traits>

#include <cuda_runtime_api.h>

namespace mincu
{
template <typename T>
static inline T align_up(T n, T a) {
    return (n + a - 1) - ((n + a - 1) % a);
}

///////////////////////////////////////////////////////////////////////////////
// text formatting
template <typename...Ts>
static void format_to(std::stringstream &os) { }
template <typename T, typename...Ts>
static void format_to(std::stringstream &os, T t, Ts...ts) {os << t; format_to(os, ts...);}
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

enum class pad {L, R};

template <pad P, typename T>
struct col
{
  const T &value;
  size_t width;
  char pad_fill;
  col(const T &val, size_t wid = 2 * sizeof(T), char f = ' ')
    : value(val), width(wid), pad_fill(f) { }
}; // col
template <typename T>
using coll = col<pad::R,T>;
template <typename T>
using colr = col<pad::L,T>;

template <pad P,typename T>
static inline std::ostream &operator<<(std::ostream &os, const col<P,T> &p) {
  auto s = format(p.value);
  std::stringstream ss;
  if (P == pad::L) {
    for (size_t i = s.size(); i < p.width; i++)
      ss << p.pad_fill;
  }
  ss << s;
  if (P == pad::R) {
    for (size_t i = s.size(); i < p.width; i++)
      ss << p.pad_fill;
  }
  os << ss.str();
  return os;
}

template <typename...Ts>
static void fatal(Ts...ts) {
  std::cerr << format(ts...) << "\n";
  exit(EXIT_FAILURE);
}

///////////////////////////////////////////////////////////////////////////////
// CUDA API wrapper

#define CUDA_API(__CUDA_API__,...) \
  do { \
    auto __cuda_api_err = __CUDA_API__(__VA_ARGS__); \
    if (__cuda_api_err != cudaSuccess) { \
      fatal(#__CUDA_API__, " near line ", __LINE__, " failed with ", \
          cudaGetErrorName(__cuda_api_err), \
          " (", cudaGetErrorString(__cuda_api_err), ")"); \
    } \
  } while(0)


///////////////////////////////////////////////////////////////////////////////
// random number generation

struct random_state {
  std::mt19937 gen;

  random_state() : gen(std::random_device()()) { }
  random_state(unsigned seed) : gen(seed) { }
  random_state(const char *seed) {
    std::string str = seed;
    std::seed_seq ss(str.begin(), str.end());
    gen.seed(ss);
  }
}; // random_state

template <typename T,typename R = T>
static void randomize_integral(
  random_state &rnd,
  T *vals,
  size_t nelems,
  T lo = std::numeric_limits<T>::min(),
  T hi = std::numeric_limits<T>::max())
{
  std::uniform_int_distribution<R> d {R(lo), R(hi)};
  for (size_t i = 0; i < nelems; i++) {
    vals[i] = T(d(rnd.gen));
  }
}

template <typename T, typename R = T>
static void randomize_real(
  random_state &rnd,
  T *vals,
  size_t nelems,
  T lo = T(0.0f),
  T hi = T(1.0f))
{
  std::uniform_real_distribution<R> d {R(lo), R(hi)};
  for (size_t i = 0; i < nelems; i++) {
    vals[i] = T(d(rnd.gen));
  }
}

// T is the type of the buffer
// R is the type of the random sequence;
//   we use a conversion operator to take R to T;
//
// Normally R = T, initialize a buffer of floats to
// random float values, but this enables things like
// randomizing floats to an integer sequence (each converted to float)
// for example.
//
// In addition, int8_t doesn't have a std::uniform_int_distribution
// instance.  Hence, one would need to use int16_t or something as
// the random sequence.
// template <typename T,typename R = T>
// static void randomize(random_state &rs, T *vals, size_t elems, R lo, R hi);

//
// E.g. for float
//   template <typename T>
//   static void randomize(T *vals, size_t elems, float lo, float hi) {
//     randomize_real<T,float>(vals, elems, lo, hi);
//   }

template <typename T, typename R>
static void randomize(random_state &rs, T *vals, size_t elems, T lo, T hi);

#define RANDOMIZE_INSTANCE2(T,R,DELEGATE)\
  template <>\
  static void randomize<T,R>(random_state &rs, T *vals, size_t elems, T lo, T hi) { \
    DELEGATE<T,R>(rs, vals, elems, lo, hi); \
  }
#define RANDOMIZE_INSTANCE(T,DELEGATE)\
  RANDOMIZE_INSTANCE2(T,T,DELEGATE)

RANDOMIZE_INSTANCE(float, randomize_real)
RANDOMIZE_INSTANCE(double, randomize_real)

RANDOMIZE_INSTANCE2(int8_t, int16_t, randomize_integral)
RANDOMIZE_INSTANCE(int16_t, randomize_integral)
RANDOMIZE_INSTANCE(int32_t, randomize_integral)
RANDOMIZE_INSTANCE(int64_t, randomize_integral)
RANDOMIZE_INSTANCE2(uint8_t, uint16_t, randomize_integral)
RANDOMIZE_INSTANCE(uint16_t, randomize_integral)
RANDOMIZE_INSTANCE(uint32_t, randomize_integral)
RANDOMIZE_INSTANCE(uint64_t, randomize_integral)


/////////////////////////////////////////////////
// vector instances
//   e.g. int2, int3, int4, float2, float3, float4
//
// This is harder becuase our bounds were our overload ranges.
// template <typename T>
// static void randomize(T ## N *vals, size_t elems, RCAST_TO lo, RCAST_TO hi) {...}
// splicing T ## N creates nonsensical T4 for float 4.
// We need RCAST_TO to be float4 to follow the pattern.
//
// E.g. float2 is RANDOMIZE_VECTOR_INSTANCE(float,2,randomize_real)
// #define RANDOMIZE_VECTOR_INSTANCE2(RCAST_TO,RSEQ,N,DELEGATE)\
//   template <typename T>\
//   static void randomize(T ## N *vals, size_t elems, RCAST_TO lo, RCAST_TO hi) {\
//     DELEGATE<T,RSEQ>(&vals[0].x, N*elems, lo, hi); \
//   }
// #define RANDOMIZE_VECTOR_INSTANCE(R,N,DELEGATE)\
//   RANDOMIZE_VECTOR_INSTANCE2(R,R,N,DELEGATE)
//
// float2
// RANDOMIZE_VECTOR_INSTANCE(float,2,randomize_real)

// broadcast conversion
// static inline operator float2(float x) const { return make_float2(x, x); }
struct frac {
  const int prec;
  union {
    uint16_t f16;
    uint16_t f16x2[2];
    uint16_t bf16;
    uint16_t bf16x2[2];
    float    f32;
    float2   f32x2;
    float4   f32x4;
    double   f64;
    double2  f64x2;
  } v;
  const enum frac_tag {F16,F16x2,BF16,BF16x2,F32,F32x2,F32x4,F64,F64x2} tag;

  frac(float f, int _prec = -1) : tag(F32), prec(_prec) {
    v.f32 = f;
  }
  frac(float2 f, int _prec = -1) : tag(F32x2), prec(_prec) {
    v.f32x2 = f;
  }
  frac(float4 f, int _prec = -1) : tag(F32x4), prec(_prec) {
    v.f32x4 = f;
  }
  frac(double f, int _prec = -1) : tag(F64), prec(_prec) {
    v.f64 = f;
  }
  frac(double2 f, int _prec = -1) : tag(F64x2), prec(_prec) {
    v.f64x2 = f;
  }
  /*
  static inline frac bf16(uint16_t fb, int _prec = -1) {
    frac f {frac::BF16, prec};
    f.v.bf16 = fb;
    return f;
  }
  static inline frac bf16x2(uint32_t fb, int _prec = -1) {
    frac f {frac::BF16x2, prec};
    f.v.bf16x2[0] = fb & 0xFFFF;
    f.v.bf16x2[1] = fb >> 16;
    return f;
  }
  static inline frac f16(uint16_t fb, int _prec = -1) {
    frac f {frac::F16, prec};
    f.v.f16 = fb;
    return f;
  }
  static inline frac f16x2(uint32_t fb, int _prec = -1) {
    frac f {frac::F16x2, prec};
    f.v.f16x2[0] = fb & 0xFFFF;
    f.v.f16x2[1] = fb >> 16;
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
static inline std::ostream &operator <<(std::ostream &os, frac v) {
  std::stringstream ss;
  if (v.prec >= 0)
    ss << std::setprecision(v.prec);
  ss << std::fixed;
  switch (v.tag) {
  case frac::F16:
    ss << hf_to_f(v.v.f16);
    break;
  case frac::F16x2:
    ss << "{" << hf_to_f(v.v.f16x2[0]) << ", " << hf_to_f(v.v.f16x2[1]) << "}";
    break;
  case frac::BF16:
    ss << bf_to_f(v.v.bf16);
    break;
  case frac::BF16x2:
    ss << "{" << bf_to_f(v.v.bf16x2[0]) << ", " << bf_to_f(v.v.bf16x2[1]) << "}";
    break;
  case frac::F32:
    ss << v.v.f32;
    break;
  case frac::F32x2:
    ss << "{" << v.v.f32x2.x << ", " << v.v.f32x2.y << "}";
    break;
  case frac::F32x4:
    ss << "{" << v.v.f32x4.x << ", " << v.v.f32x4.y << ", " << v.v.f32x4.z << ", " << v.v.f32x4.w << "}";
    break;
  case frac::F64:
    ss << v.v.f64;
    break;
  case frac::F64x2:
    ss << "{" << v.v.f64x2.x << ", " << v.v.f64x2.y << "}";
    break;
  default:
    fatal("mincu: << not defined for this tag\n");
  }
  os << ss.str();
  return os;
}

///////////////////////////////////////////////////////////////////////////////
// buffer formatting

template <typename T> static inline const char *type_name() {return "???";}
template <> static inline const char *type_name<int8_t>() {return "int8_t";}
template <> static inline const char *type_name<int16_t>() {return "int16_t";}
template <> static inline const char *type_name<int32_t>() {return "int32_t";}
template <> static inline const char *type_name<int64_t>() {return "int64_t";}
template <> static inline const char *type_name<uint8_t>() {return "uint8_t";}
template <> static inline const char *type_name<uint16_t>() {return "uint16_t";}
template <> static inline const char *type_name<uint32_t>() {return "uint32_t";}
template <> static inline const char *type_name<uint64_t>() {return "uint64_t";}
template <> static inline const char *type_name<float>() {return "float";}
template <> static inline const char *type_name<float2>() {return "float2";}
template <> static inline const char *type_name<float4>() {return "float4";}
template <> static inline const char *type_name<double>() {return "double";}
template <> static inline const char *type_name<double2>() {return "double2";}
template <> static inline const char *type_name<double4>() {return "double4";}


template <typename T>
static std::string fmt_hex_digits(T t, int cw = -1) {
  cw = cw <= 0 ? 2 * sizeof(t) : cw;
  std::stringstream ss;
  ss << std::setw(cw) << std::uppercase << std::setfill('0')
    << std::hex << t;
  return ss.str();
}
template <typename T>
static std::string fmt_dec(T t, int cw = -1) {
  cw = cw <= 0 ? 2 * sizeof(t) : cw;
  std::stringstream ss;
  ss << std::setw(cw) << std::dec << t;
  return ss.str();
}

template <typename T>
static void format_elem(std::ostream &os, T t, int prec);

template <>
static void format_elem(std::ostream &os, uint16_t t, int prec) {
  os << "0x" << fmt_hex_digits(t);
}
template <>
static void format_elem(std::ostream &os, uint32_t t, int prec) {
  os << "0x" << fmt_hex_digits(t);
}
template <>
static void format_elem(std::ostream &os, uint64_t t, int prec) {
  os << "0x" << fmt_hex_digits(t);
}
template <>
static void format_elem(std::ostream &os, int16_t t, int prec) {
  os << fmt_dec(t);
}
template <>
static void format_elem(std::ostream &os, int32_t t, int prec) {
  os << fmt_dec(t);
}
template <>
static void format_elem(std::ostream &os, int64_t t, int prec) {
  os << fmt_dec(t);
}
template <>
static void format_elem(std::ostream &os, float t, int prec) {
  os << frac(t, prec);
}
template <>
static void format_elem(std::ostream &os, float2 t, int prec) {
  os << frac(t, prec);
}
template <>
static void format_elem(std::ostream &os, float4 t, int prec) {
  os << frac(t, prec);
}

template <typename T>
static void format_buffer(
    std::ostream &os,
    const T *ptr,
    size_t elems,
    int elems_per_line,
    std::function<void(std::ostream&,const T&)> fmt_elem,
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
      fmt_elem(os, ptr[i]);
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
    std::function<void(std::ostream&,const T&)> fmt_elem)
{
  const int addr_size =
    sizeof(T) * elems <= 0xFFFFull ? 4 :
    sizeof(T) * elems <= 0xFFFFFFFFull ? 8 : -1;
  format_buffer<T>(os, ptr, elems, elems_per_line, fmt_elem,
      [&](std::ostream &os, size_t ix) {
        os << fmt_hex_digits<uint64_t>(sizeof(T) * ix, addr_size);
      });
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
//   x[i] = (x[i-1] + seq_delta)            [ when seq_mod == 0 ]
//   x[i] = (x[i-1] + seq_delta) % seq_mod  [ when seq_mod != 0 ]
template <typename E>
struct arith_seq {
  const E seq_init, seq_delta, seq_mod;
  arith_seq(E _init, E _delta = E(1), E mod = E(0))
      : seq_init(_init), seq_delta(_delta), seq_mod(mod) { }

  void apply(E *vals, size_t n) const {
    E val = seq_init;
    if (seq_mod != E(0)) {
      if constexpr (std::is_floating_point<E>::value &&
                    std::is_floating_point<E>::value) {
        val = std::fmod(val, seq_mod);
      } else {
        val %= seq_mod;
      }
    }
    for (size_t k = 0; k < n; k++) {
      vals[k] = val;
      val += seq_delta;
      if (seq_mod != E(0)) {
        if constexpr (std::is_floating_point<E>::value &&
                      std::is_floating_point<E>::value) {
          val = std::fmod(val, seq_mod);
        } else {
          val %= seq_mod;
        }
      }
    }
  } // apply
}; // arith_seq

// cyclic sequence
template <typename E>
struct cyc_seq {
  const std::initializer_list<E> ilist;

  cyc_seq(std::initializer_list<E> _elems) : ilist(_elems) {
    if (ilist.empty())
      fatal("mincu: cyc_seq empty list");
  }

  void apply(E *vals, size_t n) const {
    size_t i = 0;
    for (size_t k = 0; k < n; k++) {
      if (i == ilist.size())
        i = 0;
      vals[k] = ilist[i];
    }
  }
}; // cyc_seq

// E - the element type
// R - representation type (usually the same)
//    e.g. E=half uses R=float;  E=int8_t uses R=int16_t
template <typename E, typename R = E>
struct rnd_seq {
  const random_state rndst;
  const E rnd_lo, rnd_hi;

  rnd_seq(
      const random_state &_rndst,
      E _rnd_lo = std::numeric_limits<E>::min(),
      E _rnd_hi = std::numeric_limits<E>::max())
    : rndst(_rndst), rnd_lo(_rnd_lo), rnd_hi(_rnd_hi) { }

  void apply(E *vals, size_t n) const {
    random_state tmp = rndst; // make copy (randomize advances state)
    random_state &tmpr = tmp;
    randomize<E,R>(tmpr, vals, n, rnd_lo, rnd_hi);
  }
}; // rnd_seq


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

  void init(std::function<E(size_t)> f) {
    for (size_t i = 0; i < size(); i++) {
      get_ptr()[i] = f[i];
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
   operator       E *()       {return get_ptr();}
                  E *get_ptr() {return const_cast<E *>(get_cptr());}
   operator const E *() const {return get_cptr();}
            const E *get_cptr() const {return (const E *)ptr.get()->mem;}
   //      E &operator[](size_t ix)       {return get_ptr()[ix];} // TODO: check bounds
   //const E &operator[](size_t ix) const {return get_ptr()[ix];}


  void str(
    std::function<void(std::ostream&,const E&)> fmt_elem,
    std::ostream &os = std::cout,
    int elems_per_line = -1,
    int prec = -1) const
  {
    format_buffer(os, get_cptr(), size(), elems_per_line, fmt_elem);
  }
  void str(
    std::ostream &os = std::cout,
    int elems_per_line = -1,
    int prec = -1) const
  {
    std::function<void(std::ostream&,const E&)> fmt =
        [&](std::ostream &os, const E &e){format_elem<E>(os, e, prec);};
    str(fmt, os, elems_per_line, prec);
  }
  std::string str(int elems_per_line = -1, int prec = -1) const {
    std::stringstream ss;
    str(ss, elems_per_line, prec);
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

} // namespace mincu::

#endif // MINCU_HPP