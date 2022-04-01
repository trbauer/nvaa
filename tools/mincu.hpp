#ifndef MINCU_HPP
#define MINCU_HPP

#include <cstdint>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
// #include <memory>
#include <random>
#include <ostream>
#include <sstream>
#include <string>

#include <cuda_runtime_api.h>

namespace mincu
{

template <typename...Ts>
static void format_to(std::stringstream &os) { }
template <typename T, typename...Ts>
static void format_to(std::stringstream &os, T t, Ts...ts) {os << t; format_to(os, ts...);}
template <typename...Ts>
static std::string   format(Ts...ts) {
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
static inline std::ostream &operator<< (std::ostream &os, const col<P,T> &p) {
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
#define CUDA_API(__CUDA_API__,...) \
  do {\
    auto __cuda_api_err = __CUDA_API__(__VA_ARGS__);\
    if (__cuda_api_err != cudaSuccess) {\
      fatal(#__CUDA_API__," near line ",__LINE__," failed with ",\
          cudaGetErrorName(__cuda_api_err),\
          " (",cudaGetErrorString(__cuda_api_err),")");\
    }\
  } while(0)

struct random_state {
    std::mt19937 gen;

    random_state() : gen(std::random_device()()) { }
    random_state(unsigned seed) : gen(seed) { }
    random_state(const char *seed) {
      std::string str = seed;
      std::seed_seq ss(str.begin(), str.end());
      gen.seed(ss);
    }

    // static std::seed_seq get_seq(const char *s) {
    //  std::string str = s;
    //  return std::seed_seq(str.begin(), str.end());
    // }
}; // random_state

template <typename T,typename R = T>
static void randomize_integral(
  random_state &rnd,
  T *vals,
  size_t elems,
  T lo = std::numeric_limits<T>::min(),
  T hi = std::numeric_limits<T>::max())
{
  std::uniform_int_distribution<R> d(lo, hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = T(d(rnd.gen));
  }
}

template <typename T, typename R = T>
static void randomize_real(
  random_state &rnd,
  T *vals,
  size_t elems,
  T lo = (T)0.0f,
  T hi = (T)1.0f)
{
  std::uniform_real_distribution<R> d((R)lo,(R)hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = T(d(rnd.gen));
  }
}

/*

// int8_t and uint8_t don't have unifor_int_distribution instances
// use 16b and narrow
template <typename T,typename R = T>
static void handle_init_random_int(T *vals, size_t elems, T lo, T hi) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<R> d(lo,hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = (T)d(gen);
  }
}
template <typename uint8_t,typename R = uint16_t>
static void handle_init_random_int(T *vals, size_t elems, T lo, T hi) {
  handle_init_random_int<uint8_t,uint16_t>(vals, elems, lo, hi);
}

template <typename T>
static void handle_init_random_int(T *vals, size_t elems, T lo, T hi)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<T> d(lo,hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = d(gen)
  }
}
template <typename T>
static void handle_init_random_real(T *vals, size_t elems, T lo, T hi)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<T> d(lo,hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = d(gen)
  }
}


#define VINT_INSTANCE(VTYPE) \
  template <>\
  static void handle_init_random<TYPE>(TYPE *vals, size_t elems, TYPE lo, TYPE hi) {\
    handle_init_random_int<TYPE>(vals, elems, lo, hi);\
  }

#define INT_INSTANCE(TYPE) \
  template <>\
  static void handle_init_random<TYPE>(TYPE *vals, size_t elems, TYPE lo, TYPE hi) {\
    handle_init_random_int<TYPE>(vals, elems, lo, hi);\
  }

INT_INSTANCE(uint8_t)
INT_INSTANCE(uint16_t)
INT_INSTANCE(uint32_t)
INT_INSTANCE(uint64_t)
INT_INSTANCE(int8_t)
INT_INSTANCE(int16_t)
INT_INSTANCE(int32_t)
INT_INSTANCE(int64_t)




static global_state gs;
static void set_global_seed(unsigned s) {
  gs.gen.seed(s);
}

/*
template <typename T, typename R = T>
static void randomize_real(T *vals, size_t elems, T lo, T hi)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<R> d((R)lo,(R)hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = d(gs.gen);
  }
}
template <typename T,typename R = T>
static void randomize_integral(T *vals, size_t elems, T lo, T hi)
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<R> d((R)lo,(R)hi);
  for (size_t i = 0; i < elems; i++) {
    vals[i] = (T)d(gs.gen);
  }
}
*/

// T is the type of the buffer
// R is the type of the random sequence;
//   we use a conversion operator to take R to T;
//
// Normally R = T, initialize a buffer of floats to
// random float values, but this enables things like
// randomizing floats to an integer sequence (each converted to float)
// for example.
//
// In addition int8_t doesn't have a std::uniform_int_distribution
// instance.  Hence, one would need to use int16_t or something as
// the random sequence.
// template <typename T,typename R = T>
// static void randomize(T *vals, size_t elems, R lo, R hi);

//
// E.g. for float
//   template <typename T>
//   static void randomize(T *vals, size_t elems, float lo, float hi) {
//     randomize_real<T,float>(vals, elems, lo, hi);
//   }

#define RANDOMIZE_INSTANCE2(RCAST_TO,RSEQ,DELEGATE)\
  template <typename T>\
  static void randomize(random_state &rs, T *vals, size_t elems, RCAST_TO lo, RCAST_TO hi) {\
    DELEGATE<T,RSEQ>(rs, vals, elems, lo, hi); \
  }
#define RANDOMIZE_INSTANCE(R,DELEGATE)\
  RANDOMIZE_INSTANCE2(R,R,DELEGATE)

RANDOMIZE_INSTANCE(float,randomize_real)
RANDOMIZE_INSTANCE(double,randomize_real)

RANDOMIZE_INSTANCE2(int8_t,int16_t,randomize_integral)
RANDOMIZE_INSTANCE(int16_t,randomize_integral)
RANDOMIZE_INSTANCE(int32_t,randomize_integral)
RANDOMIZE_INSTANCE(int64_t,randomize_integral)
RANDOMIZE_INSTANCE2(uint8_t,uint16_t,randomize_integral)
RANDOMIZE_INSTANCE(uint16_t,randomize_integral)
RANDOMIZE_INSTANCE(uint32_t,randomize_integral)
RANDOMIZE_INSTANCE(uint64_t,randomize_integral)


/////////////////////////////////////////////////
// vector instances
//   e.g. int2, int3, int4, float2, float3, float4
//
// This is harder becuase our bounds were our overload ranges.
// template <typename T>
// static void randomize(T ## N *vals, size_t elems, RCAST_TO lo, RCAST_TO hi) {
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


template <typename R>
struct init {
  enum class init_type{NONE, CONST, SEQ, RANDOM, FUNC} type;
  union {
    struct {R const_val;};
    struct {R seq_init, seq_delta, seq_mod;};
    struct {random_state *rndst; R rnd_lo, rnd_hi;};
    struct {std::function<R(size_t)> *func;};
  };
  init() : type(init_type::NONE) { }
  init(R _const) : type(init_type::CONST), const_val(_const) { }
  init(random_state *rs, R _lo, R _hi) : type(init_type::RANDOM), rndst(rs), rnd_lo(_lo), rnd_hi(_hi) { }
  init(R _init, R _delta, R mod) : type(init_type::SEQ), seq_init(_init), seq_delta(_delta), seq_mod(mod) { }
  init(std::function<R(size_t)> *_func) : type(init_type::FUNC), func(_func) { }

  template <typename T = R>
  void apply(T *vals, size_t elems) const
  {
    switch (type) {
    case init_type::NONE:
      return;
    case init_type::CONST:
    {
      for (size_t k = 0; k < elems; k++)
        vals[k] = T(const_val);
    }
    case init_type::SEQ:
    {
      R val = seq_init;
      if (seq_mod) {
        val %= seq_mod;
      }
      for (size_t k = 0; k < elems; k++) {
        vals[k] = val;
        val += seq_delta;
        if (seq_mod) {
          val = val % seq_mod;
        }
      }
      return;
    }
    case init_type::RANDOM:
      randomize<T>(*rndst, vals, elems, rnd_lo, rnd_hi);
      return;
    case init_type::FUNC:
      for (size_t k = 0; k < elems; k++)
        vals[k] = (*func)(k);
    }
  }
};
template <typename R>
static init<R> init_none() {return init<R>();}
template <typename R>
static init<R> init_const(R _const) {return init<R>(_const);}
template <typename R>
static init<R> init_seq(R _init, R _delta = (R)1, R _mod = 0) {
  return init<R>(_init, _delta, _mod);
}
template <typename R>
static init<R> init_random(
  random_state &_rs,
  R _rnd_lo = std::numeric_limits<R>::min(),
  R _rnd_hi = std::numeric_limits<R>::max())
{
    return init<R>(&_rs, _rnd_lo,_rnd_hi);
}
template <typename R>
static init<R> init_random(std::function<R(size_t)> apply) {
    return init<R>(&apply);
}


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
template <> static inline const char *type_name<double>() {return "double";}


struct umem_alloc {
  void *mem;
  size_t mem_size;

  umem_alloc(size_t size) : mem_size(size) {
    // std::cout << "allocating " << size << "B\n";
    CUDA_API(cudaMallocManaged,(void **)&mem, size);
  }
  ~umem_alloc() {
    if (mem) {
      CUDA_API(cudaFree, mem);
      mem = nullptr;
      mem_size = 0x0;
    }
  }
};

#if 0
// should I enable span access?
// TODO: add stride
// TODO: make conversion routines
// TODO: support with init umem constructors and combine with umem?
template <typename E>
class umem_view
{
  std::shared_ptr<umem_alloc> ptr;
  size_t                      off, len;

        E *get_ptr()        {return const_cast<E *>(get_cptr());}
  const E *get_cptr() const {return (const E *)ptr.get()->mem + off;}

public:
  umem_view(
      umem_alloc _alloc,
      size_t _off = 0,
      size_t _len = _alloc.mem_size/sizeof(E))
    : alloc(_alloc)
    , off(_off)
    , len(_len)
  {
    if (off >= len)
      fatal("umem_view with off > len");
    if (off >= _alloc.mem_size/sizeof(E))
      fatal("umem_view with start off out of bounds");
    // TODO: we could have a char[13] map to int[2] pruning the tails
    if (_alloc.mem_size % sizeof(E))
      fatal("umem_view with misaligned type");
  }

  void prefetch_to_device() const {
    CUDA_API(cudaMemPrefetchAsync,
      get_cptr(),               0, size()*sizeof(E));
  }
  void prefetch_to_host() const {
    CUDA_API(cudaMemPrefetchAsync,
      get_cptr(), cudaCpuDeviceId, size()*sizeof(E));
  }

  operator       E *()       {return get_ptr();}
  operator const E *() const {return get_ptr();}

  size_t size() const {return len - off;}

  template <typename R>
  void apply(const init<R> &i) {
    i.apply<E>(get_ptr(), size());
    // prefetch_to_device();
  }

  template <typename T>
  static void format_elem(std::ostream &os, T t, int prec) {
    if (prec >= 0)
      os << std::fixed  << std::setprecision(prec) << t;
    else
      os << t;
  }

  template <>
  static void format_elem(std::ostream &os, uint16_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, uint32_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, uint64_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, int16_t t, int prec) {
    os << fmtDec(t);
  }
  template <>
  static void format_elem(std::ostream &os, int32_t t, int prec) {
    os << fmtDec(t);
  }
  template <>
  static void format_elem(std::ostream &os, int64_t t, int prec) {
    os << fmtDec(t);
  }

  template <typename T>
  static std::string fmtHexDigits(T t, int cw = -1) {
    cw = cw <= 0 ? 2*sizeof(t) : cw;
    std::stringstream ss;
    ss << std::setw(cw) << std::uppercase << std::setfill('0')
      << std::hex << t;
    return ss.str();
  }
  template <typename T>
  static std::string fmtDec(T t, int cw = -1) {
    cw = cw <= 0 ? 2*sizeof(t) : cw;
    std::stringstream ss;
    ss << std::setw(cw) << std::dec << t;
    return ss.str();
  }

  void str(
    std::ostream &os = std::cout,
    int elems_per_line = -1, int prec = -1) const
  {
    os << type_name<E>() << "[" << elems << "]: " <<
      "0x" << fmtHexDigits<uint64_t>((uintptr_t)get_cptr()) << ":\n";
    elems_per_line = elems_per_line < 0 ? 8 : elems_per_line;
    prec = prec < 0 ? 3 : prec;
    int addr_size =
      sizeof(E)*elems <= 0xFFFFull ? 4 :
      sizeof(E)*elems <= 0xFFFFFFFFull ? 8 :
      -1;
    size_t i = 0;
    while (i < elems) {
      os << fmtHexDigits<uint64_t>(i, addr_size) << ": ";
      for (size_t c = 0; c < elems_per_line && i < elems; c++, i++) {
        os << "  ";
        format_elem(os, get_cptr()[i], prec);
        os.flush();
      }
      os << "\n";
    }
  }
  std::string str(int elems_per_line = -1, int prec = -1) const {
    std::stringstream ss;
    formt(os, elems_per_line, prec);
    return ss.str();
  }
};
#endif // 0

// this could derive
template <typename E>
class umem // unified memory buffer
{
  // FIXME: we need to reset the value if a dispatch fails
  std::shared_ptr<umem_alloc> ptr;
  size_t                      elems;

public:
  umem(size_t _elems)
    : elems(_elems)
    , ptr(std::make_shared<umem_alloc>(_elems*sizeof(E)))
  {
  }
  umem(std::shared_ptr<umem_alloc> &_ptr, size_t elems)
    : elems(_elems)
    , ptr(_ptr)
  {
  }

  template <typename R>
  umem(size_t _elems, const init<R> &i)
    : umem<E>(_elems)
  {
    apply<R>(i);
  }
  ~umem() {
    // destructs ptr, which deallocs umem_alloc, if it's the last ref
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
    CUDA_API(cudaMemPrefetchAsync, get_cptr(), 0, elems*sizeof(E));
  }
  void prefetch_to_host() const {
    CUDA_API(cudaMemPrefetchAsync, get_cptr(), cudaCpuDeviceId, elems*sizeof(E));
  }


  template <typename R>
  void apply(const init<R> &i) {
    i.apply<E>(get_ptr(), size());
    // prefetch_to_device();
  }


   // elements
   operator       E *()       {return get_ptr();}
                  E *get_ptr() {return const_cast<E *>(get_cptr());}
   operator const E *() const {return get_cptr();}
            const E *get_cptr() const {return (const E *)ptr.get()->mem;}
   //      E &operator[](size_t ix)       {return get_ptr()[ix];}
   //const E &operator[](size_t ix) const {return get_ptr()[ix];}


  template <typename T>
  static void format_elem(std::ostream &os, T t, int prec) {
    if (prec >= 0)
      os << std::fixed  << std::setprecision(prec) << t;
    else
      os << t;
  }

  template <>
  static void format_elem(std::ostream &os, uint16_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, uint32_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, uint64_t t, int prec) {
    os << "0x" << fmtHexDigits(t);
  }
  template <>
  static void format_elem(std::ostream &os, int16_t t, int prec) {
    os << fmtDec(t);
  }
  template <>
  static void format_elem(std::ostream &os, int32_t t, int prec) {
    os << fmtDec(t);
  }
  template <>
  static void format_elem(std::ostream &os, int64_t t, int prec) {
    os << fmtDec(t);
  }


  template <typename T>
  static std::string fmtHexDigits(T t, int cw = -1) {
    cw = cw <= 0 ? 2*sizeof(t) : cw;
    std::stringstream ss;
    ss << std::setw(cw) << std::uppercase << std::setfill('0')
      << std::hex << t;
    return ss.str();
  }
  template <typename T>
  static std::string fmtDec(T t, int cw = -1) {
    cw = cw <= 0 ? 2*sizeof(t) : cw;
    std::stringstream ss;
    ss << std::setw(cw) << std::dec << t;
    return ss.str();
  }

  void str(
    std::ostream &os = std::cout,
    int elems_per_line = -1, int prec = -1) const
  {
    os << type_name<E>() << "[" << elems << "]: " <<
      "0x" << fmtHexDigits<uint64_t>((uintptr_t)get_cptr()) << ":\n";
    elems_per_line = elems_per_line < 0 ? 8 : elems_per_line;
    prec = prec < 0 ? 3 : prec;
    int addr_size =
      sizeof(E)*elems <= 0xFFFFull ? 4 :
      sizeof(E)*elems <= 0xFFFFFFFFull ? 8 :
      -1;
    size_t i = 0;
    while (i < elems) {
      os << fmtHexDigits<uint64_t>(i, addr_size) << ": ";
      for (size_t c = 0; c < elems_per_line && i < elems; c++, i++) {
        os << "  ";
        format_elem(os, get_cptr()[i], prec);
        os.flush();
      }
      os << "\n";
    }
  }
  std::string str(int elems_per_line = -1, int prec = -1) const {
    std::stringstream ss;
    str(ss, elems_per_line, prec);
    return ss.str();
  }
}; // struct umem





/*
#define VEC_OPERATORS_WITH_OP(STYPE, VTYPE,OP_SYMBOL)\
  VTYPE operator OP_SYMBOL (VTYPE v1, VTYPE v2) {\
    return make_ ## VTYPE (v1.x OP_SYMBOL v2.x, v1.y OP_SYMBOL v2.y, v3.z OP_SYMBOL v2.z,v1.w OP_SYMBOL v2.w);\
  }\
  VTYPE operator OP_SYMBOL (STYPE s, VTYPE v) {\
    return make_ ## VTYPE (s OP_SYMBOL v.x,s OP_SYMBOL v.y,s OP_SYMBOL v.z,s OP_SYMBOL v.w);\
  }\
  VTYPE  operator OP_SYMBOL (VTYPE v, STYPE s) {\
    return s OP_SYMBOL v;\
  }

#define VEC_OPERATORS(STYPE, VTYPE)\
  VEC_OPERATORS_WITH_OP(STYPE, VTYPE, *)
  VEC_OPERATORS_WITH_OP(STYPE, VTYPE, /)
  VEC_OPERATORS_WITH_OP(STYPE, VTYPE, +)
  VEC_OPERATORS_WITH_OP(STYPE, VTYPE, +)

#define OPERATORS(TYPE)\
  VEC_OPERATORS(TYPE,TYPE ## 1)\
  VEC_OPERATORS(TYPE,TYPE ## 2)\
  VEC_OPERATORS(TYPE,TYPE ## 3)\
  VEC_OPERATORS(TYPE,TYPE ## 4)

OPERATORS(float)
OPERATORS(double)
//
OPERATORS(int)

#undef VEC_OPERATORS
#undef OPERATORS
*/


} // namespace mincu::

#endif