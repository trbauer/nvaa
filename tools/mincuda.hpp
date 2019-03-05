#ifndef MIN_CUDA
#define MIN_CUDA

#include <cstdint>
#include <iostream>
#include <random>
#include <sstream>
#include <string>

template <typename...Ts>
static void format_to(std::stringstream &os) { }
template <typename T, typename...Ts>
static void format_to(std::stringstream &os, T t, Ts...ts) {os << t; format_to(os, ts...);}
template <typename...Ts>
static std::string   format(Ts...ts) {
  std::stringstream ss; format_to(ss, ts...); return ss.str();
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

*/

struct global_state {
    std::mt19937 gen;
    global_state() : gen (std::random_device()()) {
      gen.seed(2019);
    }
};
static global_state gs;
static void set_global_seed(unsigned s) {
  gs.gen.seed(s);
}

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
  static void randomize(T *vals, size_t elems, RCAST_TO lo, RCAST_TO hi) {\
    DELEGATE<T,RSEQ>(vals, elems, lo, hi); \
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
  enum class init_type{NONE,CONST,RANDOM} type;
  union {
    struct {R val_const;};
    struct {R rnd_lo; R rnd_hi;};
  };
  init() : type(init_type::NONE) { }
  init(R _const) : type(init_type::CONST), val_const(_const) { }
  init(R _lo, R _hi) : type(init_type::RANDOM), rnd_lo(_lo), rnd_hi(_hi) { }

  template <typename T = R>
  void apply(T *vals, size_t elems) const
  {
    switch (type) {
    case init_type::NONE:
      return;
    case init_type::CONST:
      for (size_t k = 0; k < elems; k++)
        vals[k] = (T)val_const;
      return;
    case init_type::RANDOM:
      randomize<T>(vals, elems, rnd_lo, rnd_hi);
      // fatal("handle_init_random: not handled yet");
      return;
    }
  }
};
template <typename R>
static init<R> init_none() {return init<R>();}
template <typename R>
static init<R> init_const(R _const) {return init<R>(_const);}
template <typename R>
static init<R> init_random(R _rnd_lo, R _rnd_hi) {return init<R>(_rnd_lo,_rnd_hi);}



template <typename T>
class umem // "unified memory"
{
  T            *mem;
public:
  const size_t  elems;

  umem(size_t _elems)
    : elems(_elems)
  {
    CUDA_API(cudaMallocManaged,&mem, _elems*sizeof(*mem));
  }
  template <typename R>
  umem(size_t _elems, const init<R> &i)
    : umem<T>(elems)
  {
    apply<R>(i);
  }
  ~umem() {
    if (mem) {
      CUDA_API(cudaFree, mem);
      mem = nullptr;
    }
  }

  void prefetch() const {
    CUDA_API(cudaMemPrefetchAsync, mem, elems*sizeof(*mem));
  }

  template <typename R>
  void apply(const init<R> &i) {
    i.apply<T>(mem,elems);
  }
  size_t size() const{return elems;}

  // elements
  operator       T *()       {return mem;}
  operator const T *() const {return mem;}
        T &operator[](size_t ix)       {return mem[ix];}
  const T &operator[](size_t ix) const {return mem[ix];}
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



#endif