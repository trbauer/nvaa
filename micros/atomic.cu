#include <cstdint>
#include <cuda_fp16.h>




extern "C"
static __device__ __noinline__ void u16_atomics(
  uint16_t *out, uint16_t *slm, uint16_t arg)
//  unsigned short int *out, unsigned short int arg)
{
//unsigned short int atomicCAS(unsigned short int *address,
//                             unsigned short int compare,
//                             unsigned short int val);
//  *out = atomicCAS(out, arg, (uint16_t)(arg+1));

//  *out = atomicCAS(out, arg, (unsigned short int)(arg+1));
  asm volatile("atom.shared.cas.b16 %0, [%1], %2, %3;" :
    "=h"(arg) : "l"(slm), "h"(arg), "h"((uint16_t)(arg+1)));
  asm volatile("atom.global.cas.b16 %0, [%1], %2, %3;" :
    "=h"(arg) : "l"(out), "h"(arg), "h"((uint16_t)(arg+1)));
  // no 16b op
  // asm("red.global.cas.b16 [%0], %1, %2;" :: "l"(out), "h"(arg), "h"((uint16_t)(arg+1)));
}

extern "C"
static __device__ __noinline__ void s32_atomics(
  int32_t *out, int32_t *_slm, int32_t arg)
{
  __shared__ int32_t slm[32];
  slm[threadIdx.x] = *out + 4;
  __syncthreads();

  arg = atomicCAS(slm+arg%32, arg+1, arg);
  //
  arg = atomicExch(slm+arg%32, arg);
  (void)atomicExch(slm+arg%32, arg);
  //
  arg = atomicMin(slm+arg%32, arg);
  (void)atomicMin(slm+arg%32, arg);
  arg = atomicMax(slm+arg%32, arg);
  (void)atomicMax(slm+arg%32, arg);
  //
  // (void)atomicInc(slm, arg);
  // (void)atomicDec(slm, arg);
  //
  arg = atomicAdd(slm+arg%32, arg);
  (void)atomicAdd(slm+arg%32, arg);
  (void)atomicSub(slm+arg%32, arg);

  arg += slm[arg % 32];

  __syncthreads();

  arg = atomicCAS(out, arg+1, arg);
  //
  arg = atomicExch(out, arg);
  (void)atomicExch(out, arg);
  //
  arg = atomicMin(out, arg);
  (void)atomicMin(out, arg);
  arg = atomicMax(out, arg);
  (void)atomicMax(out, arg);
  //
  // (void)atomicInc(out, arg);
  // (void)atomicDec(out, arg);
  //
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  (void)atomicSub(out, arg);
}

extern "C"
static __device__ __noinline__ void u32_atomics(
  uint32_t *out,  uint32_t *slm, uint32_t arg)
{
  arg = atomicCAS(slm, arg+1, arg);
  (void)atomicExch(slm, arg);
  //
  arg = atomicMin(slm, arg);
  (void)atomicMin(slm, arg);
  arg = atomicMax(slm, arg);
  (void)atomicMax(slm, arg);
  //
  arg = atomicInc(slm, arg);
  (void)atomicInc(slm, arg);
  arg = atomicDec(slm, arg);
  (void)atomicDec(slm, arg);
  //
  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);
  arg = atomicSub(slm, arg);
  (void)atomicSub(slm, arg);
  //
  arg = atomicAnd(slm, arg);
  (void)atomicAnd(slm, arg);
  arg = atomicXor(slm, arg);
  (void)atomicXor(slm, arg);
  arg = atomicOr(slm, arg);
  (void)atomicOr(slm, arg);

  __syncthreads();

  arg = atomicCAS(out, arg+1, arg);
  (void)atomicExch(out, arg);
  //
  arg = atomicMin(out, arg);
  (void)atomicMin(out, arg);
  arg = atomicMax(out, arg);
  (void)atomicMax(out, arg);
  //
  arg = atomicInc(out, arg);
  (void)atomicInc(out, arg);
  arg = atomicDec(out, arg);
  (void)atomicDec(out, arg);
  //
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  arg = atomicSub(out, arg);
  (void)atomicSub(out, arg);
  //
  arg = atomicAnd(out, arg);
  (void)atomicAnd(out, arg);
  arg = atomicXor(out, arg);
  (void)atomicXor(out, arg);
  arg = atomicOr(out, arg);
  (void)atomicOr(out, arg);
}

extern "C"
static __device__ __noinline__ void u32_atomics_system(
  uint32_t *out, uint32_t *slm, uint32_t arg)
{
  (void)atomicExch_system(slm, arg);
  arg = atomicCAS_system(slm, arg+1, arg);
  //
  arg = atomicMin_system(slm, arg);
  (void)atomicMin_system(slm, arg);
  arg = atomicMax_system(slm, arg);
  (void)atomicMax_system(slm, arg);
  //
  arg = atomicInc_system(slm, arg);
  (void)atomicInc_system(slm, arg);
  arg = atomicDec_system(slm, arg);
  (void)atomicDec_system(slm, arg);
  //
  arg = atomicAdd_system(slm, arg);
  (void)atomicAdd_system(slm, arg);
  (void)atomicSub_system(slm, arg);
  //
  arg = atomicAnd_system(slm, arg);
  (void)atomicAnd_system(slm, arg);
  arg = atomicXor_system(slm, arg);
  (void)atomicXor_system(slm, arg);
  arg = atomicOr_system(slm, arg);
  (void)atomicOr_system(slm, arg);

  __syncthreads();

  (void)atomicExch_system(out, arg);
  arg = atomicCAS_system(out, arg+1, arg);
  //
  arg = atomicMin_system(out, arg);
  (void)atomicMin_system(out, arg);
  arg = atomicMax_system(out, arg);
  (void)atomicMax_system(out, arg);
  //
  arg = atomicInc_system(out, arg);
  (void)atomicInc_system(out, arg);
  arg = atomicDec_system(out, arg);
  (void)atomicDec_system(out, arg);
  //
  arg = atomicAdd_system(out, arg);
  (void)atomicAdd_system(out, arg);
  (void)atomicSub_system(out, arg);
  //
  arg = atomicAnd_system(out, arg);
  (void)atomicAnd_system(out, arg);
  arg = atomicXor_system(out, arg);
  (void)atomicXor_system(out, arg);
  arg = atomicOr_system(out, arg);
  (void)atomicOr_system(out, arg);
}

extern "C"
static __device__ __noinline__ void u32_atomics_block(
  uint32_t *out, uint32_t *slm, uint32_t arg)
{
  arg = atomicCAS_block(slm, arg+1, arg);
  (void)atomicExch_block(slm, arg);
  //
  (void)atomicMin_block(slm, arg);
  (void)atomicMax_block(slm, arg);
  //
  arg = atomicInc_block(slm, arg);
  (void)atomicInc_block(slm, arg);
  arg = atomicDec_block(slm, arg);
  (void)atomicDec_block(slm, arg);
  //
  arg = atomicAdd_block(slm, arg);
  (void)atomicAdd_block(slm, arg);
  (void)atomicSub_block(slm, arg);
  //
  arg = atomicAnd_block(slm, arg);
  (void)atomicAnd_block(slm, arg);
  arg = atomicXor_block(slm, arg);
  (void)atomicXor_block(slm, arg);
  arg = atomicOr_block(slm, arg);
  (void)atomicOr_block(slm, arg);

  __syncthreads();

  arg = atomicCAS_block(out, arg+1, arg);
  (void)atomicExch_block(out, arg);
  //
  (void)atomicMin_block(out, arg);
  (void)atomicMax_block(out, arg);
  //
  arg = atomicInc_block(out, arg);
  (void)atomicInc_block(out, arg);
  arg = atomicDec_block(out, arg);
  (void)atomicDec_block(out, arg);
  //
  arg = atomicAdd_block(out, arg);
  (void)atomicAdd_block(out, arg);
  (void)atomicSub_block(out, arg);
  //
  arg = atomicAnd_block(out, arg);
  (void)atomicAnd_block(out, arg);
  arg = atomicXor_block(out, arg);
  (void)atomicXor_block(out, arg);
  arg = atomicOr_block(out, arg);
  (void)atomicOr_block(out, arg);
}



extern "C"
static __device__ __noinline__ void u64_atomics(
  uint64_t *out, uint64_t *slm, uint64_t arg)
{
  arg = atomicCAS(slm, arg+1, arg);
  //
  arg = atomicMin(slm, arg);
  (void)atomicMin(slm, arg);
  arg = atomicMax(slm, arg);
  (void)atomicMax(slm, arg);
  //
  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);
  //
  arg = atomicAnd(slm, arg);
  (void)atomicAnd(slm, arg);
  arg = atomicXor(slm, arg);
  (void)atomicXor(slm, arg);
  arg = atomicOr(slm, arg);
  (void)atomicOr(slm, arg);

  __syncthreads();

  arg = atomicCAS(out, arg+1, arg);
  //
  arg = atomicMin(out, arg);
  (void)atomicMin(out, arg);
  arg = atomicMax(out, arg);
  (void)atomicMax(out, arg);
  //
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  //
  arg = atomicAnd(out, arg);
  (void)atomicAnd(out, arg);
  arg = atomicXor(out, arg);
  (void)atomicXor(out, arg);
  arg = atomicOr(out, arg);
  (void)atomicOr(out, arg);
}

extern "C"
static __device__ __noinline__ void s64_atomics(
  int64_t *out, int64_t *_slm, int64_t arg)
{
  __shared__ int64_t slm[32];
  slm[threadIdx.x] = *out + 4;
  __syncthreads();

  arg = atomicMin(slm + arg%32, arg);
  (void)atomicMin(slm + arg%32, arg);
  arg = atomicMax(slm + arg%32, arg);
  (void)atomicMax(slm + arg%32, arg);
  //
  // odd that they include bitwise, but not addition
  // (signed and unsigned are the same for both)
  //
  // arg = atomicAdd(slm, arg);
  // (void)atomicAdd(slm, arg);
  //
  arg = atomicAnd(slm + arg%32, arg);
  (void)atomicAnd(slm + arg%32, arg);
  arg = atomicXor(slm + arg%32, arg);
  (void)atomicXor(slm + arg%32, arg);
  arg = atomicOr(slm + arg%32, arg);
  (void)atomicOr(slm + arg%32, arg);

  __syncthreads();

  // arg = atomicCAS(out, arg+1, arg);
  //
  arg = atomicMin(out, arg);
  (void)atomicMin(out, arg);
  arg = atomicMax(out, arg);
  (void)atomicMax(out, arg);
  //
  // odd that they include bitwise, but not addition
  // arg = atomicAdd(out, arg);
  // (void)atomicAdd(out, arg);
  //
  arg = atomicAnd(out, arg);
  (void)atomicAnd(out, arg);
  arg = atomicXor(out, arg);
  (void)atomicXor(out, arg);
  arg = atomicOr(out, arg);
  (void)atomicOr(out, arg);
}

extern "C"
__device__ __noinline__ void f16_atomics(
  __half *out, __half *slm, __half arg)
{
  // even with local SLM the compiler generates generic ATOM
  // __shared__ __half slm[32];
  // slm[threadIdx.x] = *(out + 1);
  // __syncthreads();

  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);

  __syncthreads();

  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
}

extern "C"
__device__ __noinline__ void f16x2_atomics(
  __half2 *out, __half2 *slm, __half2 arg)
{
  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);

  __syncthreads();

  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
}

extern "C"
__device__ __noinline__ void f32_atomics(
  float *out, float *slm, float arg)
{
  arg = atomicExch(slm, arg);
  //
  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);

  __syncthreads();

  arg = atomicExch(out, arg);
  // atomicMin(out, arg);
  // atomicMax(out, arg);
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);

  // atomicSub(out, arg);
  //
  // "Unimplemented feature: floating-point reduction operation"
  // no f32 min
  // asm("red.global.min.f32 [%0], %1;" :: "l"(out), "f"(arg));
  // asm("atom.global.min.f32 %0, [%1], %2;" : "=f"(arg) : "l"(out), "f"(arg));
  // *out += arg;
}


extern "C"
static __device__ __noinline__ void f64_atomics(
  double *out, double *slm, double arg)
{
  arg = atomicAdd(slm, arg);
  (void)atomicAdd(slm, arg);

  __syncthreads();

  // arg = atomicExch(out, arg);
  // atomicMin(out, arg);
  // atomicMax(out, arg);
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  // atomicSub(out, arg);
}



extern "C" __global__ void run_atomics(
    __half *f16OUT,
    const __half *f16A,

    __half2 *f16x2OUT,
    const __half2 *f16x2A,

    float *f32OUT,
    const float *f32A,

    double *f64OUT,
    const double *f64A,

    int32_t *i32OUT,
    const int32_t *i32A,

    uint32_t *u32OUT,
    const uint32_t *u32A,

    uint16_t *u16OUT,
    const uint16_t *u16A,

    int64_t *s64OUT,
    const int64_t *s64A,

    uint64_t *u64OUT,
    const uint64_t *u64A)
{
  __shared__ uint16_t u16_slm[32];
  __shared__ uint32_t u32_slm[32];
  __shared__ uint64_t u64_slm[32];
  __shared__ float f32_slm[32];
  __shared__ double f64_slm[32];

  int id = blockDim.x * blockIdx.x + threadIdx.x;
  int sid = threadIdx.x % 32;
  int sid2 = ((sid+1)%32);
  //
  u16_slm[sid] = u16A[id];
  u32_slm[sid] = u32A[id];
  u64_slm[sid] = u64A[id];
  f32_slm[sid] = f32A[id];
  f64_slm[sid] = f64A[id];
  //
  __syncthreads();
  //
  u16_atomics(u16OUT+id, u16_slm+sid2, u16A[id]);
  //
  s32_atomics(i32OUT+id, (int *)u32_slm+sid2, i32A[id]);
  //
  u32_atomics(u32OUT+id, u32_slm+sid2, u32A[id]);
  u32_atomics_system(u32OUT+id, u32_slm+sid2, u32A[id]);
  u32_atomics_block(u32OUT+id, u32_slm+sid2, u32A[id]);
  //
  s64_atomics(s64OUT+id, (int64_t*)u64_slm+sid2, s64A[id]);
  //
  u64_atomics(u64OUT+id, u64_slm+sid2, u64A[id]);
  //
  f16_atomics(f16OUT+id, (__half *)(u16_slm+sid), f16A[id]);
  //
  f16x2_atomics(f16x2OUT+id, (__half2 *)u32_slm+sid2, f16x2A[id]);
  //
  f32_atomics(f32OUT+id, f32_slm+sid2, f32A[id]);
  // f32_atomics_slm(f32_slm+sid2,f32A[id]);
  //
  f64_atomics(f64OUT+id, f64_slm+sid2, f64A[id]);
  //
  __syncthreads();

  u16OUT[sid] += u16_slm[threadIdx.x];
  u32OUT[sid] += u32_slm[threadIdx.x];
  u64OUT[sid] += u64_slm[threadIdx.x];
  f32OUT[sid] += f32_slm[threadIdx.x];
  f64OUT[sid] += f64_slm[threadIdx.x];
}
/*
extern "C" __global__ void run_atomics_inline_ptx(
    __half *f16OUT,
    const __half *f16A,

    __half2 *f16x2OUT,
    const __half2 *f16x2A,

    float *f32OUT,
    const float *f32A,

    double *f64OUT,
    const double *f64A,

    int32_t *i32OUT,
    const int32_t *i32A,

    uint32_t *u32OUT,
    const uint32_t *u32A,

    uint16_t *u16OUT,
    const uint16_t *u16A,

    int64_t *s64OUT,
    const int64_t *s64A,

    uint64_t *u64OUT,
    const uint64_t *u64A)
{
  __shared__ uint16_t u16_slm[32];
  __shared__ uint32_t u32_slm[32];
  __shared__ float f32_slm[32];

  int id = blockDim.x * blockIdx.x + threadIdx.x;
  int sid = threadIdx.x % 32;
  u32_slm[sid] = u32A[id];
  f32_slm[sid] = f32A[id];
  __syncthreads();

  uint16_t r = 0, s = 0, t = 1;
  asm("atom.global.cas.b16 %0, [%1], %2, %3;" :
    "+h"(r) : "l"(u16OUT+id), "h"(s), "h"(t));
  asm("atom.shared.cas.b16 %0, [%1], %2, %3;" :
    "+h"(r) : "l"(u16_slm+sid), "h"(s), "h"(t));

  __syncthreads();

  u16OUT[id] = r + s + t + u16_slm[sid];
  u32OUT[id] = u32_slm[sid];
  f32OUT[id] = f32_slm[sid];
}
*/