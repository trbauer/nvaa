#include <cstdint>
#include <cuda_fp16.h>

__device__ __noinline__ void f32_atomics(
  float *out, float arg)
{
  arg = atomicExch(out, arg);
  // atomicMin(out, arg);
  // atomicMax(out, arg);
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  // atomicSub(out, arg);
}
__device__ __forceinline__ void  f32_atomics_slm(
  float *out, float arg)
{
  arg = atomicExch(out, arg);
  //
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
  // atomicSub(out, arg);
}
__device__ __noinline__ void f16_atomics(
  __half *out, __half arg)
{
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
}
__device__ __noinline__ void f16x2_atomics(
  __half2 *out, __half2 arg)
{
  arg = atomicAdd(out, arg);
  (void)atomicAdd(out, arg);
}

__device__ __noinline__ void u16_atomics(
  uint16_t *out, uint16_t arg)
//  unsigned short int *out, unsigned short int arg)
{
//unsigned short int atomicCAS(unsigned short int *address,
//                             unsigned short int compare,
//                             unsigned short int val);
//  *out = atomicCAS(out, arg, (uint16_t)(arg+1));

//  *out = atomicCAS(out, arg, (unsigned short int)(arg+1));
}


__device__ __noinline__ void s32_atomics(
  int32_t *out, int32_t arg)
{
  arg = atomicCAS(out, arg+1, arg);
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

__device__ __noinline__ void u32_atomics(
  uint32_t *out, uint32_t arg)
{
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
__device__ __noinline__ void u32_atomics_system(
  uint32_t *out, uint32_t arg)
{
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
__device__ __noinline__ void u32_atomics_block(
  uint32_t *out, uint32_t arg)
{
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

// force inline to hopefully get an ATOMS message
__device__ __forceinline__ void u32_atomics_slm(
  uint32_t *out, uint32_t arg)
{
  arg = atomicCAS_block(out, arg+1, arg);
  (void)atomicExch_block(out, arg);
  //
  arg = atomicMin_block(out, arg);
  (void)atomicMin_block(out, arg);
  arg = atomicMax_block(out, arg);
  (void)atomicMax_block(out, arg);
  //
  arg = atomicInc_block(out, arg);
  (void)atomicInc_block(out, arg);
  arg = atomicDec_block(out, arg);
  (void)atomicDec_block(out, arg);
  //
  arg = atomicAdd_block(out, arg);
  (void)atomicAdd_block(out, arg);
  arg = atomicSub_block(out, arg);
  (void)atomicSub_block(out, arg);
  //
  arg = atomicAnd_block(out, arg);
  (void)atomicAnd_block(out, arg);
  arg = atomicXor_block(out, arg);
  (void)atomicXor_block(out, arg);
  arg = atomicOr_block(out, arg);
  (void)atomicOr_block(out, arg);
}

__device__ __noinline__ void u64_atomics(
  uint64_t *out, uint64_t arg)
{
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



extern "C" __global__ void run_atomics(
    float *f32OUT,
    const float *f32A,

    __half *f16OUT,
    const __half *f16A,
    __half2 *f16x2OUT,
    const __half2 *f16x2A,

    int32_t *i32OUT,
    const int32_t *i32A,

    uint32_t *u32OUT,
    const uint32_t *u32A,

    uint16_t *u16OUT,
    const uint16_t *u16A,

    uint64_t *u64OUT,
    const uint64_t *u64A)
{
  __shared__ uint32_t u32_slm[32];
  __shared__ float f32_slm[32];

  int id = blockDim.x * blockIdx.x + threadIdx.x;
  int sid = threadIdx.x % 32;
  int sid2 = ((sid+1)%32);
  //
  u32_slm[sid] = u32A[id];
  f32_slm[sid] = f32A[id];
  __syncthreads();
  //
  u16_atomics(u16OUT+id, u16A[id]);
  //
  s32_atomics(i32OUT+id, i32A[id]);
  //
  u32_atomics(u32OUT+id, u32A[id]);
  u32_atomics_system(u32OUT+id, u32A[id]);
  u32_atomics_block(u32OUT+id, u32A[id]);
  u32_atomics_slm(u32_slm+sid2,u32A[id]);
  //
  u64_atomics(u64OUT+id, u64A[id]);
  //
  f16_atomics(f16OUT+id, f16A[id]);
  //
  f16x2_atomics(f16x2OUT+id, f16x2A[id]);
  //
  f32_atomics(f32OUT+id, f32A[id]);
  f32_atomics_slm(f32_slm+sid2,f32A[id]);
  //
  __syncthreads();
  u32OUT[sid] += u32_slm[threadIdx.x];
  f32OUT[sid] += f32_slm[threadIdx.x];
}



