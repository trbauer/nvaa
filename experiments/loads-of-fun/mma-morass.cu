#include <cstdio>

#include <cuda_runtime.h>

/*
__device__ uint2 mma_m8n8k16_rc_satfinite_u8(uint32_t a, uint32_t b, uint2 c)
{
  uint2 d;
  asm volatile(
    "// s8 elements in A and u8 elements in B"
    "mma.sync.aligned.m8n8k16.row.col.satfinite.s32.s8.u8.s32"
    "  {%0, %1},"
    "  {%2},"
    "  {%3},"
    "  {%4, %5};"
      : "=r"(d.x), "=r"(d.y)
      : "r"(a), "r"(b), "r"(c.x), "r"(c.y) : "memory");
  return d;
}
__noinline__ __device__ uint2 mma_m8n8k32_rc_satfinite_u4(uint32_t a, uint32_t b, uint2 c)
{
  uint2 d;
  asm volatile (R"(
    mma.sync.aligned.m8n8k32.row.col.satfinite.s32.u4.u4.s32
      {%0, %1},{%2},{%3},{%4, %5};)"
        : "=r"(d.x), "=r"(d.y)
        : "r"(a), "r"(b), "r"(c.x), "r"(c.y) : "memory");
  return d;
}

__noinline__ __device__ uint2 mma_m8n8k32_rc_overflow_u4(uint32_t a, uint32_t b, uint2 c)
{
  uint2 d;
  asm volatile (R"(
    mma.sync.aligned.m8n8k32.row.col.s32.u4.u4.s32
      {%0, %1},{%2},{%3},{%4, %5};)"
        : "=r"(d.x), "=r"(d.y)
        : "r"(a), "r"(b), "r"(c.x), "r"(c.y) : "memory");
  return d;
}


*/
__noinline__ __device__ uint2 mma_m8n8k16_rc_overflow_u8(uint32_t a, uint32_t b, uint2 c)
{
  uint2 d;
  asm volatile (R"(
    mma.sync.aligned.m8n8k16.row.col.s32.u8.u8.s32
      {%0, %1},{%2},{%3},{%4, %5};)"
        : "=r"(d.x), "=r"(d.y)
        : "r"(a), "r"(b), "r"(c.x), "r"(c.y) : "memory");
  return d;
}

__noinline__ __device__ uint2 mma_m8n8k16_rc_satfinite_u8(uint32_t a, uint32_t b, uint2 c)
{
  uint2 d;
  asm volatile (R"(
    mma.sync.aligned.m8n8k16.row.col.satfinite.s32.u8.u8.s32
      {%0, %1},{%2},{%3},{%4, %5};)"
        : "=r"(d.x), "=r"(d.y)
        : "r"(a), "r"(b), "r"(c.x), "r"(c.y) : "memory");
  return d;
}

/*
.reg .s32 s32d<8>, s32a<8>;
.reg .u32 u32a<8>;
.reg .pred scaleD;
.reg .b64   descA, descB;

wgmma.mma_async.sync.aligned.m64n8k32.s32.s8.s8.satfinite
  {s32d0, s32d1, s32d2, s32d3},
  {s32a0, s32a1, s32a2, s32a3},
  descB,
  1;
*/
/*
__global__ void example_mma(
  uint2 * __restrict__ ds,
  const uint32_t * __restrict__ as,
  const uint32_t * __restrict__ bs,
  const uint2 * __restrict__ cs)
{
  uint32_t a = as[threadIdx.x];
  uint32_t b = bs[threadIdx.x];
  uint2 c = cs[threadIdx.x];

  // uint2 d = mma_m8n8k32_rc_overflow_u4(a, b, c);
  // uint2 d = mma_m8n8k32_rc_satfinite_u4(a, b, c);
  // uint2 d = mma_m8n8k16_rc_overflow_u8(a, b, c);
  // uint2 d = mma_m8n8k16_rc_satfinite_u8(a, b, c);

  ds[threadIdx.x] = d;
}

////////////////////////////////////////////////////////////////////////////////

__noinline__ __device__ uint4 wgmma_m64n8k32_s8(uint4 a, const void *descB)
{
  uint4 d;
  asm volatile (R"(
    wgmma.mma_async.sync.aligned.m64n8k32.s32.s8.s8
      {%0, %1, %2, %3},
      {%4, %5, %6, %7},
      %8,
      1;)"
        : "=r"(d.x), "=r"(d.y), "=r"(d.z), "=r"(d.w)
        : "r"(a.x), "r"(a.y), "r"(a.z), "r"(a.w),
          "l"(descB)
        : "memory");
  return d;
}

__noinline__ __device__ uint4 wgmma_m64n8k32_s4(uint4 a, const void *descB)
{
  uint4 d;
  asm volatile (R"(
    wgmma.mma_async.sync.aligned.m64n8k32.s32.s8.s8
      {%0, %1, %2, %3},
      {%4, %5, %6, %7},
      %8,
      1;)"
        : "=r"(d.x), "=r"(d.y), "=r"(d.z), "=r"(d.w)
        : "r"(a.x), "r"(a.y), "r"(a.z), "r"(a.w),
          "l"(descB)
        : "memory");
  return d;
}

__noinline__ __device__ uint4 wgmma_m64n8k32_s8_satfinite(uint4 a, const void *descB)
{
  uint4 d;
  asm volatile (R"(
    wgmma.mma_async.sync.aligned.m64n8k32.s32.s8.s8.satfinite
      {%0, %1, %2, %3},
      {%4, %5, %6, %7},
      %8,
      1;)"
        : "=r"(d.x), "=r"(d.y), "=r"(d.z), "=r"(d.w)
        : "r"(a.x), "r"(a.y), "r"(a.z), "r"(a.w),
          "l"(descB)
        : "memory");
  return d;
}

__noinline__ __device__ uint4 wgmma_m64n8k32_u8_s8(
    uint4 a, const void *descB, bool negate)
{
  uint4 d;
  asm volatile (R"(
    {
    .reg .pred prNeg;
    setp.ne.b32  prNeg, %9, 0;
    wgmma.mma_async.sync.aligned.m64n8k32.s32.u8.s8
      {%0, %1, %2, %3},
      {%4, %5, %6, %7},
      %8,
      prNeg;
      })"
        : "=r"(d.x), "=r"(d.y), "=r"(d.z), "=r"(d.w)
        : "r"(a.x), "r"(a.y), "r"(a.z), "r"(a.w),
          "l"(descB), "r"(negate ? 1 : 0)
        : "memory");
  return d;
}

__global__ void example_wgmma(
  uint4 * __restrict__ ds,
  const uint4 * __restrict__ as,
//  const uint4 * __restrict__ bs,
//  const uint4 * __restrict__ cs,
  const void *descB,
  bool neg)
{
  uint4 a = as[threadIdx.x];

  // uint4 d = wgmma_m64n8k32_s8_satfinite(a, descB);
  // uint4 d = wgmma_m64n8k32_s8(a, descB);
  uint4 d = wgmma_m64n8k32_u8_s8(a, descB, neg);


  ds[threadIdx.x] = d;
}
*/
struct float8
{
  float s[8];
};

__noinline__ __device__ float8 wgmma_m64n16k8_f32_tf32_tf32(
    const void *descA, const void *descB)
{
  float8 d;
  asm volatile (R"({
    wgmma.mma_async.sync.aligned.m64n16k8.f32.tf32.tf32
      {%0, %1, %2, %3, %4, %5, %6, %7},
      descA,
      descB,
      0, -1, -1;
          })"
            : "=r"(d.s[0]), "=r"(d.s[1]), "=r"(d.s[2]), "=r"(d.s[3]),
              "=r"(d.s[4]), "=r"(d.s[5]), "=r"(d.s[6]), "=r"(d.s[7])
            : "l"(descA),"l"(descB)
            : "memory");
  return d;
}

__global__ void example_wgmma(
  float8 * __restrict__ ds,
//  const float8 * __restrict__ as,
  const void *descA,
  const void *descB,
  bool neg)
{
  float8 d = wgmma_m64n16k8_f32_tf32_tf32(descA, descB);
  ds[threadIdx.x] = d;
}

/*
// https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-mma

.reg .b32 %Ra, %Rb, %Rc<2>, %Rd<2>;

// s8 elements in A and u8 elements in B
mma.sync.aligned.m8n8k16.row.col.satfinite.s32.s8.u8.s32
  {%Rd0, %Rd1},
  {%Ra},
  {%Rb},
  {%Rc0, %Rc1};
 */