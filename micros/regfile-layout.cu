// D.run ["micros/regfile-layout.cu","--arch=sm_75","-o=micros/sm_75/regfile-layout.sass","-lines","--save-ptx=micros/sm_75/regfile-layout.ptx"]
#include <cuda_fp16.h>

extern "C" __global__ void add_s8(
    const char *A,
    char *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 1;
  OUT[id] = a;
}
extern "C" __global__ void add_u8(
    const unsigned char *A,
    unsigned char *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 1;
  OUT[id] = a;
}
extern "C" __global__ void add_u8x4(
    const uchar4 *A,
    const uchar4 *B,
    uchar4 *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id], b = B[id];
  auto t1 = make_uchar4(a.x+b.x,a.y+b.y,a.z+b.z,a.w+b.w);
  auto t2 = make_uchar4(t1.x+0x88,t1.y+0x88,t1.z+0x88,t1.w+0x88);
  OUT[id] = t2;
}
extern "C" __global__ void add_s16(
    const short *A,
    short *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 1;
  OUT[id] = a;
}
extern "C" __global__ void add_u16(
    const unsigned short *A,
    unsigned short *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 1;
  OUT[id] = a;
}

extern "C" __global__ void add_u16x2(
    const short2 *A,
    const short2 *B,
    short2 *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id], b = B[id];
  auto t1 = make_short2(a.x + b.x, a.y + b.y);
  auto t2 = make_short2(t1.x + 0x1616, t1.y + 0x1616);
  OUT[id] = t2;
}

extern "C" __global__ void add_f16(
    const __half *A,
    const __half *B,
    __half *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + B[id];
  OUT[id] = a;
}
extern "C" __global__ void add_f16x2(
    const __half2 *A,
    const __half2 *B,
    __half2 *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + B[id];
  OUT[id] = a;
}


extern "C" __global__ void add_s64(
    const long long *A,
    long long *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 0x33;
  OUT[id] = a;
}
extern "C" __global__ void add_u64(
    const unsigned long long *A,
    unsigned long long *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id] + 0x44;
  OUT[id] = a;
}