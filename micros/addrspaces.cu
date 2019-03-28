__device__ __noinline__ bool testGlobal(
  const void *ptr)
{
  return __isGlobal(ptr) != 0;
}
__device__ __noinline__ bool testLocal(
  const void *ptr)
{
    unsigned int ret;
    asm volatile ("{ \n\t"
                  "    .reg .pred p; \n\t"
                  "    isspacep.local p, %1; \n\t"
                  "    selp.u32 %0, 1, 0, p;  \n\t"
#if (defined(_MSC_VER) && defined(_WIN64)) || defined(__LP64__) || defined(__CUDACC_RTC__)
                  "} \n\t" : "=r"(ret) : "l"(ptr));
#else
                  "} \n\t" : "=r"(ret) : "r"(ptr));
#endif

    return ret;
}
__device__ __noinline__ bool testShared(
  const void *ptr)
{
    unsigned int ret;
    asm volatile ("{ \n\t"
                  "    .reg .pred p; \n\t"
                  "    isspacep.shared p, %1; \n\t"
                  "    selp.u32 %0, 1, 0, p;  \n\t"
#if (defined(_MSC_VER) && defined(_WIN64)) || defined(__LP64__) || defined(__CUDACC_RTC__)
                  "} \n\t" : "=r"(ret) : "l"(ptr));
#else
                  "} \n\t" : "=r"(ret) : "r"(ptr));
#endif

    return ret;
}
__device__ __noinline__ bool testConstant(
  const void *ptr)
{
    unsigned int ret;
    asm volatile ("{ \n\t"
                  "    .reg .pred p; \n\t"
                  "    isspacep.const p, %1; \n\t"
                  "    selp.u32 %0, 1, 0, p;  \n\t"
#if (defined(_MSC_VER) && defined(_WIN64)) || defined(__LP64__) || defined(__CUDACC_RTC__)
                  "} \n\t" : "=r"(ret) : "l"(ptr));
#else
                  "} \n\t" : "=r"(ret) : "r"(ptr));
#endif

    return ret;
}


__device__ __noinline__ void query_func(
  float *out, const float *a)
{
  float x = *a;
  if (testGlobal(out)) {
    x += 1.0f;
  } else if (testLocal(out)) {
    x += 2.0f;
  } else if (testShared(out)) {
    x += 3.0f;
  } else if (testConstant(out)) {
    x += 4.0f;
  }
  *out = x;
}


//  cudaMemcpytoSymbol
__constant__ float C[32];

extern "C" __global__ void run_query_space(
    const float *A,
    float *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  float bs[16];
  auto bs_len = sizeof(bs)/sizeof(bs[0]);
  for (int i = 0; i < bs_len; i++) {
    if (id % 2) {
      bs[i] = A[id];
    } else {
      bs[i] = C[id];
    }
  }

  if (__isGlobal(OUT + id)) {
    bs[0] += 1.0f;
  }

  __shared__ float tile[32];
  tile[threadIdx.x] = A[2*id];
  __syncthreads();

  if (id % 4 == 0) {
    query_func(OUT + id, A + threadIdx.x);
  } else if (id % 4 == 1) {
    query_func(OUT + id, tile + threadIdx.x);
  } else if (id % 4 == 2) {
    query_func(OUT + id, C + threadIdx.x);
  } else {
    query_func(OUT + id, bs + threadIdx.x % bs_len);
  }
}
