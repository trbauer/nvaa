#include "mincu.hpp"

#define K 64

using namespace mincu;


static __device__ uint64_t get_time()
{
  uint64_t out = 0;
  asm volatile("mov.u64 %0, %%clock64;" : "=l"(out));
  return out;
}

/*
static __device__ inline uint32_t get_time32()
{
  uint32_t mclk;
  asm volatile("mov.u32 %0, %%clock;" : "=r"(mclk));
  return mclk;
}
*/

static void __global__ fma_lat_reg(
        float *out,
  const float *in)
{
  auto tid = K * threadIdx.x;
  float reg = (float)tid;
  float sum = 0.0f;
  auto rt_stt = clock64();
  for (auto i = 0; i < K; i++) {
    sum += in[tid + i] * reg;
  }
  auto rt_end = clock64();
  out[tid] = sum;
  printf("T: %lld\n", (rt_end - rt_stt) / 64);
}

int main(int argc, char **argv)
{
  umem<float> oups(1, const_seq<float>(0.0f));
  umem<float> inps((size_t)1, arith_seq<float4>(make_float4(0,0,0,0)));
  fma_lat_reg<<<1,1>>>(oups, inps);
  return EXIT_SUCCESS;
}

