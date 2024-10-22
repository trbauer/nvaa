#include "mincu.hpp"


using namespace mincu;


static __device__ uint64_t get_time()
{
  uint64_t out = 0;
  asm volatile("mov.u64 %0, %%clock64;" : "=l"(out) :: "memory");
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
typedef uint64_t ulong;
typedef uint32_t uint;

static __global__ void ffma_r(
        ulong *out_time,
        float  *out,
  const float4 *in0)
{
  const int K = 64 * 1024;

  // uint gid = get_global_id(0);
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  float reg = (float)threadIdx.x;
  float sum = 0.0f;
  ulong rt_sum = 0;
  const float4 *in_ptr = in0 + gid;
  // #pragma nounroll
  #pragma unroll 1
  for (uint i = 0; i < K / 16; i++) {
    // load16; time(use16)
    float4 acc[4];
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      acc[k] = *in_ptr++;
    }
    ulong rt_stt = get_time();
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      sum += acc[k].x * reg;
      sum += acc[k].y * reg;
      sum += acc[k].z * reg;
      sum += acc[k].w * reg;
    }
    rt_sum += get_time() - rt_stt;
  }
  out[gid] = sum;
  // printf("T: %lld\n", rt_sum / K);
  out_time[0] = rt_sum / K;
}

static __global__ void ffma_c(
        ulong *out_time,
        float  *out,
  const float4 *in0,
        float konst)
{
  const int K = 64 * 1024;

  // uint gid = get_global_id(0);
  const size_t gid = blockIdx.x * blockDim.x + threadIdx.x;
  float sum = 0.0f;
  ulong rt_sum = 0;
  const float4 *in_ptr = in0 + gid;
  // #pragma nounroll
  #pragma unroll 1
  for (uint i = 0; i < K / 16; i++) {
    // load16; time(use16)
    float4 acc[4];
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      acc[k] = *in_ptr++;
    }
    ulong rt_stt = get_time();
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      sum += acc[k].x * konst;
      sum += acc[k].y * konst;
      sum += acc[k].z * konst;
      sum += acc[k].w * konst;
    }
    rt_sum += get_time() - rt_stt;
  }
  out[gid] = sum;
  // printf("T: %lld\n", rt_sum / K);
  out_time[0] = rt_sum / K;
}


int main(int argc, char **argv)
{
  umem<uint64_t> out_times[] {
    umem<uint64_t>{1, const_seq<uint64_t>(0)},
    umem<uint64_t>{1, const_seq<uint64_t>(0)},
    umem<uint64_t>{1, const_seq<uint64_t>(0)},
  };
  umem<float> oups(1, const_seq<float>(0.0f));
  umem<float4> inps((size_t)1, arith_seq<float4>());
  std::vector<float> elapsed_ss;
  std::string arg = argv[1];
  if (argc != 2) {
    fatal("expected 'c' or 'r' as argument");
  }
  if (arg == "r") {
    elapsed_ss = time_dispatches_s({
      [&] {ffma_r<<<1,1>>>(out_times[0], oups, inps);},
      [&] {ffma_r<<<1,1>>>(out_times[1], oups, inps);},
      [&] {ffma_r<<<1,1>>>(out_times[2], oups, inps);},
      });
  } else if (arg == "c") {
    elapsed_ss = time_dispatches_s(
      {
      [&] {ffma_c<<<1,1>>>(out_times[0], oups, inps, 0.5f);},
      [&] {ffma_c<<<1,1>>>(out_times[1], oups, inps, 0.5f);},
      [&] {ffma_c<<<1,1>>>(out_times[2], oups, inps, 0.5f);},
      });
  } else {
    fatal("expected 'c' or 'r' as argument");
  }
  auto e0 = cudaDeviceSynchronize();
  if (e0 != cudaSuccess) {
    fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
  }
  for (size_t i = 0; i < elapsed_ss.size(); i++) {
    std::cout << format("elapsed_s[", i, "]: ",
                        frac(elapsed_ss[i], 3), ", ", (int64_t)out_times[i][0], " c\n");
  }
  return EXIT_SUCCESS;
}
