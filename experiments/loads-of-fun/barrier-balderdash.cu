extern "C" __global__ void test_ifp_reg(
  uint32_t *dsts, const int32_t *src0s)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int laneId = threadIdx.x % 32;
  auto src0 = src0s[gid];

  int32_t dst = 0;
  int next_lane = (laneId + 1) % 32;
  if (threadIdx.x % 2) {
    src0 += 1;
    // __syncwarp(0xFFFFFFFF);
    dst = __shfl_sync(0xFFFFFFFF, src0, next_lane);
  } else {
    src0 += 2;
    // __syncwarp(0xFFFFFFFF);
    dst = __shfl_sync(0xFFFFFFFF, src0, next_lane);
  }
  // dst = src0 + ((threadIdx.x % 2) ? 1 : 2)

  dsts[gid] = dst;
}

/*
extern "C" __global__ void test_ifp_smem(
  uint32_t *dsts, const int32_t *src0s)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int laneId = threadIdx.x % 32;
  auto src0 = src0s[gid];

  __shared__ int32_t smem[64];
  for (int t = threadIdx.x; t < sizeof(mem)/sizeof(mem[0]); t++) {
    smem[t] = src0s[blockIdx.x * blockDim.x + t];
  }

  int32_t dst = 0;
  int next_lane = (threadIdx.x + 1) % (sizeof(mem)/sizeof(mem[0]));
  if (threadIdx.x % 2) {
    smem[threadIdx.x]++;
    __syncwarp(0xFFFFFFFF); // smem low and high half are coherent
  } else {
    smem[threadIdx.x]--;
    __syncwarp(0xFFFFFFFF); // smem low and high half are coherent
  }

  dsts[gid] = dst;
}

extern "C" __global__ void test_ifp_fgatom(
  uint32_t *dsts, const int32_t *src0s)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int laneId = threadIdx.x % 32;
  auto src0 = src0s[gid];

  __shared__ int32_t smem[64];
  for (int t = threadIdx.x; t < sizeof(mem)/sizeof(mem[0]); t++) {
    smem[t] = src0s[blockIdx.x * blockDim.x + t];
  }

  int32_t dst = 0;
  int next_lane = (threadIdx.x + 1) % (sizeof(mem)/sizeof(mem[0]));
  if (threadIdx.x % 2) {
    for (int i = 0; i < iterations; i++) {
      // grab neighbor's lock
      while (atomicCAS(smem + next_lane, 0, 1) != 0)
        ;
    }
    smem[threadIdx.x]++;
    __syncwarp(0xFFFFFFFF); // smem low and high half are coherent
  } else {
    smem[threadIdx.x]--;
    __syncwarp(0xFFFFFFFF); // smem low and high half are coherent
  }

  dsts[gid] = dst;
}

#if __CUDA_ARCH__ >= 900
// move to mbarrier-embarrassment (embark)
extern "C"
__global__ void
async_adventures_mbarrier(
    uint32_t *dsts,
    const uint32_t *srcs,
    uint32_t src)
{
  // using barrier = cuda::barrier<cuda::thread_scope_block>;
  __shared__  cuda::barrier<cuda::thread_scope_block> bar0, bar1;

  __shared__ int32_t smem[8][64];

  if (block.thread_rank() == 0) {
    init(&bar0, n); // barrier for even channels
    init(&bar1, n); // barrier for odd channels
  }

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;

}
#endif // __CUDA_ARCH__ >= 900

*/

