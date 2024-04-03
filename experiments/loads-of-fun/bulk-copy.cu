#include <cuda_runtime.h>
#include <cuda/barrier>
#include <cooperative_groups.h>

#include <cstdint>


extern "C" __global__ void example_bulk_group_g_to_d(
  int32_t *dsts, const int32_t *src0s, uint32_t array_mask)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int blk_start = 128 * blockIdx.x * blockDim.x;
  auto src0 = src0s[gid];

  __shared__ cuda::barrier<cuda::thread_scope_block> bar;
  __shared__ alignas(alignof(int32_t)) int32_t smem[1024];

  auto block = cooperative_groups::this_thread_block();
  if (threadIdx.x == 0) {
    init(&bar, 32);
  }
  __syncthreads();

  if (threadIdx.x == 0) {
    asm volatile (R"(
      {
      cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [%2+  0], [%1+  0], 128, [%0];
      cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [%2+128], [%1+128], 128, [%0];
      cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes.multicast::cluster
                                               [%2+256], [%1+256], 128, [%0], 0xF0AA;
        }
       )" :: "l"(&bar), "l"((const uint8_t *)src0s + gid), "l"(&smem) : "memory");
   }

  //  // .nv_debug_ptx_txt:76:           cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [%rd13+  0], [%rd2+  0], 128, [%rd11];
  //      PLOP3.(s0&s1&s2)  P0, PT, PT,     PT,     PT,     0x80,   0x0      {!1};
  //  // .nv_debug_ptx_txt:76:           cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [%rd13+  0], [%rd2+  0], 128, [%rd11];
  //      UMOV      UR6,    0x8                              {!1};
  //.L_x_2:
  //@P0   ELECT     P1,     URZ,    PT                       {!13,Y};
  //@P1   R2UR      UR8,    R4                               {!2};
  //@P1   R2UR      UR9,    R5                               {!2};
  //@P1   PLOP3.(~s0&s1&s2)  P0, PT, P1,    PT,     PT,     0x8,    0x0      {!2};
  //      PLOP3.(~s0&s1&s2)  P1, PT, PT,    PT,     PT,     0x8,    0x0      {!9,Y};
  //      UBLKCP.S.G  [UR4], [UR8], UR6                      {!2,+1.R};
  //@P0   BRA.U.ANY  `(.L_x_2)                               {!5,^1};
  //
  // ... [later compile]
  //       PLOP3.(s0&s1&s2)  P0, PT, PT,     PT,     PT,     0x80,   0x0      {!1};
  // ..
  // .L_x_6:
  // @P0   ELECT     P1,     URZ,    PT                       {!13,Y};
  // @P1   R2UR      UR8,    R2                               {!2};
  // @P1   R2UR      UR9,    R3                               {!2};
  // @P1   PLOP3.(~s0&s1&s2)  P0, PT, P1,    PT,     PT,     0x8,    0x0      {!2};
  //       PLOP3.(~s0&s1&s2)  P1, PT, PT,    PT,     PT,     0x8,    0x0      {!9,Y};
  //       UBLKCP.S.G.MULTICAST  [UR6], [UR8], UR4            {!2,+1.R};
  // @P0   BRA.U.ANY  `(.L_x_6)                               {!5,^1};


  bar.wait(bar.arrive());
  dsts[gid] = smem[threadIdx.x];

}


extern "C" __global__ void example_bulk_group_s_to_g(
  int32_t *dsts, const int32_t *src0s, uint32_t array_mask)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int blk_start = 128 * blockIdx.x * blockDim.x;
  auto src0 = src0s[gid];

  __shared__ alignas(alignof(int32_t)) int32_t smem[256];

  smem[threadIdx.x] = threadIdx.x;

  __syncthreads();


  if (threadIdx.x == 0) {
    asm volatile (R"(
      {
      cp.async.bulk.global.shared::cta.bulk_group [%0+  0], [%1+  0], 512;
      cp.async.bulk.global.shared::cta.bulk_group [%0+512], [%1+512], 512;
        }
       )" ::  "l"((const uint8_t *)src0s + gid), "l"(&smem) : "memory");
  }

  // ...
  //      PLOP3.(s0&s1&s2)  P0, PT, PT,     PT,     PT,     0x80,   0x0      {!2};
  // ...
  //      UMOV      UR8,    0x20                             {!3,Y};
  // .L_x_2:
  //   // .nv_debug_ptx_txt:59:           cp.async.bulk.global.shared::cta.bulk_group [%rd3+  0], [%rd4+  0], 512;
  // @P0   R2UR      P1,     UR10,   R2                       {!2};
  // @P0   R2UR.OR   P1,     UR11,   R3                       {!8,Y};
  // @P0   PLOP3.(s0&s1&s2)  P0, PT, P1,     PT,     PT,     0x80,   0x0      {!2};
  //       PLOP3.(s0&s1&s2)  P1, PT, PT,     PT,     PT,     0x80,   0x0      {!3,Y};
  //       UBLKCP.G.S  [UR10], [UR5], UR8                     {!8,+2.R};
  // @P0   BRA.U.ANY  `(.L_x_2)                               {!5,^2};

  asm volatile("cp.async.bulk.commit_group;" ::: "memory");
  asm volatile("cp.async.bulk.wait_group.read 0;" ::: "memory");
  asm volatile("cp.async.bulk.wait_group 0;" ::: "memory");


  dsts[gid] = smem[threadIdx.x];
}



extern "C" __global__ void example_bulk_group_s_to_dsm(
  int32_t *dsts, const int32_t *src0s, uint32_t array_mask)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int blk_start = 128 * blockIdx.x * blockDim.x;
  auto src0 = src0s[gid];

  __shared__ alignas(4*alignof(int32_t)) int32_t smem[1024];
  extern __shared__ alignas(4 * alignof(int32_t)) int32_t smem2;

  __shared__ cuda::barrier<cuda::thread_scope_block> bar;
  if (threadIdx.x == 0) {
    init(&bar, 32);
  }

  smem[threadIdx.x] = src0;

  __syncthreads();


  if (threadIdx.x == 0) {
    asm volatile (R"(
      {
      cp.async.bulk.shared::cluster.shared::cta.mbarrier::complete_tx::bytes [%2+  0], [%1+  0], 128, [%0];
      cp.async.bulk.shared::cluster.shared::cta.mbarrier::complete_tx::bytes [%2+128], [%1+128], 128, [%0];
      })" :: "l"(&bar), "l"((const uint8_t *)smem2), "l"(&smem) : "memory");
  }

  bar.wait(bar.arrive());

//       PLOP3.(s0&s1&s2)  P0, PT, PT,     PT,     PT,     0x80,   0x0      {!1};
// ..
// .L_x_4:
//   // .nv_debug_ptx_txt:80:           cp.async.bulk.shared::cluster.shared::cta.mbarrier::complete_tx::bytes [%rd9+  0], [%rd8+  0], 128, [%rd7];
// @P0   ELECT     P1,     URZ,    PT                       {!1};
//       UBLKCP.S.S  [UR10], [UR4], UR5                     {!12,Y,+1.R};
// @P1   PLOP3.(~s0&s1&s2)  P0, PT, P1,    PT,     PT,     0x8,    0x0      {!2};
//       PLOP3.(~s0&s1&s2)  P1, PT, PT,    PT,     PT,     0x8,    0x0      {!11,Y};
// @P0   BRA.U.ANY  `(.L_x_4)                               {!5,^1};

  dsts[gid] = smem[(threadIdx.x + 1) % 1024];
}



extern "C" __global__ void example_bulk_group_reduce_s_to_g(
  int32_t *dsts, const int32_t *src0s, uint32_t array_mask)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const int blk_start = 128 * blockIdx.x * blockDim.x;
  auto src0 = src0s[gid];

  __shared__ alignas(alignof(int32_t)) int32_t smem[512];

  smem[threadIdx.x] = src0;

  __syncthreads();


  if (threadIdx.x == 0) {
    asm volatile (R"(
      {
      cp.reduce.async.bulk.global.shared::cta.bulk_group.add.f32 [%0+  0], [%1+  0], 128;
      cp.reduce.async.bulk.global.shared::cta.bulk_group.add.f64 [%0+128], [%1+128], 128;
      cp.async.bulk.commit_group;
      cp.async.bulk.wait_group.read 0;
      cp.reduce.async.bulk.global.shared::cta.bulk_group.add.noftz.bf16 [%0+256], [%1+256], 128;
      })" ::  "l"((const uint8_t *)blk_start), "l"(&smem) : "memory");
  }
  // usual loops with
  //      UBLKRED.G.S.ADD.F32.RN  [UR6], [UR8], UR9          {!12,Y,+2.R};
  //      UBLKRED.G.S.ADD.F64.RN  [UR8], [UR15], UR16        {!12,Y,+2.R};
  //      UBLKRED.G.S.ADD.BF16.RN  [UR6], [UR4], UR5         {!12,Y,+2.R};
  //
  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-cp-reduce-async-bulk

  // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-cp-async-bulk-prefetch
  if (threadIdx.x == 0) {
    asm volatile (R"(
      {
        cp.async.bulk.prefetch.L2.global   [%0], 512;
      })" ::  "l"((const uint8_t *)blk_start) : "memory");
  }
  // 512 causes UR4 to be 0x20, 256 is 0x10 (units of 16B)
    // UMOV      UR4,    0x10                             {!2};
    // UBLKPF.L2  [UR6], UR4                              {!2,+2.R};

  asm volatile("cp.async.bulk.commit_group;" ::: "memory");
//  asm volatile("cp.async.bulk.wait_group.read 0;" ::: "memory");
  asm volatile("cp.async.bulk.wait_group 0;" ::: "memory");

  dsts[gid] = smem[threadIdx.x];
}


