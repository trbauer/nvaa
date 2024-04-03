// init function
// https://nvidia.github.io/cccl/libcudacxx/extended_api/synchronization_primitives/barrier/init.html

/*
#include <cuda_runtime.h>
#include <cuda/barrier>
#include <cooperative_groups.h>

#include "mincu.hpp"
using namespace mincu;

__device__ void emit_bar(const char *when, const uint32_t *ptr)
{
  printf("%s: mbar: 0x%08X'%08X\n", when, ptr[0], ptr[1]);
}

extern "C" __global__ void mbar_inline(
  int32_t *dsts, const int32_t *src0s, uint32_t array_mask)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto src0 = src0s[gid];

#pragma diag_suppress static_var_with_dynamic_init
  __shared__ cuda::barrier<cuda::thread_scope_block> bar;
  __shared__ alignas(alignof(int32_t)) int32_t smem[4 * 64];

  // volatile uint32_t *pbar = &bar;

  auto block = cooperative_groups::this_thread_block();
  int32_t sum = 0;
  if (threadIdx.x == 0) {
    init(&bar, 32);
    if (array_mask == 0) {
      asm volatile ("mbarrier.inval.shared.b64 [%0];" :: "l"(&bar) : "memory");
    }
  }
  __syncthreads();

  bar.wait(bar.arrive());

  dsts[gid] = sum;
}

int main(int argc, char **argv)
{
  static const size_t BLOCKS = 1; // 1 warp only
  static const size_t TPB = 32; // threads per block (1 warp)

  umem<int32_t> inps(4 * BLOCKS * TPB,
      [&](size_t ix) {return 'A' + (int32_t)ix;});
  umem<int32_t> oups(BLOCKS * TPB, const_seq<int32_t>(0));
  // std::cout << "inps:\n";
  // inps.str(std::cout, 8);

  mbar_inline<<<BLOCKS,TPB>>>(oups, inps, BLOCKS * TPB - 1);

  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
}
*/


#include <cuda/barrier>
#include <cuda/std/utility> // cuda::std::move

/*
#if defined(__CUDA_MINIMUM_ARCH__) && __CUDA_MINIMUM_ARCH__ < 900
static_assert(false, "Insufficient CUDA Compute Capability: cuda::device::memcpy_async_tx is not available.");
#endif // __CUDA_MINIMUM_ARCH__

__device__ alignas(16) int gmem_x[2048];

__global__ void example_kernel(int *dsts) {
    using barrier_t = cuda::barrier<cuda::thread_scope_block>;
  __shared__ alignas(16) int smem_x[1024];
  __shared__ barrier_t bar;
  if (threadIdx.x == 0) {
    init(&bar, blockDim.x);
  }
  __syncthreads();

  barrier_t::arrival_token token;
  if (threadIdx.x == 0) {
    cuda::device::memcpy_async_tx(smem_x, gmem_x, cuda::aligned_size_t<16>(sizeof(smem_x)), bar);
    token = cuda::device::barrier_arrive_tx(bar, 1, sizeof(smem_x));
  } else {
    auto token = bar.arrive(1);
  }
  bar.wait(cuda::std::move(token));

  // smem_x contains the contents of gmem_x[0], ..., gmem_x[1023]
  smem_x[threadIdx.x] += threadIdx.x;

  __syncthreads();

  int hash = 0;
  for (int i : smem_x) {
    hash = (hash << 5) ^ i ^ (threadIdx.x << 13);
  }
  atomicAdd(dsts + threadIdx.x, sum);
}
*/

__global__ void example_mbarrier_arrives(int *dsts)
{
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-test-wait-try-wait

      // Example 1a, thread synchronization with test_wait:

      .reg .b64 arv_tkn;
      .reg .pred complete;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.arrive.shared.b64  arv_tkn, [mbar]; // implicit arrive count
      mbarrier.arrive.shared.b64  arv_tkn, [mbar], 1; // explicit arrive count of 1
      mbarrier.arrive.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4
      mbarrier.arrive_drop.shared.b64  arv_tkn, [mbar]; // implicit arrive count
      mbarrier.arrive_drop.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4

      // computation not requiring mbarrier synchronization...

      waitLoopSpin:
      mbarrier.test_wait.shared.b64    complete, [mbar], arv_tkn;
      @!complete nanosleep.u32 20; // user program provides backoff
      @!complete bra waitLoopSpin;
  }
  )" ::: "memory");
}

__global__ void example_mbarrier_arrive_expect(int *dsts)
{
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-test-wait-try-wait

      // Example 1a, thread synchronization with test_wait:

      .reg .b64 arv_tkn;
      .reg .pred complete;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.expect_tx.relaxed.cta.shared.b64 [mbar], 512; // expect_tx += 512 (no increase in arrivals)
      mbarrier.expect_tx.shared.b64 [mbar], 512; // expect_tx += 512 (no increase in arrivals)

      mbarrier.arrive.shared.b64  arv_tkn, [mbar]; // implicit arrive count
      mbarrier.arrive.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4
      mbarrier.arrive.noComplete.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4
      mbarrier.arrive_drop.shared.b64  arv_tkn, [mbar]; // implicit arrive count
      mbarrier.arrive_drop.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4

      mbarrier.arrive.expect_tx.shared.b64  arv_tkn, [mbar], 4; // implicit arrival of 1 with expect_tx += 4
      mbarrier.arrive_drop.expect_tx.shared.b64  arv_tkn, [mbar], 8; // implicit arrival of 1 (and drop) with expect += 8

      mbarrier.complete_tx.shared.b64   [mbar],     32; // simulate completions


      // computation not requiring mbarrier synchronization...

      waitLoopSpin:
      mbarrier.test_wait.shared.b64    complete, [mbar], arv_tkn;
      @!complete nanosleep.u32 20; // user program provides backoff
      @!complete bra waitLoopSpin;
  }
  )" ::: "memory");
}


__global__ void example_mbarrier_test_wait(int *dsts)
{
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-test-wait-try-wait

      // Example 1a, thread synchronization with test_wait:

      .reg .b64 arv_tkn;
      .reg .pred complete;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.arrive.shared.b64  arv_tkn, [mbar]; // N threads executing mbarrier.arrive

      // computation not requiring mbarrier synchronization...

      waitLoopSpin:
      mbarrier.test_wait.shared.b64    complete, [mbar], arv_tkn;
      @!complete nanosleep.u32 20; // user program provides backoff
      @!complete bra waitLoopSpin;
  }
  )" ::: "memory");

  atomicAdd(dsts + threadIdx.x, 1);
}

__global__ void example_mbarrier_try_wait(int *dsts)
{
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-test-wait-try-wait
      // Example 1b, thread synchronization with try_wait :

      .reg .b64 arv_tkn;
      .reg .pred complete;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.arrive.shared.b64  arv_tkn, [mbar]; // N threads executing mbarrier.arrive

      // computation not requiring mbarrier synchronization...

      waitLoop:
      mbarrier.try_wait.shared.b64    complete, [mbar], arv_tkn;
      @!complete bra waitLoop;
  }
  )" ::: "memory");

  atomicAdd(dsts + threadIdx.x, 1);
}

__global__ void example_mbarrier_try_wait_sup(int *dsts)
{
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-test-wait-try-wait
      .reg .b64 arv_tkn;
      .reg .pred complete;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.arrive.shared.b64  arv_tkn, [mbar], 4; // N threads executing mbarrier.arrive

      // computation not requiring mbarrier synchronization...

      waitLoopSusp:
      mbarrier.try_wait.shared.b64    complete, [mbar], arv_tkn, 0x1234;
      @!complete bra waitLoopSusp;
  })" ::: "memory");

  atomicAdd(dsts + threadIdx.x, 2);
}


__global__ void example_mbarrier_array(int *dsts)
{
  asm volatile (R"(
    {
   //  .shared .align 8 .b8 _ZZ16mbarrier_svectorILi4EEvPiPKiE4bars[32];

      .reg .b64 arv_tkn;
      .reg .b32 mbars_base;
      .reg .b32 mbar_addr32;
      .reg .b32 bid; // barrier index 0..7
      .reg .pred complete;

      .shared .align 8 .b64 mbars[4];

      mov.u32   mbars_base, mbars;
      mov.u32   bid, %tid.x;
      and.b32   bid, bid, 3; // mod by barrier count (4)
      shl.b32   bid, bid, 3; // scale index to byte offset
      add.s32   mbar_addr32, mbars_base, bid; // add to base

      mbarrier.init.shared.b64 [mbar_addr32], 2;

      mbarrier.arrive.shared.b64  arv_tkn, [mbar_addr32], 4;

      // computation not requiring mbarrier synchronization...

      waitLoopSusp:
      mbarrier.try_wait.shared.b64    complete, [mbar_addr32], arv_tkn, 0x1234;
      @!complete bra waitLoopSusp;
  })" ::: "memory");
}



/*


__global__ void example_mbarrier_pending_count(int *dsts)
{
  uint32_t pending_count = 0;
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-pending-count
      .reg .b64 state;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      // mbarrier.arrive.noComplete.b64 state, [mbar], 3;
      mbarrier.arrive.b64 state, [mbar], 3;
      mbarrier.pending_count.b64        %0, state;
  }
  )" : "=r"(pending_count) :: "memory");

  atomicAdd(dsts + threadIdx.x, (int)pending_count);
}


__global__ void example_mbarrier_arrive_nocomplete(int *dsts)
{
  uint32_t pending_count = 0;
  asm volatile (R"(
    {
      // https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier-pending-count
      .reg .b64 state;
      .shared .b64 mbar;

      mbarrier.init.shared.b64 [mbar], 32;  // N threads participating in the mbarrier.

      mbarrier.arrive.noComplete.b64 state, [mbar], 3;

      mbarrier.pending_count.b64        %0, state;
  }
  )" : "=r"(pending_count) :: "memory");

  atomicAdd(dsts + threadIdx.x, (int)pending_count);
}

*/
