#include <cuda/atomic> // https://github.com/NVIDIA/cccl/tree/main/libcudacxx
#include <cstdio>


// instead use atomicCas
/*
static __device__ uint4 int128cas(uint4 *addr, uint4 cmp, uint4 swap)
{
  // atom{.sem}{.scope}{.space}.cas.b128 d, [a], b, c {, cache-policy};
  // .space =              { .global, .shared{::cta, ::cluster} };
  // .sem =                { .relaxed, .acquire, .release, .acq_rel };
  // .scope =              { .cta, .cluster, .gpu, .sys };
  uint4 ret;
  asm volatile (
      "{\n\t"
      ".reg .b128 c, s, d;\n"
      "mov.b128 c, {%5,%6,%7,%8};\n"
      "mov.b128 s, {%9,%10,%11,%12};\n"
      "atom.global.cas.b128 d, [%4], c, s;\n"
      "mov.b128 {%0,%1,%2,%3}, d;\n"
      "}"
      : "=r"(ret.x), "=r"(ret.y), "=r"(ret.z), "=r"(ret.w)
      : "l"(addr),
        "r"(cmp.x), "r"(cmp.y), "r"(cmp.z), "r"(cmp.w),
        "r"(swap.x), "r"(swap.y), "r"(swap.z), "r"(swap.w)
      : "memory");
  return ret;
}
*/


///////////////////////////////////////////////////////////////////////////////
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-atom
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#atomiccas
extern "C"
__global__ void atomic_cas_kernel(uint4 * dst, uint4 c, uint4 s)
{
  __shared__ uint4 smem[128];
  smem[threadIdx.x] = make_uint4(threadIdx.x, threadIdx.y, threadIdx.z, 0);
  __syncthreads();

  uint4 *generic = (threadIdx.x == 0 ? smem : dst) + threadIdx.x;
  c = atomicCAS<uint4>(generic, s, c); // generic memory
  c = atomicCAS<uint4>(smem + threadIdx.y, c, s); // shared memory
  (void)atomicCAS<uint4>(dst + threadIdx.x, c, s); // global memory
}

///////////////////////////////////////////////////////////////////////////////
extern "C"
__global__ void atomic_exch_kernel(uint4 * dst, uint4 s)
{
  __shared__ uint4 smem[128];
  smem[threadIdx.x] = make_uint4(threadIdx.x, threadIdx.y, threadIdx.z, 0);
  __syncthreads();

  uint4 *generic = (threadIdx.x == 0 ? smem : dst) + threadIdx.x;
  s = atomicExch<uint4>(generic, s); // generic memory
  s = atomicExch<uint4>(smem + threadIdx.y, s); // shared memory
  (void)atomicExch<uint4>(dst + threadIdx.x, s); // global memory
}


/*
///////////////////////////////////////////////////////////////////////////////
#if defined(__CUDACC__) // NVCC
   #define MY_ALIGN(n) __align__(n)
#elif defined(__GNUC__) // GCC
  #define MY_ALIGN(n) __attribute__((aligned(n)))
#elif defined(_MSC_VER) // MSVC
  #define MY_ALIGN(n) __declspec(align(n))
#else
  #error "Please provide a definition for MY_ALIGN macro for your host compiler!"
#endif

struct alignas(16) custom128 {
  uint32_t word0, word1, word2, word3;
};
static constexpr custom128 zero128 {};

static_assert(sizeof(custom128) == 16);
static_assert(alignof(custom128) >= 16);
static_assert(std::is_trivially_copyable<custom128>::value);


// __device__ cuda::atomic<custom128, cuda::thread_scope_device> atm_dev(zero128);
// __device__ cuda::atomic<custom128, cuda::thread_scope_system> atm_sys; // (0);


__global__ void atomic_exch2_kernel(
  cuda::atomic<custom128, cuda::thread_scope_device> *atm_dev,
  cuda::atomic<custom128, cuda::thread_scope_system> *atm_sys,
  custom128 * dst, custom128 s)
{
  static_assert(sizeof(*atm_dev) == 16 + 16);
// static_assert(cuda::atomic<custom128, cuda::thread_scope_block>::is_always_lock_free);
// static_assert(cuda::atomic<custom128, cuda::thread_scope_device>::is_always_lock_free);

  // __shared__ uint4 smem[128];
// #pragma diag_suppress static_var_with_dynamic_init
//  __shared__ cuda::atomic<custom128, cuda::thread_scope_block> atm_blk;


  custom128 val_a {};
  custom128 val_b {0x0, 0x1, 0x2, 0x3};
//  if (threadIdx.x == 0) {
//    atm_blk.store(val_a);
//  }
/*
  __syncthreads();
  auto expect = val_a, desired = val_b;
  while (!atm_dev.compare_exchange_weak(
        expect,
        desired,
        cuda::memory_order::release,
        cuda::memory_order::relaxed))
    ;

  val_a = atm_dev[threadIdx.x].exchange(val_b);
  dst[threadIdx.x] = val_a;
//  atm_dev->store(expect, cuda::memory_order::relaxed);
//  atm_sys->store(desired);
}



https://en.cppreference.com/w/cpp/atomic/atomic/compare_exchange
struct alignas(16) linked_list_node {
  linked_list_node *prev, *next;
};
*/