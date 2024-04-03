// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#memory-fence-functions

//
// __inline_hint__

// barrier predicate sync
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#synchronization-functions

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-membar

extern "C"
__global__ void fence_fiasco(
    int32_t *s32_dsts,
    const int32_t *s32_srcs)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  asm volatile ("fence.proxy.async;" ::: "memory");


}

// examples
// tensormap.replace.tile.global_address.global.b1024.b64   [gbl], new_addr;
// fence.proxy.tensormap::generic.release.gpu;
// fence.proxy.tensormap::generic.acquire.gpu [tmap], 128;