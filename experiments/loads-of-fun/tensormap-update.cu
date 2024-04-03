#include <cuda.h>         // CUtensormap
#include <cuda/barrier>
using barrier = cuda::barrier<cuda::thread_scope_block>;
namespace cde = cuda::device::experimental;


__constant__ CUtensorMap tensor_map_const;

__global__ void tensor_map_update(
    const __grid_constant__ CUtensorMap tensor_map_gc_arg,
    int x, int y, void *new_mem0, void *new_mem1, void *arg_ptr)
{
  CUtensorMap *ptr = (CUtensorMap *)arg_ptr;
  asm volatile ("tensormap.replace.tile.global_address.global.b1024.b64   [%0], %1;" :: "l"(ptr), "l"(new_mem0) : "memory");

  asm volatile ("tensormap.replace.tile.global_address.global.b1024.b64   [%0], %1;" :: "l"(ptr), "l"(new_mem1) : "memory");
  asm volatile ("tensormap.replace.tile.box_dim.global.b1024.b32   [%0], 0, %1;" :: "l"(ptr), "r"(0x3) : "memory");
  asm volatile ("tensormap.replace.tile.box_dim.global.b1024.b32   [%0], 1, %1;" :: "l"(ptr), "r"(0x44) : "memory");
  asm volatile ("tensormap.replace.tile.box_dim.global.b1024.b32   [%0], 2, %1;" :: "l"(ptr), "r"(0x55) : "memory");
  asm volatile ("tensormap.replace.tile.box_dim.global.b1024.b32   [%0], 3, %1;" :: "l"(ptr), "r"(0x66) : "memory");

//  asm volatile ("tensormap.replace.tile.global_address.shared::cta.b1024.b64   [%0], %1;" :: "l"(ptr), "l"(new_mem0) : "memory");
}
  /*
  __constant__ CUtensorMap global_tensor_map;
  __global__ void kernel()
  {
    // Use global_tensor_map here.
  }
  int main() {
    CUtensorMap local_tensor_map;
    // [ ..Initialize map.. ]
    cudaMemcpyToSymbol(global_tensor_map, &local_tensor_map, sizeof(CUtensorMap));
    kernel<<<1, 1>>>();
}
  */