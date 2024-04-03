#include <cuda.h>         // CUtensormap
// #include <cuda_runtime.h>
// #include <cuda/barrier>
// #include <cooperative_groups.h>


#include <cstdint>



/*
__global__ void example_tmi_modify_tensormap(int *dsts)
{
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-tensormap-cp-fenceproxy
  uint32_t pending_count = 0;
  asm volatile (R"(
    {

// Example: manipulate a tensor-map object and then consume it in cp.async.bulk.tensor

.reg .b64 new_addr;
.global .align 128 .b8 gbl[128];
.shared .align 128 .b8 sMem[128];
.shared .b64 mbar;

mbarrier.init.shared.b64 [mbar], 32;  // # threads participating in the mbarrier.

cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [sMem], [gMem], 128, [mbar];

// use them

try_wait_loop:
mbarrier.try_wait.shared.b64 p, [mbar], state;
@!p bra try_wait loop;

tensormap.replace.tile.global_address.shared.b1024.b64   [sMem], new_addr;
tensormap.cp_fenceproxy.global.shared::cta.proxy.tensormap::generic.release.gpu.sync.aligned        [gbl], [sMem], 128;
fence.proxy.tensormap::generic.acquire.gpu [gbl], 128;
cp.async.bulk.tensor.1d.shared::cluster.global.tile  [addr0], [gbl, {tc0}], [mbar0];

  }
  )" : "=r"(pending_count) :: "memory");

  atomicAdd(dsts + threadIdx.x, (int)pending_count);
}
*/
extern "C" __global__ void kernel(
  const __grid_constant__ CUtensorMap tensor_map)
{
}

__constant__ CUtensorMap global_tensor_map;
__global__ void kernel()
{
  // Use global_tensor_map here.
}