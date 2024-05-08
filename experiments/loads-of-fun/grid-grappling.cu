// Primary header is compatible with pre-C++11, collective algorithm headers require C++11
#include <cooperative_groups.h>
// Optionally include for memcpy_async() collective
#include <cooperative_groups/memcpy_async.h>
// Optionally include for reduce() collective
#include <cooperative_groups/reduce.h>
// Optionally include for inclusive_scan() and exclusive_scan() collectives
#include <cooperative_groups/scan.h>

// Alternatively use an alias to avoid polluting the namespace with collective algorithms
namespace cg = cooperative_groups;

extern "C"
__global__ void grid_sync(
    int32_t *s32_dsts,
    const int32_t *s32_srcs, int32_t x)
{
  // thread_block g = this_thread_block();
  // auto g = cg::this_thread_block();
  cg::grid_group g = cg::this_grid();

  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto val = atomicAdd(s32_dsts + gid, x);

  g.sync();

  /*
  auto tk = g.barrier_arrive();
  for (int i = 0; i < 4; i++) {
    val += s32_srcs[gid + i];
  }
  g.barrier_wait(std::move(tk));
  */
  val = atomicAdd(s32_dsts + threadIdx.y, val);
}


// cluster_group g = this_cluster();
