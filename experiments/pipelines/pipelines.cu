// multistage pipeline
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#multi-stage-asynchronous-data-copies-using-cuda-pipeline

// release/acquire logic and staging buffers


// pipeline primitives
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#pipeline-primitives-interface

// TMA
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#asynchronous-data-copies-using-tensor-memory-access-tma


// figure out how cp.async can zero fill?

#include <cuda_runtime.h>
#include <cuda_pipeline_primitives.h>
#include <cuda/pipeline>
#include <cooperative_groups.h>

#include "mincu.hpp"

using namespace mincu;

struct opts {
  int verbosity = 0;
};


// 1. create buffer with uint4's of all 1's (small negative numbers)
// 2a. device function memcpy_async to pipeline   8,  8 (no ZFILL)
// 2b. device function memcpy_async to pipeline  16, 16 (no ZFILL)
// 2za. device function memcpy_async to pipeline  8, 4 (ZFILL)
// 2zb. device function memcpy_async to pipeline 16, 4 (ZFILL)
// 2zc. device function memcpy_async to pipeline 16, 8 (ZFILL)
//

// void __pipeline_memcpy_async(void* __restrict__ dst_shared,
//                              const void* __restrict__ src_global,
//                              size_t size_and_align,
//                              size_t zfill=0);


__global__ void pipecopy_zfill(
    uint4 *oups,
    const uint4 *srcs,
    uint32_t zero)
{
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  __shared__ uint4 shmem[64];

  uint4 *shptr = &shmem[threadIdx.x];

  // 8,8;8,4
//  uint4 val = srcs[gid];
//  if (threadIdx.x == 3) {
//    val.x++;
//  }

  // https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#memcpy-async-primitive
// printf("T%d \n", threadIdx.x);
  if (threadIdx.x == 2) {
//    __pipeline_memcpy_async(shptr, &srcs[gid].x, 16, 5); // zero-fill last 5? (works on sm_75)
    __pipeline_memcpy_async(shptr, &srcs[gid].x, 16, 4); // zero-fill last 4
//    __pipeline_memcpy_async(shptr, &srcs[gid].x, 16, 8); // zero-fill last 8
//    __pipeline_memcpy_async(shptr, &srcs[gid].x, 16, 12); // zero-fill last 12
  } else {
    __pipeline_memcpy_async(shptr, &srcs[gid].x, 16); // no zero-fill
  }
  __pipeline_commit();
  __pipeline_wait_prior(0);

  oups[gid] = *shptr;
}

const uint32_t BLOCK_SIZE = 64;
__global__ void pipecopy_multistage(
    int32_t *oups,
    const int32_t *srcs,
    int32_t zero)
{
  constexpr size_t MAX_STAGES = 4;
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  __shared__ int32_t shmem[MAX_STAGES][BLOCK_SIZE];

  cuda::pipeline<cuda::thread_scope_thread> pipe = cuda::make_pipeline();
  const auto asize = cuda::aligned_size_t<alignof(int32_t)>(sizeof(int32_t));

  for (int stage = 0; stage < MAX_STAGES; stage++) {
    pipe.producer_acquire();
    cuda::memcpy_async(shmem[stage] + threadIdx.x,
                       srcs + stage * BLOCK_SIZE + threadIdx.x,
                       asize,
                       pipe);
    pipe.producer_commit();
  }

  uint32_t sum = 0;
  for (int stage = 0; stage < MAX_STAGES; stage++) {
    pipe.consumer_wait();
    const auto me = shmem[stage][threadIdx.x];
    for (int i = 0; i < BLOCK_SIZE; i++)
      sum += shmem[stage][i] - me;
    pipe.consumer_release();
  }

  oups[gid] = sum;
}

/*
__global__ void pipecopy_multistage_sharedstate(
    uint1 *oups,
    const uint1 *srcs,
    uint32_t zero)
{
  constexpr size_t maxPipelineStages = 4;
  const auto gid = blockIdx.x * blockDim.x + threadIdx.x;
  __shared__ uint4 shmem[64];

  const auto shape = cuda::aligned_size_t<alignof(*srcs)>(sizeof(*srcs));
  __shared__ cuda::pipeline_shared_state<cuda::thread_scope_block,
                                         maxPipelineStages> shared_state;
  const auto thread_role = (threadIdx.x < 32)
                               ? cuda::pipeline_role::consumer
                               : cuda::pipeline_role::producer;
  auto pipe = cuda::make_pipeline(cta, &shared_state, thread_role);
}
*/

static void run_zfill(const opts &os)
{
  static const size_t BLOCKS = 1; // 1 warp only
  static const size_t TPB = 8; // threads per block (1 warp)
  // uint4
  // int4
  umem<uint4> inps(BLOCKS * TPB,
      [&](size_t ix) {return make_uint4(~0u,~0u - 1,~0u - 2,~0u - 3);});
  umem<uint4> oups(BLOCKS * TPB, const_seq<uint4>(make_uint4(0u, 0u, 0u, 0u)));
  std::cout << "inps:\n";
  inps.str(std::cout, 2);
  std::cout << "oups (init):\n";
  oups.str(std::cout, 2);

  pipecopy_zfill<<<BLOCKS,TPB>>>(oups, inps, 0);

  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  std::cout << "oups (final):\n";
  oups.str(std::cout, 2);
}


int main(int argc, const char **argv)
{
  opts os;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    std::string key = arg, val;
    auto eq = arg.find('=');
    if (eq != std::string::npos) {
      key = arg.substr(0, eq + 1); // include the =
      val = arg.substr(eq + 1);
    }
    auto bad_opt = [&](std::string msg0, std::string msg1 = "", std::string msg2 = "") {
      fatal(arg, ": ", msg0, msg1, msg2);
    };

    if (arg == "-h" || arg == "--help") {
      std::cout <<
        "usage: pipelines.exe OPTS TEST+\n"
        "where OPTS are:\n"
        "  -v/-v2                  sets verbosity\n"
        "EXAMPLES:\n"
        " % zfill\n"
        "";
      return EXIT_SUCCESS;
    } else if (arg == "-v") {
      os.verbosity = 1;
    } else if (arg == "-v2") {
      os.verbosity = 2;
    } else if (!arg.empty() && arg[0] == '-') {
      bad_opt("unexpected option");
    } else {
      bad_opt("unexpected argument");
    }
  } // for args

  run_zfill(os);

  return EXIT_SUCCESS;
}

