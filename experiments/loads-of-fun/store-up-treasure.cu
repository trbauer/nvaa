// 1. Compile with something like
// % nvcc -arch sm_90 micro.cu -cubin --generate-line-info -o micro-sm_90.cubin
// 2. Then disassemble with:
// % nvdisasm micro-sm_90.cubin --print-line-info --print-code
#include <cstdio>

#include <cuda_runtime.h>


extern "C"
__global__ void store_up_treasure(float *dsts, const float val)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;


// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#id82
// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-st
  asm volatile ("st.global.b32    [%0], %1;" :: "l"(dsts + gid + 0), "f"(val) : "memory");
//
  asm volatile ("st.global.wb.b32 [%0], %1;" :: "l"(dsts + gid + 1), "f"(val) : "memory");
  asm volatile ("st.global.cg.b32 [%0], %1;" :: "l"(dsts + gid + 2), "f"(val) : "memory");
  asm volatile ("st.global.cs.b32 [%0], %1;" :: "l"(dsts + gid + 3), "f"(val) : "memory");
  asm volatile ("st.global.wt.b32 [%0], %1;" :: "l"(dsts + gid + 4), "f"(val) : "memory");

  asm volatile ("st.weak.global.wb.b32 [%0], %1;" :: "l"(dsts + gid + 5), "f"(val) : "memory");
  asm volatile ("st.weak.global.cg.b32 [%0], %1;" :: "l"(dsts + gid + 6), "f"(val) : "memory");
  asm volatile ("st.weak.global.cs.b32 [%0], %1;" :: "l"(dsts + gid + 7), "f"(val) : "memory");
  asm volatile ("st.weak.global.wt.b32 [%0], %1;" :: "l"(dsts + gid + 8), "f"(val) : "memory");

  // st.release.*.global.b32
  asm volatile ("st.release.cta.global.b32     [%0], %1;" :: "l"(dsts + gid + 9), "f"(val) : "memory");
  asm volatile ("st.release.cluster.global.b32 [%0], %1;" :: "l"(dsts + gid + 10), "f"(val) : "memory");
  asm volatile ("st.release.gpu.global.b32     [%0], %1;" :: "l"(dsts + gid + 11), "f"(val) : "memory");
  asm volatile ("st.release.sys.global.b32     [%0], %1;" :: "l"(dsts + gid + 12), "f"(val) : "memory");
  // st.relaxed.*.global.b32
  asm volatile ("st.relaxed.cta.global.b32     [%0], %1;" :: "l"(dsts + gid + 13), "f"(val) : "memory");
  asm volatile ("st.relaxed.cluster.global.b32 [%0], %1;" :: "l"(dsts + gid + 14), "f"(val) : "memory");
  asm volatile ("st.relaxed.gpu.global.b32     [%0], %1;" :: "l"(dsts + gid + 15), "f"(val) : "memory");
  asm volatile ("st.relaxed.sys.global.b32     [%0], %1;" :: "l"(dsts + gid + 16), "f"(val) : "memory");
}

