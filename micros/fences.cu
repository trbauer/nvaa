

#define FENCE_KERNEL(ID,ASM_STR)\
extern "C" __global__ void fences_kernel_ ## ID(\
  volatile float *OUT, volatile float *IN)\
{\
  int id = blockDim.x * blockIdx.x + threadIdx.x;\
  OUT[id] = IN[id+1] + 1.0f;\
  asm(ASM_STR);\
  OUT[id+1] = IN[id] + 2.0f;\
}

// same as .acq_rel (since that's default)
FENCE_KERNEL(fence__cta, "fence.cta;")
FENCE_KERNEL(fence__gpu, "fence.gpu;")
FENCE_KERNEL(fence__sys, "fence.sys;")
//
FENCE_KERNEL(fence_sc_cta, "fence.sc.cta;")
FENCE_KERNEL(fence_sc_gpu, "fence.sc.gpu;")
FENCE_KERNEL(fence_sc_sys, "fence.sc.sys;")
//
FENCE_KERNEL(fence_ar_cta, "fence.acq_rel.cta;")
FENCE_KERNEL(fence_ar_gpu, "fence.acq_rel.gpu;")
FENCE_KERNEL(fence_ar_sys, "fence.acq_rel.sys;")


extern "C" __global__ void fences_kernel_threadfence(
  volatile float *OUT, volatile float *IN)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  OUT[id] = IN[id+1] + 1.0f;
  __threadfence();
  OUT[id+1] = IN[id] + 2.0f;
}
extern "C" __global__ void fences_kernel_threadfence_block(
  volatile float *OUT, volatile float *IN)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  OUT[id] = IN[id+1] + 1.0f;
  __threadfence_block();
  OUT[id+1] = IN[id] + 2.0f;
}
extern "C" __global__ void fences_kernel_threadfence_system(
  volatile float *OUT, volatile float *IN)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  OUT[id] = IN[id+1] + 1.0f;
  __threadfence_system();
  OUT[id+1] = IN[id] + 2.0f;
}


