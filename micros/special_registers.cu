// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers-globaltimer
#include <stdint.h>

extern "C" __global__ void get_global_timer(
  uint32_t *OUT32_LO,
  uint32_t *OUT32_HI,
  uint64_t *OUT64,
  uint64_t *EST_LATENCY)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;

  uint64_t t64;
  asm("mov.u64  %0,%globaltimer;" : "=l"(t64):);
  OUT64[id] = t64;

  const int LATENCY_SAMPLES = 2;
  uint64_t latency_sum = 0;
  uint64_t dummy = 0;
  for (int i = 0; i < LATENCY_SAMPLES; i++) {

    /*
    asm volatile(
      ".reg .u64 st, et;\n"
      "mov.u64   st, %clock64;\n"
      "mov.u64  %0,%globaltimer;\n"
      "mov.u64   et, %clock64;\n"
      "sub.u64   %0, st, et"
      : "=l"(t64), "=":);
      */

    auto st = clock64();
    asm volatile("mov.u64  %0,%globaltimer;" : "=l"(t64):);
    latency_sum += (uint64_t)(clock64() - st);

    dummy += t64;
  }
  if (dummy < 1)
    latency_sum += 1;
  EST_LATENCY[id] = latency_sum/LATENCY_SAMPLES;
  // TODO: test latency of global timer64


  uint32_t t32_lo, t32_hi;
  asm("mov.u32  %0,%globaltimer_lo;" : "=r"(t32_lo):);
  asm("mov.u32  %0,%globaltimer_hi;" : "=r"(t32_hi):);
  OUT32_LO[id] = t32_lo;
  OUT32_HI[id] = t32_hi;
}

// get_smem_sizes<1,64,4*64>(A,B,TOT_SMEM,DYN_SMEM)
extern "C" __global__ void get_smem_sizes(
  const uint32_t *A,
  uint32_t *B,
  uint32_t *TOT_SMEM,
  uint32_t *DYN_SMEM)
{
  __shared__ uint32_t STILE[32];
  extern __shared__ uint32_t DTILE[];

  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  STILE[threadIdx.x] = A[id];
  DTILE[threadIdx.x] = A[id] + 1;
  __syncthreads();

  uint32_t dsmem;
  asm("mov.u32  %0, %dynamic_smem_size;" : "=r"(dsmem));
  DYN_SMEM[id] = dsmem;

  uint32_t tsmem;
  asm("mov.u32  %0, %total_smem_size;" : "=r"(tsmem));
  TOT_SMEM[id] = tsmem;

  // use the SLM
  B[id] =
    STILE[(id+1) % (sizeof(STILE)/sizeof(STILE[0]))] +
    DTILE[(id+1) % (dsmem/sizeof(DTILE[0]))];
}


// get_envs<1,32>
extern "C" __global__ void get_envs(
  uint32_t *ENV)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;

#define GET_ENV_REG(N)\
  do {\
    if (threadIdx.x == (N)) {\
      asm("mov.u32  %0, %envreg" #N ";" : "=r"(ENV[id]));\
    } \
  } while (0)

  GET_ENV_REG(0);
  GET_ENV_REG(1);
  GET_ENV_REG(2);
  GET_ENV_REG(3);
  GET_ENV_REG(4);
  GET_ENV_REG(5);
  GET_ENV_REG(6);
  GET_ENV_REG(7);
  GET_ENV_REG(8);
  GET_ENV_REG(9);
  GET_ENV_REG(10);
  GET_ENV_REG(11);
  GET_ENV_REG(12);
  GET_ENV_REG(13);
  GET_ENV_REG(14);
  GET_ENV_REG(15);
  GET_ENV_REG(16);
  GET_ENV_REG(17);
  GET_ENV_REG(18);
  GET_ENV_REG(19);
  GET_ENV_REG(20);
  GET_ENV_REG(21);
  GET_ENV_REG(22);
  GET_ENV_REG(23);
  GET_ENV_REG(24);
  GET_ENV_REG(25);
  GET_ENV_REG(26);
  GET_ENV_REG(27);
  GET_ENV_REG(28);
  GET_ENV_REG(29);
  GET_ENV_REG(30);
  GET_ENV_REG(31);
}

extern "C" __global__ void get_pm32(
  uint32_t *PMs)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  if (threadIdx.x == 0) {
    asm("mov.u32  %0, %pm0;" : "=r"(PMs[id+0]));
    asm("mov.u32  %0, %pm1;" : "=r"(PMs[id+1]));
    asm("mov.u32  %0, %pm2;" : "=r"(PMs[id+2]));
    asm("mov.u32  %0, %pm3;" : "=r"(PMs[id+3]));
    asm("mov.u32  %0, %pm4;" : "=r"(PMs[id+4]));
    asm("mov.u32  %0, %pm5;" : "=r"(PMs[id+5]));
    asm("mov.u32  %0, %pm6;" : "=r"(PMs[id+6]));
    asm("mov.u32  %0, %pm7;" : "=r"(PMs[id+7]));
  }
}

extern "C" __global__ void get_pm64(
  uint64_t *PMs)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  if (threadIdx.x == 0) {
    asm("mov.u64  %0, %pm0_64;" : "=l"(PMs[id+0]));
    asm("mov.u64  %0, %pm1_64;" : "=l"(PMs[id+1]));
    asm("mov.u64  %0, %pm2_64;" : "=l"(PMs[id+2]));
    asm("mov.u64  %0, %pm3_64;" : "=l"(PMs[id+3]));
    asm("mov.u64  %0, %pm4_64;" : "=l"(PMs[id+4]));
    asm("mov.u64  %0, %pm5_64;" : "=l"(PMs[id+5]));
    asm("mov.u64  %0, %pm6_64;" : "=l"(PMs[id+6]));
    asm("mov.u64  %0, %pm7_64;" : "=l"(PMs[id+7]));
  }
}

