// __device__ uint64_t get_time_nano(uint64_t *tt)

#ifdef IS_CUDA
#define KERNEL         __global__
#define DEVICE         __device__
#define GLOBAL_ADDR
#define LOCAL_ADDR     __shared__
#define CONSTANT_ADDR  __constant__
typedef uint32_t       uint;
typedef uint64_t       ulong;
static __device__ int get_local_id(int)
{
  return threadIdx.x;
}
#else // OpenCL
#define KERNEL         kernel
#define DEVICE
#define GLOBAL_ADDR    global
#define LOCAL_ADDR     local
#define CONSTANT_ADDR  constant
#endif


///////////////////////////////////////////////////////////////////////////////
// RUN.
// cls64 -e "#0`clock.cl`globaltimer(0:[8*128]wp1,0:[8*128]wp1)<1,1>"
KERNEL void globaltimer(GLOBAL_ADDR ulong *gts64s, GLOBAL_ADDR ulong *deltas)
{
  if (get_local_id(0) % 32 != 0)
    return;
  for (uint base = 0; base < 128; base += 32) {
    ulong prev = 0;
    deltas[base + 0] = 0;
    for (int i = 0; i < 32; i++) {
      ulong curr;
      asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(curr) :: "memory");
      gts64s[base + i] = curr;
      if (i > 0) {
        deltas[base + i] = (long)(curr - prev);
      }
      prev = curr;
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
// cls64 -e "#0`clock.cl`clocks(0:[8*32]wp8, 0:[4*32]wp8, 0:[4*32]wp8)<1,1>"
KERNEL void clocks(
    GLOBAL_ADDR ulong *cs64s,
    GLOBAL_ADDR uint *csLos,
    GLOBAL_ADDR uint *csHis)
{
  if (get_local_id(0) % 32 != 0)
    return;
  uint base = 0;
  for (int i = 0; i < 128; i++) {
    asm volatile("mov.u64 %0, %%clock64;" : "=l"(cs64s[base+i]) :: "memory");
    asm volatile("mov.u32 %0, %%clock;" : "=r"(csLos[base+i]) :: "memory");
    asm volatile("mov.u32 %0, %%clock_hi;" : "=r"(csHis[base+i]) :: "memory");
  }
}

// cls64 -e "#0`clock.cl`clocks_delta(0:[4*32]wp8, 0:[4*32]wp8)<1,1>"
KERNEL void clocks_delta(
    GLOBAL_ADDR uint *csLos, GLOBAL_ADDR int *csDt)
{
  if (get_local_id(0) % 32 != 0)
    return;
  uint base = 0;
  uint prev = 0;
  for (int i = 0; i < 128; i++) {
    uint curr;
    asm volatile("mov.u32 %0, %%clock;" : "=r"(curr) :: "memory");
    csLos[base + i] = curr;
    csDt[base + i] = i == 0 ? 0 : (int)(curr - prev);
    prev = curr;
  }
}
