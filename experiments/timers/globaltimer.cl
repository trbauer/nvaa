// RUN.
// cls64 -e "#0`globaltimer.cl`get_times(0:[8*128]wp1)<1,1>"

// __device__ uint64_t get_time_nano(uint64_t *tt)

void sample(global ulong *tt, uint off) {
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+0]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+1]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+2]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+3]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+4]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+5]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+6]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+7]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+8]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+9]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+10]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+11]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+12]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+13]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+14]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+15]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+16]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+17]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+18]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+19]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+20]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+21]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+22]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+23]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+24]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+25]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+26]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+27]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+28]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+29]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+30]));
  asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(tt[off+31]));
}

kernel void get_times(global ulong *tt)
{
  sample(tt, 0);
  sample(tt, 32);
  sample(tt, 64);
  sample(tt, 96);
}
