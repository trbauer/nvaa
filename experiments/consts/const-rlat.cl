// cls64 -e "#0`const-rlat.cl`ffma_r(0:w,0:w,1:r)<1>" -B
// cls64 -e "#0`const-rlat.cl`ffma_r(0:wp,0:w,(float4)(1,2,3,4):r)<1>" -B
ulong get_time()
{
  ulong out = 0;
  asm volatile("mov.u64 %0, %%clock64;" : "=l"(out) :: "memory");
  // asm volatile("mov.u64 %0, %%globaltimer;" : "=l"(out) :: "memory");
  return out;
}
uint get_time32()
{
  uint out = 0;
  asm volatile("mov.u32 %0, %%clock;" : "=r"(out) :: "memory");
  return out;
}

kernel void ffma_r(
        global ulong *out_time,
        global float  *out,
  const global float4 *in0)
{
  const int K = 64 * 1024;

  uint gid = get_global_id(0);
  float reg = (float)get_local_id(0);
  float sum = 0.0f;
  ulong rt_sum = 0;
  const global float4 *in_ptr = in0 + gid;
  #pragma nounroll
  for (uint i = 0; i < K / 16; i++) {
    // load16; time(use16)
    float4 acc[4];
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      acc[k] = *in_ptr++;
    }
    ulong rt_stt = get_time();
    for (uint k = 0; k < sizeof(acc)/sizeof(acc[0]); k++) {
      sum += acc[k].s0 * reg;
      sum += acc[k].s1 * reg;
      sum += acc[k].s2 * reg;
      sum += acc[k].s3 * reg;
    }
    rt_sum += get_time() - rt_stt;
  }
  out[gid] = sum;
  if (gid == 0)
    out_time[0] = rt_sum / K;
  // printf("T: %lld\n", rt_sum / K);
}


