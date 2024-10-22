ulong get_time()
{
  ulong out = 0;
  asm("mov.u64 %0, %%clock64;" : "=rd"(out) :: "memory");
  return out;
}


// Buffer size is N elements
// Input will be N-1 for all.
// [0..] -> N-1
// [...] -> N-1
// ..
// [N-1] -> 0
kernel void walkConsts(
  global uint *out,
  global long *runtime,
//  const global uint *in,
  int start,
  constant uint *indices)
{
  uint gid = get_global_id(0);
//  uint idx = in[gid];
  uint idx = indices[start];
  ulong rtStt = get_time();
  for (uint i = 0; i < K; i++) {
    idx = indices[idx];
  }
  ulong rtEnd = get_time();
  out[gid] = idx;
  printf("T: %lld\n", (rtStt - rtEnd));
  if ((uint)get_local_id(0) == 0) {
    runtime[gid] = (long)(rtStt - rtEnd);
  }
}

