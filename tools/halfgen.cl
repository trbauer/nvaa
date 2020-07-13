
kernel void genseq(global ushort *OUT)
{
  uint id = get_global_id(0);
  OUT[id] = id;
}

kernel void f16_to_f32(global float *OUT, const global half *IN)
{
  uint id = get_global_id(0);
  OUT[id] = vload_half(id, IN);
}



kernel void f32_to_f16_rtn(
  const global float *IN,
  global half *OUTn)
{
  uint id = get_global_id(0);
  vstore_half_rtn(IN[id], id, OUTn);
}
kernel void f32_to_f16_rtp(
  const global float *IN,
  global half *OUTn)
{
  uint id = get_global_id(0);
  vstore_half_rtp(IN[id], id, OUTn);
}
kernel void f32_to_f16_rtz(
  const global float *IN,
  global half *OUTn)
{
  uint id = get_global_id(0);
  vstore_half_rtz(IN[id], id, OUTn);
}
kernel void f32_to_f16_rte(
  const global float *IN,
  global half *OUTn)
{
  uint id = get_global_id(0);
  vstore_half_rte(IN[id], id, OUTn);
}
kernel void f32_to_f16(
  const global float *IN,
  global half *OUT,
  global half *OUTn,
  global half *OUTp,
  global half *OUTz,
  global half *OUTe)
{
  uint id = get_global_id(0);
  vstore_half(IN[id], id, OUT);
  f32_to_f16_rtn(IN, OUTn);
  f32_to_f16_rtp(IN, OUTp);
  f32_to_f16_rtz(IN, OUTz);
  f32_to_f16_rte(IN, OUTe);
}

float w32_to_float(uint bits)
{
  union {float f; uint i} u;
  u.i = bits;
  return u.f;
  // return as_float(bits);
}

kernel void w32_to_f16_rte(const global uint *IN, global half *OUT)
{
  uint id = get_global_id(0);
  vstore_half_rte(w32_to_float(IN[id]), id, OUT);
}
kernel void w32_to_f16_rtz(const global uint *IN, global half *OUT)
{
  uint id = get_global_id(0);
  vstore_half_rtz(w32_to_float(IN[id]), id, OUT);
}
kernel void w32_to_f16_rtn(const global uint *IN, global half *OUT)
{
  uint id = get_global_id(0);
  vstore_half_rtn(w32_to_float(IN[id]), id, OUT);
}
kernel void w32_to_f16_rtp(const global uint *IN, global half *OUT)
{
  uint id = get_global_id(0);
  vstore_half_rtp(w32_to_float(IN[id]), id, OUT);
}


float float_from_id(uint hi)
{
//  uint id_x = get_global_id(0);
//  uint id_y = get_global_id(1);
//  return w32_to_float(id_x|(id_y<<16));
    return w32_to_float(hi | get_global_id(0));
}


kernel void all_f16s_rte(global half *OUT, uint hi)
{
  uint id = get_global_id(0);
  vstore_half_rte(float_from_id(hi), id, OUT);
}
kernel void all_f16s_rtz(global half *OUT, uint hi)
{
  uint id = get_global_id(0);
  vstore_half_rtz(float_from_id(hi), id, OUT);
}
kernel void all_f16s_rtn(global half *OUT, uint hi)
{
  uint id = get_global_id(0);
  vstore_half_rtn(float_from_id(hi), id, OUT);
}
kernel void all_f16s_rtp(global half *OUT, uint hi)
{
  uint id = get_global_id(0);
  vstore_half_rtp(float_from_id(hi), id, OUT);
}
