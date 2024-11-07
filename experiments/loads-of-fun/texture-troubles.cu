// textures
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#texture-functions
// surfaces
// https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#surface-object-api-appendix
extern "C"
__global__ void texture_troubles(
    float4 *dsts,
    const float2 *srcs,
    float alpha,
    cudaTextureObject_t rgbaTex)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  const float2 src = srcs[gid];

  float4 px0 = tex2D<float4>(rgbaTex, (float)threadIdx.x + 0.0f, (float)threadIdx.y + 0.0f);
  float4 px1 = tex2D<float4>(rgbaTex, (float)threadIdx.x + 0.0f, (float)threadIdx.y + 1.0f);
  float4 px2 = tex2D<float4>(rgbaTex, (float)threadIdx.x + 1.0f, (float)threadIdx.y + 0.0f);
  float4 px3 = tex2D<float4>(rgbaTex, (float)threadIdx.x + 1.0f, (float)threadIdx.y + 1.0f);

  // The texture sampler typically uses the same depbar for all texture ops
  // The DEPBAR.LE operation is incrementally used to release texture fetches as they arrive.
  //
  // This also illustrates the payload creation.

  dsts[gid].x = 0.5000f * px0.x + alpha * px1.x + src.x * px2.x + src.y * px3.x;
  dsts[gid].y = 0.2500f * px0.y + alpha * px1.y + src.x * px2.y + src.y * px3.y;
  dsts[gid].z = 0.1250f * px0.z + alpha * px1.z + src.x * px2.z + src.y * px3.z;
  dsts[gid].w = 0.0625f * px0.w + alpha * px1.w + src.x * px2.w + src.y * px3.w;
}

/*
static float4 operator*(float4 a, float b) {
  a.x *= b;
  a.y *= b;
  a.z *= b;
  a.w *= b;
  return a;
}
*/
static __device__ float4& operator*=(float4 &a, float b) {
  a.x *= b;
  a.y *= b;
  a.z *= b;
  a.w *= b;
  return a;
}

// cudaBoundaryModeZero
// cudaBoundaryModeClamp
// cudaBoundaryModeTrap

extern "C"
__global__ void surface_stress(
    cudaSurfaceObject_t dst_surf,
    cudaSurfaceObject_t src_surf,
    float alpha)
{
  const int gid_x = blockIdx.x * blockDim.x + threadIdx.x;
  const int gid_y = blockIdx.x * blockDim.x + threadIdx.x;
  float4 px0 = surf2Dread<float4>(src_surf, gid_x + 0, gid_y + 0, cudaBoundaryModeTrap);
  float4 px1 = surf2Dread<float4>(src_surf, gid_x + 1, gid_y + 0, cudaBoundaryModeZero);
  float4 px2 = surf2Dread<float4>(src_surf, gid_x + 2, gid_y + 0, cudaBoundaryModeClamp);
  px0 *= alpha;
  px1 *= alpha;
  px2 *= alpha;
  surf2Dwrite(px0, dst_surf, gid_x + 0, gid_y + 0, cudaBoundaryModeTrap);
  surf2Dwrite(px1, dst_surf, gid_x + 1, gid_y + 0, cudaBoundaryModeZero);
  surf2Dwrite(px2, dst_surf, gid_x + 1, gid_y + 0, cudaBoundaryModeClamp);
}