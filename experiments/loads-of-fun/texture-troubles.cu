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