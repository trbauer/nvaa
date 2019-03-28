
// D.run ["micros/shfl.cu","--arch=sm_75","-o=micros/sm_75/shfl.sass","-lines"]

extern "C" __global__ void shuffle32(
    const int *A,
    int *OUT,
    int len)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  int a = A[id];

  int sum = 0;

  // immediate
  sum += __shfl_sync(0xAAAAAAAA, a, 0xB, warpSize);

//  for (int i = 0; i < warpSize; i++) {
  for (int i = 0; i < len; i++) {
    sum += __shfl_sync(0xAAAAAAAA, a, i, warpSize); // broadcast
    sum += __shfl_up_sync(0xAAAAAAAA, a, 1, warpSize);  // the neighbor above
    sum += __shfl_down_sync(0xAAAAAAAA, a, 1, warpSize); // below
    sum += __shfl_xor_sync(0xFFFFFFFF, a, 1, warpSize); //
  }
  OUT[id] = sum;
}

extern "C" __global__ void shuffle64(
    const long long *A,
     long long *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  long long a = A[id];


  long long sum = 0;
  for (int i = 0; i < warpSize; i++) {
    sum += __shfl_sync(0xAAAAAAAA, a, i, warpSize);
    sum += __shfl_up_sync(0xAAAAAAAA, a, 1, warpSize);
    sum += __shfl_down_sync(0xAAAAAAAA, a, 1, warpSize);
  }
  OUT[id] = sum;
}