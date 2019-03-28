// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#miscellaneous-instructions-pmevent


extern "C" __global__ void run_atomics(
    const float *A,
    const float *B,
    float *OUT,
    int numElements)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;

  asm("pmevent          1;");
  asm("pmevent          7;");
  asm("pmevent.mask  0xFF;");

  OUT[id] = A[id] + B[id];
}
