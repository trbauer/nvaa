
// D.run ["micros/nanosleep.cu","--arch=sm_75","-o=micros/sm_75/nanosleep.sass","-lines"]

extern "C" __global__ void nanosleep_kernel(
    volatile float *A,
    float *OUT,
    int k)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  float sum = A[id];
  __nanosleep(0x12345678);
  for (int i = 0; i < k; i++) {
    // asm volatile("nanosleep.u32 %0;" :: "r"(ns));
    sum += A[id];
    __nanosleep(i);
  }
  OUT[id] = sum;
}