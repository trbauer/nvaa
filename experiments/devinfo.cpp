#include <cstdio>

#include <cuda.h>
// https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__DEVICE.html
//

//  https://developer.nvidia.com/cuda-gpus
// most official docs I can find
//  https://arnon.dk/matching-sm-architectures-arch-and-gencode-for-various-nvidia-cards/
//  https://en.wikipedia.org/wiki/List_of_Nvidia_graphics_processing_units#RTX_40_series
//  https://en.wikipedia.org/wiki/CUDA


#include <cuda_runtime.h>

int main() {
  int driver_version = 0, runtime_version = 0;

  cudaDriverGetVersion(&driver_version);
  cudaRuntimeGetVersion(&runtime_version);

  printf("Driver Version: %d\n"
         "Runtime Version: %d\n",
         driver_version, runtime_version);

  int nDevices;

  cudaGetDeviceCount(&nDevices);
  for (int i = 0; i < nDevices; i++) {
    cudaDeviceProp prop;
    cudaGetDeviceProperties(&prop, i);
    printf("Device Number: %d\n", i);
    printf("  Device name: %s\n", prop.name);
    printf("  Memory Clock Rate (KHz): %d\n",
           prop.memoryClockRate);
    printf("  Memory Bus Width (bits): %d\n",
           prop.memoryBusWidth);
    printf("  Peak Memory Bandwidth (GB/s): %f\n",
           2.0*prop.memoryClockRate*(prop.memoryBusWidth/8)/1.0e6);
    printf("  Total global mem (GB): %.3f\n",
           prop.totalGlobalMem / 1024.0 / 1024.0 / 1024.0);
    printf("  Total L2 size (MB): %.3f\n",
           prop.l2CacheSize / 1024.0 / 1024.0);
    printf("  Total const mem (KB): %.3f\n",
           prop.totalConstMem / 1024.0);
    printf("\n");
  }

  return EXIT_SUCCESS;
}
