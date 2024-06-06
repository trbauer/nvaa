/// OPTIONS nvcc: -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4\include" -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4/extras/CUPTI/include"
/// OPTIONS nvcc: -L "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4\extras\CUPTI\lib64" -lcupti
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>

// CUDA headers
#include <cuda.h>
#include "device_launch_parameters.h"
#include <cuda_runtime.h>

// CUPTI headers
#include <cupti_sass_metrics.h>
#include <cupti_profiler_target.h>
#include <cupti_target.h>

#define RUNTIME_API_CALL(apiFunctionCall)                                           \
do                                                                                  \
{                                                                                   \
    cudaError_t _status = apiFunctionCall;                                          \
    if (_status != cudaSuccess)                                                     \
    {                                                                               \
        std::cerr << "\n\nError:" << __FILE__ << ":" << __LINE__ << ": Function "   \
        << #apiFunctionCall << " failed with error(" << _status << "): "            \
        << cudaGetErrorString(_status) << ".\n\n";                                  \
                                                                                    \
        exit(EXIT_FAILURE);                                                         \
    }                                                                               \
} while (0)

#define CUPTI_API_CALL(apiFunctionCall)                                             \
do                                                                                  \
{                                                                                   \
    CUptiResult _status = apiFunctionCall;                                          \
    if (_status != CUPTI_SUCCESS)                                                   \
    {                                                                               \
        const char *pErrorString;                                                   \
        cuptiGetResultString(_status, &pErrorString);                               \
                                                                                    \
        std::cerr << "\n\nError:" << __FILE__ << ":" << __LINE__ << ": Function "   \
        << #apiFunctionCall << " failed with error(" << _status << "): "            \
        << pErrorString << ".\n\n";                                                 \
                                                                                    \
        exit(EXIT_FAILURE);                                                         \
    }                                                                               \
} while (0)


// "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4/bin/nvcc"
// -gencode arch=compute_70,code=sm_70 -gencode arch=compute_72,code=sm_72
// -gencode arch=compute_75,code=sm_75 -gencode arch=compute_80,code=sm_80
// -gencode arch=compute_86,code=sm_86 -gencode arch=compute_87,code=sm_87
// -gencode arch=compute_89,code=sm_89 -gencode arch=compute_90,code=sm_90
// -lineinfo
// -c
// -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4/include"
// -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4/extras/CUPTI/include"
// -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.4/extras/CUPTI/samples/common"

int main(int argc, char *argv[])
{
  int deviceNum = 0;

  cudaDeviceProp prop;
  RUNTIME_API_CALL(cudaSetDevice(deviceNum));
  RUNTIME_API_CALL(cudaGetDeviceProperties(&prop, deviceNum));
  std::cout << "Device Name: " << prop.name << "\n";
  std::cout << "Device compute capability: " << prop.major << "." << prop.minor << "\n";

  CUpti_Profiler_Initialize_Params profilerInitializeParams = { CUpti_Profiler_Initialize_Params_STRUCT_SIZE };
  CUPTI_API_CALL(cuptiProfilerInitialize(&profilerInitializeParams));

  return EXIT_SUCCESS;
}