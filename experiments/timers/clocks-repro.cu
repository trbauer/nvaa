#include <array>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>

///////////////////////////////////////////////////////////////////////////////
// text formatting
template <typename...Ts>
static void format_to(std::ostream &os) { }
template <typename T, typename...Ts>
static void format_to(std::ostream &os, T t, Ts...ts) {
  os << t; format_to(os, ts...);
}
template <typename...Ts>
static std::string format(Ts...ts) {
  std::stringstream ss; format_to(ss, ts...); return ss.str();
}

template <typename...Ts>
static void fatal(Ts...ts) {
  format_to(std::cerr, ts...); std::cerr << "\n";
  exit(EXIT_FAILURE);
}

#define CUDA_API(__CUDA_API__,...) \
  do { \
    auto __cuda_api_err = __CUDA_API__(__VA_ARGS__); \
    if (__cuda_api_err != cudaSuccess) { \
      fatal(#__CUDA_API__, " near line ", __LINE__, " failed with ", \
          cudaGetErrorName(__cuda_api_err), \
          " (", cudaGetErrorString(__cuda_api_err), ")"); \
    } \
  } while(0)

static const size_t NELEMS = 128;

/*
__global__ void stream_clock(uint32_t *cs)
{
  if (threadIdx.x != 0)
    return;
  for (int i = 0; i < NELEMS; i++)
    cs[i] = clock();
}
*/

__global__ void batch_clock(uint32_t *cs)
{
  static const int BATCH = 16;
  if (threadIdx.x != 0)
    return;
  for (int i = 0; i < NELEMS; i += BATCH) {
    uint32_t bs[BATCH];
    for (int k = 0; k < BATCH; k++) {
      bs[k] = clock();
    }
    for (int k = 0; k < BATCH; k++) {
      cs[i + k] = bs[k];
    }
  }
}

static std::string fmt_hex(uint32_t x) {
  std::stringstream ss;
  ss << "0x" << std::hex << std::uppercase << std::setw(8) << std::setfill('0') << x;
  return ss.str();
}

int main(int argc, const char *arv[])
{
  uint32_t *d_ptr;
  CUDA_API(cudaMalloc, &d_ptr, NELEMS * sizeof(uint32_t));
  std::array<uint32_t,NELEMS> h_arr;


  batch_clock<<<1,1>>>(d_ptr);
  auto e0 = cudaDeviceSynchronize();
  if (e0 != cudaSuccess) {
    fatal(cudaGetErrorName(e0), " (", cudaGetErrorString(e0), "): unexpected error");
  }
  CUDA_API(cudaMemcpy, &h_arr[0], d_ptr, NELEMS * sizeof(uint32_t),
            cudaMemcpyDeviceToHost);
  for (size_t i = 0; i < h_arr.size(); i++) {
    std::cout << fmt_hex(h_arr[i]) << "  ";
    if (i == 0) {
      std::cout << "----------";
    } else {
      std::cout << h_arr[i] - h_arr[i - 1];
    }
    std::cout << "\n";
  }

  return EXIT_SUCCESS;
}