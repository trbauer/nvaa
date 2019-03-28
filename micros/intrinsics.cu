// D.run ["micros/intrinsics.cu","--arch=sm_75","-o=micros/sm_75/intrinsics.sass","-lines","--save-ptx=micros/sm_75/intrinsics.ptx"]
//
// https://docs.nvidia.com/cuda/cuda-math-api/index.html
// https://docs.nvidia.com/cuda/cuda-math-api/group__CUDA__MATH__INTRINSIC__INT.html#group__CUDA__MATH__INTRINSIC__INT
// https://docs.nvidia.com/cuda/cuda-math-api/group__CUDA__MATH__INTRINSIC__SIMD.html

extern "C" __global__ void brev_i(
    const int *A,
    int *OUT)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = __brev(A[id]);
  OUT[id] = a;
}
extern "C" __global__ void brev_u(
    const unsigned *A,
    unsigned *OUT)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = __brev(A[id]);
  OUT[id] = a;
}
extern "C" __global__ void brev_u64(
    const unsigned long long *A,
    unsigned long long *OUT)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = __brevll(A[id]);
  OUT[id] = a;
}
extern "C" __global__ void usad_int(
    const int *A,
    const int *B,
    const int *C,
    int *OUT)
{
  int id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = __usad(A[id],B[id],C[id]);
  OUT[id] = a;
}

extern "C" __global__ void vabsdiffu4(
    const unsigned *A,
    const unsigned *B,
    unsigned *OUT)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id], b = B[id];
  auto r = __vabsdiffu4(a,b);
  OUT[id] = r;
}

// only s32 supported in PTX
extern "C" __global__ void add_s32_sat(
    const int *A,
    const int *B,
    int *OUT)
{
  auto id = blockDim.x * blockIdx.x + threadIdx.x;
  auto a = A[id], b = B[id];
  int result = 0;
  asm("add.sat.s32 %0, %1, %2;" : "=r"(result) : "r"(a),"r"(b));
  OUT[id] = result;
}
