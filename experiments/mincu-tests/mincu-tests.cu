#include "mincu.hpp"

#include <iostream>
#include <cstdio>
#include <functional>

using namespace mincu;

MINCU_ENABLE_COLOR_IO_VIA_STATIC_CONSTRUCTOR();

///////////////////////////////////////////
// TODO:
//  - move test files to testing.hpp (testing.cpp)
//  - move host-only tests to mincu-tests-host.cpp
//  - place device tests to mincu-tests-device.cpp
//  - mincu-tests.cu calls into both those
//     (all this breaks bexp.exe! unless we do #include only or something)
struct opts {
  int verbosity = 0;
};

static opts g_os;


template <typename T>
static std::string mc_format(T t, fmt_opts fos = fmt_opts())
{
  std::stringstream ss;
  format_elem<T>(ss, t, fos);
  return ss.str();
}

static std::string test_esc_str(std::string s) {
  std::stringstream ss;
  ss << '\"';
  for (size_t i = 0; i < s.size(); i++) {
    switch(s[i]) {
    case '\\': ss << "\\";
    case '\"': ss << "\"";
    case '\'': ss << "\'";
    case '\n': ss << "\n";
    case '\t': ss << "\t";
    default:
      if (std::isprint(s[i])) {
        ss << s[i];
      } else {
        ss << "\\x" << fmt_hex_digits(s[i], 2);
      }
    }
  }
  ss << '\"';
  return ss.str();
}
static std::string test_fmt(const char *str) {
  return test_esc_str(std::string(str));
}
static std::string test_fmt(std::string str) {
  return test_esc_str(str);
}

template <typename T>
static std::string test_fmt(const T &t) {
  return mincu::format(t);
}



static const char *g_current_label = "?";

static void test_fatal(
    int line, const char *macro,
    const char *sut_expr, const std::string &sut_fmtd,
    const char *exp_expr, const std::string &exp_fmtd,
    std::string hint = "")
{
  mincu::fatal("test ", ansi_red(g_current_label), "; near line ",
               ansi_yellow(line), ": ", macro, "(", sut_expr, ", ", exp_expr,
               ")\n"
               "    sut:",
               sut_fmtd, ", exp:", exp_fmtd, (hint.empty() ? "" : "\n  "), hint);
}

template <typename T1, typename T2>
static void test_eq_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp, std::string hint = "")
{
  if (!(sut == exp)) {
    test_fatal(line, "TEST_EQ", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp), hint);
  }
}
template <typename T1, typename T2>
static void test_ne_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp)
{
  if (!(sut != exp)) {
    test_fatal(line, "TEST_NE", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp));
  }
}
template <typename T1, typename T2>
static void test_lt_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp)
{
  if (!(sut < exp)) {
    test_fatal(line, "TEST_NE", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp));
  }
}
template <typename T1, typename T2>
static void test_le_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp)
{
  if (!(sut <= exp)) {
    test_fatal(line, "TEST_NE", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp));
  }
}
template <typename T1, typename T2>
static void test_gt_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp)
{
  if (!(sut > exp)) {
    test_fatal(line, "TEST_GT", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp));
  }
}
template <typename T1, typename T2>
static void test_ge_impl(
    int line,
    const char *sut_expr, const T1 &sut,
    const char *exp_expr, const T2 &exp)
{
  if (!(sut >= exp)) {
    test_fatal(line, "TEST_GE", sut_expr, test_fmt(sut), exp_expr,
               test_fmt(exp));
  }
}
#define TEST_EQ(SUT, EXP, ...) \
  test_eq_impl(__LINE__, #SUT, SUT, #EXP, EXP, __VA_ARGS__)
#define TEST_NE(SUT, EXP) \
  test_ne_impl(__LINE__, #SUT, SUT, #EXP, EXP)
#define TEST_LT(SUT, EXP) \
  test_lt_impl(__LINE__, #SUT, SUT, #EXP, EXP)
#define TEST_LE(SUT, EXP) \
  test_le_impl(__LINE__, #SUT, SUT, #EXP, EXP)
#define TEST_GT(SUT, EXP) \
  test_gt_impl(__LINE__, #SUT, SUT, #EXP, EXP)
#define TEST_GE(SUT, EXP) \
  test_ge_impl(__LINE__, #SUT, SUT, #EXP, EXP)

static void start_test(const char *lbl)
{
  g_current_label = lbl;
  if (g_os.verbosity >= 0)
    std::cout << "============= " << lbl << "\n";
}

static void TEST_GROUP(const char *LBL, std::function<void()> BLOCK) {
  start_test(LBL);
  BLOCK();
  g_current_label = "?";
}


/*
extern "C"
__global__ void add_float_k(
    float *dsts,
    const float *srcs, float k)
{
  const int gid = blockIdx.x * blockDim.x + threadIdx.x;
  auto val = srcs[gid];
  auto val_plus_k = val + k;
  dsts[gid] = val_plus_k;
}


static void test_add_float_k()
{
  start_test("static void test_add_float_k");
  static const size_t BLOCKS = 1; // 1 warp only
  static const size_t TPB = 32; // threads per block (1 warp)

  umem<float> inps(64, arith_seq<float>(0.0f));
  umem<float> oups(64);
  inps.str(std::cout, 8, 3);

  add_float_k<<<BLOCKS,TPB>>>(dsts, srcs, 1.0f);
  auto e = cudaDeviceSynchronize();
  if (e != cudaSuccess) {
    mincu::fatal(cudaGetErrorName(e), " (", cudaGetErrorString(e), "): unexpected error");
  }
  oups.str(std::cout, 8, 3);
} // test_add_float_k
*/

static void run_buf_init_tests()
{
  /////////////////////////////////////////////
  // constant sequence initializer
  TEST_GROUP("umem<unsigned>.no_init",
      [] {
        umem<unsigned> buf(8);
        TEST_EQ(buf.size(), 8);
      });

  TEST_GROUP("umem<unsigned>.const_seq(3).reinit(const_seq(7)",
      [] {
        umem<unsigned> buf(8, const_seq(3u));
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], 3u);
        }
        // re-initialize
        buf.init(const_seq(7u));
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], 7u);
        }
      });

  TEST_GROUP("umem<unsigned>.const_seq(3)[2] = 55",
      [] {
        umem<unsigned> buf(8, const_seq(3u));
        buf[2] = 55;
        for (size_t i = 0; i < buf.size(); i++) {
          if (i == 2) {
            TEST_EQ(buf[i], 55u);
          } else {
            TEST_EQ(buf[i], 3u);
          }
        }
      });

  TEST_GROUP("umem<uint2>.const_seq(2, 3)[3] = make_uint2(4, 1)",
      [] {
        umem<uint2> buf(4, const_seq(make_uint2(2, 3)));
        buf[3] = make_uint2(4, 1);
        for (size_t i = 0; i < buf.size(); i++) {
          if (i == 3) {
            TEST_EQ(buf[i], make_uint2(4, 1));
          } else {
            TEST_EQ(buf[i], make_uint2(2, 3));
          }
        }
      });

  TEST_GROUP("umem<unsigned>.arit_seq(0)",
      [] {
        // arithmetic sequence initializer
        umem<unsigned> buf {8, arith_seq(0u)};
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], i);
        }
      });
  TEST_GROUP("umem<unsigned>.arit_seq(0, 2)",
      [] {
        // arithmetic sequence initializer
        umem<unsigned> buf {8, arith_seq(0u, 2u)};
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], 2 * i);
        }
      });
  TEST_GROUP("umem<unsigned>.arit_seq(77u, 2u, 5u)",
      [] {
        umem<unsigned> buf {8, arith_seq(77u, 2u, 5u)};
        unsigned val = 77u % 5;
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], val);
          val = (val + 2u) % 5u;
        }
      });

  TEST_GROUP("umem<uint2>.arith_seq(make_uint2(0, 1), make_uint2(1, 2))",
      [] {
        auto exp = make_uint2(0, 1);
        const auto delta = make_uint2(1, 2);
        umem<uint2> buf {8, arith_seq(exp, delta)};
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], exp);
          exp += delta;
        }
      });
  TEST_GROUP("umem<uint2>.arith_seq(make_uint2(4,2), make_uint2(1,2), make_uint2(3,4))",
      [] {
        auto exp = make_uint2(4, 2);
        const auto delta = make_uint2(1, 2), mod = make_uint2(3, 4);
        umem<uint2> buf {8, arith_seq(exp, delta, mod)};
        exp = exp % mod;
        TEST_EQ(buf[0], make_uint2(1, 2)); // inital value is mod'd
        for (size_t i = 0; i < buf.size(); i++) {
          TEST_EQ(buf[i], exp, format("idx[", i ,"]"));
          exp += delta;
          exp %= mod;
        }
      });

    // this assumes std::mt19937 is deterministic across STL implementations
    // (I *think* this is true.  I think the seeds are give in the spec.)
    TEST_GROUP("umem<unsigned>.rnd_seq(0, 10)",
      [] {
        random_state rs {12007};
        umem<unsigned> buf {4u, rnd_seq(rs, 0u, 10u)};
        // buf.str(std::cout);
        TEST_EQ(buf[0], 1);
        TEST_EQ(buf[1], 7);
        TEST_EQ(buf[2], 8);
        TEST_EQ(buf[3], 1);
      });

  ///////////////////////////////////////////////
  // TODO: extra tests
  // - init with lambda
  // - init cyc
  // - init with random (fix values), ensure random doesn't get clobbered
  // - init with random int8_t
  // - init float types (fmod in arith_seq)
  //
  // - umem::str() (format_elem etc..)
  //
} // test_inits


static void run_format_tests()
{
  TEST_GROUP("format.misc",
      [] {
        TEST_EQ(format((const char *)nullptr), "nullptr"); // doesn't blow how
        TEST_EQ(format(2), "2");
        TEST_EQ(format(2u), "0x2");
        TEST_EQ(format(-2," ",2u), "-2 0x2");
        TEST_EQ(format(hex(2)), "00000002"); // hex() wrapper does not include 0x
        TEST_EQ(format(hex(2, 4)), "0002");
      });

  /////////////////////////////////////////////////////////////////////////////
  // char*
  TEST_GROUP("mctypes.format_elem<int8_t>",
      [] {
        const fmt_opts fos {};
        TEST_EQ(mc_format<int8_t>(0), "0");
        TEST_EQ(mc_format<int8_t>(0, fos.hex()), "0x00");
        TEST_EQ(mc_format<int8_t>(31), "31");
        TEST_EQ(mc_format<int8_t>(-16), "-16");
        TEST_EQ(mc_format<int8_t>(127), "127");
        TEST_EQ(mc_format<int8_t>(-128), "-128");
        TEST_EQ(mc_format<int8_t>(0x10), "16");
        TEST_EQ(mc_format<int8_t>(16, fos.hex()), "0x10");
        TEST_EQ(mc_format<int8_t>(16, fos.cols(0)), "16");
        TEST_EQ(mc_format<int8_t>(16, fos.cols(1)), "16");
        TEST_EQ(mc_format<int8_t>(16, fos.cols(-2)), "16");
        TEST_EQ(mc_format<int8_t>(16, fos.cols(4)), "  16");
        TEST_EQ(mc_format<int8_t>(18, fos.cols(5).hex()), " 0x12");
        TEST_EQ(mc_format<int8_t>(-4, fos.cols(5).hex()), " 0xFC");
        TEST_EQ(mc_format<int8_t>(-128, fos.hex()), "0x80");
        TEST_EQ(mc_format<int8_t>(-127, fos.hex()), "0x81");
        TEST_EQ(mc_format<int8_t>(127, fos.hex()), "0x7F");
      });
  TEST_GROUP("mctypes.format_elem<char2>",
      [] {
        const fmt_opts fos {};
        TEST_EQ(mc_format<char2>(make_char2(1,22)), "{1,22}");
        TEST_EQ(mc_format<char2>(make_char2(1,22), fos.cols(4)), "{   1,  22}");
      });
  TEST_GROUP("mctypes.format_elem<char3>",
      [] {
        const fmt_opts fos {};
        TEST_EQ(mc_format<char3>(make_char3(1,22,3)), "{1,22,3}");
        TEST_EQ(mc_format<char3>(make_char3(1,22,3), fos.cols(2)), "{ 1,22, 3}");
      });
  TEST_GROUP("mctypes.format_elem<char4>",
      [] {
        const fmt_opts fos {};
        TEST_EQ(mc_format<char4>(make_char4(1,22,3,44)), "{1,22,3,44}");
        TEST_EQ(mc_format<char4>(make_char4(1,22,3,124), fos.cols(3)), "{  1, 22,  3,124}");
        TEST_EQ(mc_format<char4>(make_char4(-128,2,18,3), fos.hex().cols(5)), "{ 0x80, 0x02, 0x12, 0x03}");
      });

  /////////////////////////////////////////////////////////////////////////////
  // uchar*
  TEST_GROUP("mctypes.format_elem<uint8_t>",
      [] {
        const fmt_opts fos {};
        TEST_EQ(mc_format<uint8_t>(0x0), "0x00");
        TEST_EQ(mc_format<uint8_t>(0x3, fos.cols(0)), "0x03");
        TEST_EQ(mc_format<uint8_t>(0x3, fos.dec()), "3");
        TEST_EQ(mc_format<uint8_t>(0x11, fos.dec()), "17");
        TEST_EQ(mc_format<uint8_t>(0x11, fos.dec().cols(5)), "   17");
        TEST_EQ(mc_format<uint8_t>(0x34), "0x34");
        TEST_EQ(mc_format<uint8_t>(0xFF), "0xFF");
        TEST_EQ(mc_format<uint8_t>(0xFF, fos.dec()), "255");
        TEST_EQ(mc_format<uint8_t>(0x34, fos.cols(6)), "  0x34");
      });
  TEST_GROUP("mctypes.format_elem<uchar2>",
      [] {
        TEST_EQ(mc_format<uchar2>(make_uchar2(1,2)), "{0x01,0x02}");
      });
  TEST_GROUP("mctypes.format_elem<uchar3>",
      [] {
        TEST_EQ(mc_format<uchar3>(make_uchar3(1,2,3)), "{0x01,0x02,0x03}");
      });
  TEST_GROUP("mctypes.format_elem<uchar4>",
      [] {
        TEST_EQ(mc_format<uchar4>(make_uchar4(1,2,3,4)), "{0x01,0x02,0x03,0x04}");
      });
}

static void run_mc_derived_function_tests()
{
  TEST_GROUP("mctypes.derived<uint2>",
      [] {
        TEST_EQ(make_uint2(1, 2), make_uint2(1, 2));
        TEST_NE(make_uint2(1, 2), make_uint2(1, 3));
        TEST_NE(make_uint2(1, 2), make_uint2(2, 2));
        //
        TEST_EQ(make_uint2(2, 2), 2u);
        TEST_NE(make_uint2(2, 3), 2u);
        //
        TEST_EQ(make_uint2(2, 2) + make_uint2(3, 4), make_uint2(5, 6));
        TEST_EQ(make_uint2(2, 2) + 1u,               make_uint2(3, 3));
        TEST_EQ(2u * make_uint2(2, 3),               make_uint2(4, 6));
        TEST_EQ(make_int2(2, 3) * -2,                make_int2(-4, -6));
        TEST_EQ(make_uint2(6, 7) % 2u,               make_uint2(0, 1));

        // TODO: many many many more (all the dervied operators etc..)
      });
}

int main(int argc, char **argv)
{
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    std::string key = arg, val;
    auto eq = arg.find('=');
    if (eq != std::string::npos) {
      key = arg.substr(0, eq + 1); // include the =
      val = arg.substr(eq + 1);
    }
    auto bad_opt = [&](std::string msg0, std::string msg1 = "", std::string msg2 = "") {
      fatal(arg, ": ", msg0, msg1, msg2);
    };

    if (arg == "-h" || arg == "--help") {
      std::cout <<
        "usage: mincu-tests.exe OPTS TEST+\n"
        "where OPTS are:\n"
        "  -v/-v2                  sets verbosity\n"
        "EXAMPLES:\n"
        " % ...\n"
        "";
      return EXIT_SUCCESS;
    } else if (arg == "-v") {
      g_os.verbosity = 1;
    } else if (arg == "-v2") {
      g_os.verbosity = 2;
    } else if (!arg.empty() && arg[0] == '-') {
      bad_opt("unexpected option");
    } else {
      bad_opt("unexpected argument");
    }
  } // for args

  run_format_tests();

  run_mc_derived_function_tests();

  run_buf_init_tests();
  // TODO: run_format_umem_tests()
  //  test_add_float_k();

  return EXIT_SUCCESS;
}

/*

1>
1>E:\dev\nvaa\experiments\mincu-tests\vs>"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\bin\nvcc.exe" -gencode=arch=compute_75,code=\"compute_75,compute_75\" -gencode=arch=compute_75,code=\"sm_75,compute_75\" --use-local-env -ccbin "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\HostX64\x64" -x cu -rdc=true  -I"E:\dev\nvaa\experiments\mincu-tests\.." -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\include"     --keep-dir x64\Debug  -maxrregcount=0   --machine 64 --compile -cudart static -std=c++17 -Xcompiler="/EHsc -Zi -Ob0" -g  -D_WINDOWS -D"CMAKE_INTDIR=\"Debug\"" -D_MBCS -D"CMAKE_INTDIR=\"Debug\"" -Xcompiler "/EHsc /W1 /nologo /Od /FS /Zi /RTC1 /MDd " -Xcompiler "/Fdmincu-tests75.dir\Debug\vc143.pdb" -o mincu-tests75.dir\Debug\mincu-tests.obj "E:\dev\nvaa\experiments\mincu-tests\mincu-tests.cu"
1>mincu-tests.cu
1>tmpxft_00004328_00000000-7_mincu-tests.cudafe1.cpp
1>
1>E:\dev\nvaa\experiments\mincu-tests\vs>"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\bin\nvcc.exe"\
  -dlink  -o mincu-tests75.dir\Debug\mincu-tests75.device-link.obj -Xcompiler "/EHsc /W1 /nologo /Od /Zi /RTC1 /MDd \
  " -Xcompiler "/Fdmincu-tests75.dir\Debug\vc143.pdb" -L"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\bin/crt" \
  -L"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\lib\x64" \
  cudadevrt.lib cudart_static.lib kernel32.lib user32.lib gdi32.lib winspool.lib shell32.lib ole32.lib \
    oleaut32.lib uuid.lib comdlg32.lib advapi32.lib \
  -forward-unknown-to-host-compiler -Wno-deprecated-gpu-targets \
  -gencode=arch=compute_75,code=compute_75 -gencode=arch=compute_75,code=sm_75
  "mincu-tests75.dir\Debug\mincu-tests.obj"

1>E:\dev\nvaa\experiments\mincu-tests\vs>"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\bin\nvcc.exe" \
  --use-local-env \
  -ccbin "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Tools\MSVC\14.38.33130\bin\HostX64\x64" \
  -x cu -rdc=true  -I"E:\dev\nvaa\experiments\mincu-tests\.." -I"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.3\include" \
  --keep-dir x64\Debug  -maxrregcount=0   --machine 64 --compile -cudart static -std=c++20 \
  --generate-code=arch=compute_75,code=[compute_75,sm_75] -Xcompiler="/EHsc -Ob0 \
  -Zi" -g  -D_WINDOWS -D"CMAKE_INTDIR=\"Debug\"" -D_MBCS -D"CMAKE_INTDIR=\"Debug\"" \
  -Xcompiler "/EHsc /W1 /nologo /Od /FS /Zi /RTC1 /MDd " -Xcompiler "/Fdmincu-tests75.dir\Debug\vc143.pdb" \
  -o mincu-tests75.dir\Debug\mincu-tests.obj \
  "E:\dev\nvaa\experiments\mincu-tests\mincu-tests.cu"

*/

/*
// FAILS to repro with custom type here
// uint2 still failed on format, but not xformat here
// It seems that the definition of (x)format_to must follow the << instances.
template <typename...Ts>
static inline void xformat_to(std::ostream &os) { }
template <typename T, typename...Ts>
static inline void xformat_to(std::ostream &os, T t, Ts...ts) {os << t; format_to(os, ts...);}
template <typename...Ts>
static inline std::string xformat(Ts...ts) {
  std::stringstream ss; xformat_to(ss, ts...); return ss.str();
}
struct foo {
  int value;
  foo(int v) : value(v) { }
};

static inline std::ostream &operator <<(std::ostream &os, foo f) {
  os << "foo{" << f.value << "}";
  return os;
}

static void micro_test()
{
  std::cout << foo(22) << "\n";
  std::cout << xformat("bar ", 32, " baz") << "\n";
  std::cout << xformat("qux ", foo(32), " zap") << "\n";
  uint2 i2 = make_uint2(1, 2);
  std::cout << "uint2 " << i2 << "\n";
  std::cout << "uint2 " << xformat(i2) << "\n";
  std::cout << "uint2 " << mincu::format(i2) << "\n";
}
// */

