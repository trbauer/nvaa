#include "mincu.hpp"

#include <iostream>
#include <cstdio>

using namespace mincu;


#define TEST_FAIL(...) \
  mincu::fatal("near line ", __LINE__, ": ", __VA_ARGS__);

static void start_test(const char *lbl)
{
  std::cout << "============= " << lbl << "\n";
}

void test_inits()
{
  start_test("test_inits");

  /////////////////////////////////////////////
  // constant sequence initializer
  umem<unsigned> buf0(8);
  if (buf0.size() != 8)
    TEST_FAIL("unexpected buffer size");

  umem<unsigned> buf1(8, const_seq(3u));
  for (size_t i = 0; i < buf1.size(); i++) {
    if (buf1[i] != 3u) {
      TEST_FAIL("const_seq: expected 3u");
    }
  }
  buf1.init(const_seq(7u));
  for (size_t i = 0; i < buf1.size(); i++) {
    if (buf1[i] != 7u) {
      TEST_FAIL("const_seq: expected 7u");
    }
  }

  /////////////////////////////////////////////
  // arithmetic sequence initializer
  umem<unsigned> buf2 {8, arith_seq(0u)};
  for (size_t i = 0; i < buf2.size(); i++) {
    if (buf2[i] != i) {
      TEST_FAIL("arith_seq(0u): mismatch");
    }
  }

  umem<unsigned> buf3 {8, arith_seq(77u, 2u, 5u)};
  unsigned val = 77u % 5;
  for (size_t i = 0; i < buf3.size(); i++) {
    if (buf3[i] != val) {
      TEST_FAIL("arith_seq(77u, 2u, 5u): mismatch");
    }
    val = (val + 2u) % 5u;
  }

  ///////////////////////////////////////////////
  // TODO: extra tests
  // - init cyc
  // - init with random (fix values), ensure random doesn't get clobbered
  // - init with random int8_t
  // - init with lambda
  // - float types
  //
  // - umem::str() (format_elem etc..)
  //
}

int main()
{
  test_inits();

  return EXIT_SUCCESS;
}