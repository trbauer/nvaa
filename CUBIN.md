## OPEN QUESTIONS

Some questions to solve

* `__assertfail`'s signature


   `extern void __assertfail(const char *expr, const char *file, int32_t lno, int64_t, int64_t);`

   In PTX

  `.extern .func __assertfail
  (  .param .b64 __assertfail_param_0,
     .param .b64 __assertfail_param_1,
     .param .b32 __assertfail_param_2,
     .param .b64 __assertfail_param_3,
     .param .b64 __assertfail_param_4
  )`

   In SASS

    R4-5 = STR(expr),
    R6-7 = FILE(__FILE__),
    R8 = __LINE__
    R9-10 = ??,0x0? (SRZ)
    R11-12 = ??,0x2
    R13 = 0x0 (RZ)  (set over the range)

* `__assertfail`'s text
  Can I copy it from FN ptr?

* `__assertfail`'s how does it affect the binary

* custom extern function???

* What does the `malloc` text look like?  Does the heap pointer get passed in like stack?

* What does the `malloc` do when it runs out of memory.  What are the addressing.

* What does `printf` do

* Device Side Errors
   - Does assert interrupt dispatch?  Can I test with USM?
   - What's the signature of `__assertfail`
      infer from PTX and ISA
   - Does an assert that causes a barrier miss TDR?
   - What does a stack overflow look like?
   - What does a malloc failure look like?
   - What does an SLM bounds violation look like?
   - What does a global bounds violation look like?  (Careful...)



## PAYLOAD LAYOUT

Kernel arguments are passed in `c[0x0][...]`; the first block typically holds
dispatch built-in uniforms.  User uniforms usually start at `c[0x0][0x160]`.

    c[0x0][0x0] is blockDim.x
    c[0x0][0x4] is blockDim.y
    c[0x0][0x8] is blockDim.z
    ...
    c[0x0][0x28] is the top of the stack (grows down)
    ...
    c[0x0][0x160] is start of uniform parameters
    ...
    we are limited to 4k of payloads
    c[0x][0x0...0xFFF]
    ...
    structures are passed directly (never indirectly)

c[0x0][] = kernel args
c[0x1][] = ???
c[0x2][] = compiler (e.g. math constants c.f. bisect-large sample)
c[0x3][] = user consts (e.g. __constant__)


## Sections

Some ELF sections are:

  * `.nv.shared.FUNCTION` holds SLM allocs

  * `.nv.info`
      - EIATTR_REGCOUNT
      - EIATTR_MAX_STACK_SIZE
      - EIATTR_MIN_STACK_SIZE
      - EIATTR_FRAME_SIZE

  * `.nf.info.FUNCTION`  holds param
      - EIATTR_PARAM_CBANK
      - EIATTR_CBANK_PARAM_SIZE
      - EIATTR_KPARAM_INFO*
      - EIATTR_MAXREG_COUNT
      - EIATTR_EXTERNS?
      - EIATTR_EXIT_INSTR_OFFSETS
      - EIATTR_CRS_STACK_SIZE

  * Constant sections
    - `.section  .nv.constant0.FUNCTION,"a",@progbits`
    - `.zero   368 ; // allocation for c[0x0]????`

   (More output)

      //--------------------- .nv.constant0._Z11test_assertPfS_ --------------------------
        .section  .nv.constant0._Z11test_assertPfS_,"a",@progbits
        .align  4
      .nv.constant0._Z11test_assertPfS_:
        .zero   368

      //--------------------- .text._Z11test_assertPfS_ --------------------------
        .section  .text._Z11test_assertPfS_,"ax",@progbits
        .sectioninfo  @"SHI_REGISTERS=24"
        .align  128
              .global         FUNCTION
              .type           FUNCTION,@function
              .size           FUNCTION,(END_LABEL - FUNCTION)
              .other          FUNCTION,@"STO_CUDA_ENTRY STV_DEFAULT"


## Assembler Syntax

    `index@(...)` takes a symbol table (.symtab) index



## Format of `.nv.info*`


### Per Module Info

    .section  .nv.info,"",@"SHT_CUDA_INFO"

    template <u8 CODE>
    struct WPS
    {
      u8[2]  tag {0x04, CODE}; // 0xXX04
      u16    sizeof_rest; // appears to be 0x8
      //
      // index is a .symtab index (a FUNC)
      struct{u32 index; u32 regs} per_function;
    };

    using EIATTR_REGCOUNT = WPS<0x2F>; // 0x2F04
    using EIATTR_MAX_STACK_SIZE = WPS<0x23>;
    using EIATTR_MIN_STACK_SIZE = WPS<0x12>;
    using EIATTR_FRAME_SIZE = WPS<0x11>;

    // for multiple functions the they appear to just use a
    // pair of entries
    EIATTR_REGCOUNT .. for __global__ kernel1
    EIATTR_REGCOUNT .. for __global__ kernel2



### Per Function Info


    // c[0x0][0x0] is blockDim.x
    // c[0x0][0x4] is blockDim.y
    // c[0x0][0x8] is blockDim.z
    // ...
    // c[0x0][0x28] is the stack top
    // ...
    // c[0x0][0x160] is start of uniform parameters
    struct EIATTR_PARAM_CBANK {
      uint8_t  tag []{0x04, 0x0a};
      uint16_t sizeof_rest; // the sum total of all the next bytes
      //
      // e.g targets .nv.constant0._Z11test_assertPfS_PKy6custom
      // .nv.constant0._Z11test_assertPfS_PKy6custom:
      // .zero   392
      uint32_t section_index;
      uint16_t start_offset; // e.g. {0x160} where params start
      uint16_t sizeof_elems; // e.g. {0x0028} same as EIATTR_CBANK_PARAM_SIZE::sizeof_elems
    };
    struct EIATTR_CBANK_PARAM_SIZE {
      uint8_t  tag[] {0x03, 0x11};
      // no sizeof_rest!
      uint16_t sizeof_elems; // sizeof explicit argument layout
    };
    // Layout
    // e.g. (float *dsts, float *src0s, const uint64_t *src1s,
    //       custom unif, bool z, float k)
    // float       *dsts  = {..., 0x000, 0x0, 0x000, {0x00, 0xF0, 0x21, 0x00}} // 8 B
    // float       *src0s = {..., 0x000, 0x1, 0x008, {0x00, 0xF0, 0x21, 0x00}} // 8 B
    // const float *src1s = {..., 0x000, 0x2, 0x010, {0x00, 0xF0, 0x21, 0x00}} // 8 B
    // custom       unif  = {..., 0x000, 0x3, 0x018, {0x00, 0xF0, 0x41, 0x00}} // 16 B = ~float4
    // bool         z     = {..., 0x000, 0x4, 0x028, {0x00, 0xF0, 0x05, 0x00}} // 4 B = std::max(4,sizeof(bool))
    // fat          f     = {..., 0x000, 0x5, 0x02C, {0x00, 0xF0, 0x01, 0x20}} // 0x800 B = ~float[512] =
    // float        k     = {..., 0x000, 0x6, 0x82C, {0x00, 0xF0, 0x11, 0x00}} // 4 B
    //  ==> 0x0830
    struct EIATTR_KPARAM_INFO {
      u8[2]  tag {0x04, 0x17};
      u16    sizeof_rest; // the sum total of all the next bytes
      u32    zero????; // appears to always be zero
      u16    param_index;
      u16    arg_offset; // offset into the argument section
      u8[4]  attrs???; // some sort of attributes; e.g. .byte 0x00, 0xf0, 0x21, 0x00; byte[2]&0xF seems to correspond to sizeof
    };

    struct EIATTR_MAXREG_COUNT {
      u8[2] tag {0x03, 0x1b};
      u16 value; // e.g. 0x00FF
    };

    struct EIATTR_FRAME_SIZE {
      u8[2]  tag {0x04, 0x11};
      u16    sizeof_rest;
      u32[*] index; // index@(_function)
    };
    struct EIATTR_EXTERNS {
      u8[2]   tag {0x04, 0x0F};
      u16     sizeof_rest;
      u32[*] symbol_indices // e.g. index@(__assertfail)??
    };
    struct EIATTR_EXIT_INSTR_OFFSETS {
      u8[2] tag {0x04, 0x1c};
      u16 sizeof_rest;
      u32[*] offsets = ... // offsets of EXIT ops
    };
    struct EIATTR_CRS_STACK_SIZE {
      u8[2] tag {0x04, 0x1e};
      u16 next_patch; // the sum total of all the next bytes
      u32 ???; // e.g. {0x0}
    };