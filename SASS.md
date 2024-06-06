


## IADD3

Ternary integer addition.  This instruction has two major formats:

1. Ternary 32b addition (`IADD3 ...`) accepts three parameters and writes
   out one or two destination predicates with overflow bits.

2. Extended addition 32b (`IADD3.X ...`).
   This accepts carry-ins.  It's unclear if the instruction will also
   carry out; presumably the X encoding does something different.
   Otherwise, why not just use a single instruction?


### IADD3 Accumulates to predicates

    IADD3   DstR[, DstPr0 [, DstPr1]], Src0, Src1, Src2

    DstR = Src0 + Src1 + Src2
    DstPr0 = Low carry-out bit; logically bit[32] of the sum
    DstPr1 = High carry-out bit; logically bit[33] of the sum

E.g. `0xFFFFFFFF + 0xFFFFFFFF + 0xFFFFFFFF` has a carry out of two bits.
If the `PT` (predicate true) register is used, then the signal is discarded
and is often absent in assembly.


### IADD3.X extended addition propagates carry bits in predciates

    IADD3.X  Rdst, Src0, Src1, Src2 [, SrcPr0 [, SrcPr1]]
    SrcPr0 = carry in
    SrcPr1 = carry in

It appears in sequences such as

    IADD3        R6, P1, R2, c[0x0][0x170], RZ ;
    IADD3.X      R7,     ....c[0x0][0x174], RZ, P1, !PT;
    STG.E... [R6]; // writes to R6..R7

The predicate register `P1` above is used as a carry-in bit.
The second carry-in is `!PT` generates a `0` value.

Int64 addition emit sequences such as the following.

    ...
    IADD3            R11, P0, P1, R34, R32, R11         {!4,Y};
    IADD3.X          R33,         R35, R33, R29, P0, P1 {!2};
    ... last adds will zip the live ranges together

The scheduling seems to stall for 4 clocks (the bypass latency).


### IADD3 Opens
- Can we use both carry-ins and carry outs at the same time?
- What is the semantic impact of .X?  Just to enable carry-ins?





## IMAD
Integer multiply-add has several formats.

### 32b x 32b + 32b => 32b

     IMAD             Dst, Src0, Src1, Src2;

     Dst = Src0*Src1 + Src2


### IMAD.WIDE

Performs a 32b x 32b + 64b and stores the resulting 64b in successive registers.
The intermediate product is 64b and zero or sign extends before being added
to an adjacent pair of addend registers.

     IMAD.WIDE[.U32]  Dst, Src0, Src1, Src2;
     Dst[0] =          Src0*Src1 + Src2
     Dst[1] = OVERFLOW(Src0*Src1 + Src2)

The `.U32` zero-extends the result.
Leaving that option off sign extends the result.

     MAD.WIDE.U32  DST, -0x2, 0x4, RZ;
     // (-0x2 is 0xFFFFFFFE)

The above gives.

       DST[0] = 0xFFFFFFF8
       DST[1] = 0x00000003

but

     MAD.WIDE       DST, -0x2, 0x4, RZ; // sign extend

leads to

      DST[0] = 0xFFFFFFF8
      DST[1] = 0xFFFFFFFF

This would be `-8` as an Int64.

### IMAD.X (with Predicate Carry In)

     IMAD.X     Dst, Src0, Src1, Src2, SrcPr;

     Dst = Src0*Src1 + Src2 + SrcPr

Often we see IMAX.X used to add a carry-in bit from another part of a larger
computation.  It's also a cheap way to convert a predicate to int (0 or 1).
`IADD3` does the same thing, but `IMAD` runs in the other ALU giving better
concurrency.

### 64b Multiplication

Signed multiplication uses the following sequence.

    // R9:R8 = R3:R2 *s R5:R4
    IMAD             R7,  R2, R5,  RZ;
    IMAD.WIDE.U32    R8,  R2, R4,  RZ;
    IMAD             R11, R3, R4,  R7;
    IADD3            R9,  R9, R11, RZ;

  Unsigned multiplication uses the following sequence.

    // R9:R8 = R3:R2 *u R5:R4
    IMAD             R7,  R2, R5,  RZ
    IMAD.WIDE.U32    R8,  R2, R4,  RZ
    IMAD             R11, R3, R4,  R7
    IADD3            R9,  R9, R11, RZ


### LEA and Address Seqeunces
Load effective address.

`R2-R3` is 64b index.  The constant is the base 64b value.
Output is `R4-R5 = ((R2-3 * 2^3) + const64)`.

    T buf[N];
    ...
    buf + ix*sizeof(T)

This produces the following `LEA`s.

    `R2-R3` are 64b `ix` (LO and HI)
    `c[0x0][0x170]` => LO(buf)
    `c[0x0][0x174]` => HI(buf)

    LEA       R4|P0, R2,  c0[0x170],      0x3;
    LEA.HI.X  R5,    R2,  c0[0x174], R3,  0x3, P0;

    STG.E.64  [R4.64], ...;

Semantics:

    R4|P0 = LO32(R2 << 3) + c0[0x170] + RZ
    R5    = LO32(R3 << 3) + c0[0x174] + HI32(R2 << 3) + P0

More generally expect to see `B + (IX << S)` as the following.

    LEA      DST[0],P0, IX[0], B[0], S;
    LEA.HI.X DST[1],    IX[0], B[0], IX[1], S, P0;


## LOP3

The `LOP3` instruction has several predicate inputs and an optional
predicate destination.  The predicate output appears to be a non-zero flag.

[ what're the extra arguments? Output non-zero or zero?? ]

## `SHF` - Funnel Shifter

`(SRC2|SRC0) >> SRC1`

`U32` means zero fill.  The `.HI` means to take the high word of the result.

```
SHF.R.U32.HI  R3, RZ,     0xb,    R3               {!4,Y,^2};
```
This shifts `(R3:RZ) >> 0xB` with zero fill and takes the top.
So it's really just `(R3>>0xB)`.

If I picked the low word, we would get the junk shifted off the bottom of `SRC2`.

Another example is the uniform variant.

```
USHF.R.S32.HI  UR7, URZ,  0x1f,   UR6
```
This is a sign-extension, it takes bit 31 and extends it down to 0.
This is `((UR6:URZ) >>sx 31)` (sign extension of top bit).

## PRMT

Permutes A pair of 32 values and does a byte select on them.
An example would be a misaligned DWord load that needs to assemble 4 bytes into a 32b value.

See: https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-prmt


We have the following:

     // barrier:592:                            memcpy(__destination, __source, __total_size);
      LDG.E.U8.SYS  R6, [R2+0x1]                         {!4,+3.W,+1.R};
      LDG.E.U8.SYS  R7, [R2]                             {!4,+3.W,+1.R};
      LDG.E.U8.SYS  R5, [R2+0x2]                         {!4,+4.W,+1.R};
      LDG.E.U8.SYS  R4, [R2+0x3]

This is loading bytes `B[3,2,1,0] = {R4,R5,R6,R7}`.  The `PRMT` assembles these into a single 32b value.

      // barrier:592:                            memcpy(__destination, __source, __total_size);
      PRMT      R6,     R6,     0x7604, R7               {!4,Y,^3};
      PRMT      R5,     R5,     0x7054, R6               {!4,Y,^4};
      PRMT      R6,     R4,     0x654,  R5               {!2,^5};

The general form of `PRMT` is to byte select from the 8 bytes of `(SRC2<<32|SRC0)`.
There are some throw-away values in the high selectors that should have no effect.
Then this really is selecting.

      PRMT      R6,     R6,     0xXX04, R7               {!4,Y,^3};
      // R6 = XXB[1]B[0]
      PRMT      R5,     R5,     0xX054, R6               {!4,Y,^4};
      // R5 = XB[2]B[1]B[0]
      PRMT      R6,     R4,     0x0654, R5               {!2,^5};
      // R6 = B[3]B[2]B[1]B[0]  (R4.B0|R5.B2<<16|R5.B1<<8|R5.B0)

The above was a `cuda::memcpy_async` on a part that made it synchronous,
but had to honor byte alignment for some reason (my fault?).

https://nvidia.github.io/cccl/libcudacxx/extended_api/asynchronous_operations/memcpy_async.html

## Sampler Messages

```C++
    const sampler_t sampler0 =
        CLK_NORMALIZED_COORDS_FALSE |
        CLK_ADDRESS_CLAMP_TO_EDGE |
        CLK_FILTER_NEAREST;
```

Yields:

      TLD.SCR.LZ.CL.NODEP  R12, R10, R0, R7,    UR4,    0x0,    2D


`.CL` seems to mean match `CLK_ADDRESS_CLAMP_TO_EDGE` (but not `CL_ADDRESS_CLAMP`).
Twiddling the others doesn't seem to affect things.

We can tell this is generally:

    TLD.... DST1.ZW, DST0.XY, Us, Vs, SamplerIx?, Imm??, <GEOM>

`SamplerIx`? parameter above seems to come from const buffer and sometimes
derived via pair of const buffer inputs.
E.g. `c0[0x178]`, `c0[0x190]`, `ULOP3.(s0&s1|~s1&s2) ... UR14, 0xfffff, UR5`, derived
`s0&s1|~s1&s2` with 0xFFFFF is `s0&0xFFFFF|0xFFF00000&s2`
`s2` comes from `c0[0x17C]` and `UR14` from `c0[0x168]`.
s0 is the LSB and s2 must be some sort of index.
This is a pairing function `s2 * 1MB + s0` and we see one of the indexes is shifted up 20 (1MB).

Given:

      // images.cl:57:    px = read_imagef(srcImg0, sampler0, (int2)(pX,pY));
      TLD.SCR.LZ.NODEP  R4, R2, R8, R9, UR4, 0x0, 2D {!1,Y,+6.W};

Able to prove that:
 `R2` holds `float4::s0`
 `R3` holds `float4::s1`
 `R4` holds `float4::s2`
 `R5` holds `float4::s3`


## `VOTE`


### __all_sync

```C++
    if (__all_sync(0xFFFFFFFF, val >= 2.0f)) {
        dsts[gid] = 1.0f;
    }
```

Produces CUDA12 sm90

```
           FSETP.GE.AND  P0, PT,     R2,     2,      PT       {!13,Y,^3};
           VOTE.ALL  P0,     P0                               {!13,Y};
     @!P0  EXIT
     .... do the store
```

### __any_sync

If we reduce the sync mask.

```C++
    if (__any_sync(0xFFFF0000, val >= 2.0f)) {
        dsts[gid] = 1.0f;
    }
```


We get (CUDA12:sm90) PTX `vote.sync.any.pred 	%p2, %p1, %r5`.

    IMAD.MOV  R0,     RZ,     RZ,     -0x10000         {!1}; // 0xFFFF`0000
    FSETP.GE.AND  P0, PT,     R2,     2,      PT       {!3,Y,^3};
    WARPSYNC.EXCLUSIVE  R0                             {!10};
    VOTE.ANY  P0,     P0                               {!13,Y}
    ...

### __ballot_sync

`unsigned __ballot_sync(unsigned mask, int predicate);`

      auto x = __ballot_sync(0xFFFFFFFF, val >= 2.0f);
      dsts[gid] = x;

Produces PTX `vote.sync.ballot.b32 %r6, %p1, %r5` and SASS:

      FSETP.GE.AND  P0, PT,     R2,     2,      PT       {!13,Y,^3};
      VOTE.ANY  R0,     PT,     P0                       {!4,Y};

Enabling a non-uniform mask and testing the output.

    auto x = __ballot_sync(0xF0F0F0F0u, val >= 3.0f);
    if (x == 0x000000FF)
      dsts[gid] = val;

Gives the following PTX.

```C++
setp.ge.f32 	%p1, %f1, 0f40400000; // 3.0f
mov.u32 	%r5, -252645136; // 0xF0F0F0F0
vote.sync.ballot.b32 	%r6, %p1, %r5; // %r6 is bit mask
setp.ne.s32 	%p3, %r6, 255; // 0xFF
@%p3 bra ... around store
```

SASS gives.

```C++
      IMAD.MOV  R0,     RZ,     RZ,     -0xf0f0f10       {!1}; // 0xF0F0F0F0u
      FSETP.GE.AND  P0, PT,     R7,     3,      PT       {!3,Y,^3}; // 3.0f
      WARPSYNC.EXCLUSIVE  R0                             {!10};
      VOTE.ANY  R0,     PT,     P0                       {!4,Y};
      ISETP.NE.AND  P0, PT,     R0,     0xff,   PT       {!13,Y};
@P0   EXIT
```

Seems like `VOTE.{ANY,ALL}` where an optional data register destination holds
the bitmask (`R0` here) of those passing the predicate for `ANY`.
`ALL` wouldn't make sense since the values are either 0x0 or ~0x0.

## `MATCH`

### __match_any_sync/__match_all_sync

Like the conflict operation.
__match_any_sync
Returns mask of threads that have same value of value in mask

__match_all_sync
Returns mask if all threads in mask have the same value for value; otherwise 0 is returned. Predicate pred is set to true if all threads in mask have the same value of value; otherwise the predicate is set to false.

    unsigned x = __match_any_sync(0xFFFFFFFFu, val);
    dsts[gid] = x;

This produce PTX

	match.any.sync.b32 	%r7, %r5, %r6;

Running produces

    float[32]: 0x0000000E00010000:
    0000:   0.000  0.000  0.000  1.000  0.000  0.000  0.000  0.000
    0008:   2.000  2.000  0.000  0.000  0.000  0.000  0.000  0.000
    0010:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    0018:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    uint32_t[32]: 0x0000000E00000000:
    0000:   0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0x00000008  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7
    0008:   0x00000300  0x00000300  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7
    0010:   0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7
    0018:   0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7  0xFFFFFCF7

I reduced the mask and made element 1 -0.0f! (Does it do bitwise comparison?)

      unsigned x = __match_any_sync(0x0000FFFFu, val);
      dsts[gid] = x;

PTX

	  mov.u32 	%r6, 65535;
	  match.any.sync.b32 	%r7, %r5, %r6;

SASS

    // sm_70_rt.hpp:122:                 return __match32_any_sync(mask, __float_as_uint(value));
    MATCH.ANY  R0,    R2                               {!2,+1.W,^3};

This produces output.

    float[32]: 0x0000000E00010000:
    0000:   0.000  -0.000  0.000  1.000  0.000  0.000  0.000  0.000
    0008:   2.000  2.000  0.000  0.000  0.000  0.000  0.000  0.000
    0010:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    0018:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    uint32_t[32]: 0x0000000E00000000:
    0000:   0x0000FCF5  0x00000002  0x0000FCF5  0x00000008  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5
    0008:   0x00000300  0x00000300  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5
    0010:   0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5
    0018:   0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5  0x0000FCF5

Do exited channels count?

    auto val = srcs[gid];
    if (threadIdx.x >= 8) {
      return;
    }
    unsigned x = __match_any_sync(0x0000FFFFu, val);
    dsts[gid] = x;

    %> micros
    float[32]: 0x0000000E00010000:
    0000:   0.000  -0.000  0.000  1.000  0.000  0.000  0.000  0.000
    0008:   2.000  2.000  0.000  0.000  0.000  0.000  0.000  0.000
    0010:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    0018:   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
    uint32_t[32]: 0x0000000E00000000:
    0000:   0x000000F5  0x00000002  0x000000F5  0x00000008  0x000000F5  0x000000F5  0x000000F5  0x000000F5
    0008:   0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000
    0010:   0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000
    0018:   0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000  0x00000000

So...
 * it partitions values to equivalence classes
 * it doesn't honor float comparison (-0.0f != 0.0f)
 * The masked out channels (top 16) are not compared against.
 * Exited channels don't are excluded from the match.

## Querying Address Spaces

```C++
    asm volatile (
        "{\n"
        ".reg .pred p0;\n"
// uncomment one of these
//        "isspacep.const  p0, %1;\n"
//        "isspacep.shared  p0, %1;\n"
//        "isspacep.shared::cta  p0, %1;\n"
//        "isspacep.shared::cluster p0, %1;\n"
//        "isspacep.global p0, %1;\n"
        "selp.u32  %0, 1, 0, p0;\n"
        "}\n"
```



### Summary

|Space|PTX|SASS|
|----|---|---|
|Constant|`isspacep.const`|compare against program constant|
|Shared|`isspace.shared`|`QSPC.E.S PR,REG, [REG]`|
|Shared CTA|`isspace.shared::cta`|`QSPC.E.S PR,REG, [REG]`|
|Shared Cluster|`isspace.shared::cluster`|`QSPC.E.D PR,REG, [REG]`|
|Global|`isspace.global`|`QSPC.E.G PR,REG, [REG]`|


### Constant

```
      ULDC.64   UR4,    c0[0xd0]                         {!2};
      ISETP.GE.U32.AND  P1, PT, R4.reu, UR4,    PT       {!2};
      ISETP.GE.U32.AND  P0, PT, R4,     UR4,    PT       {!2};
      ISETP.GE.U32.AND.EX  P1, PT, R5,  UR5,    PT,     P1       {!4,Y};
      ISETP.GE.U32.AND.EX  P0, PT, R5,  UR5,    !P1,    P0       {!1};
```
The first predicate is the `.AND` carry in and the second is the `.EX`.

Some sort of range check against a constant limit in `c0[0xD0]`.

### Global

      ULDC.64   UR4,    c0[0xd0]                         {!1};
      IMAD.MOV  R7,     RZ,     RZ,     R3               {!1};
      ISETP.GE.U32.AND  P2, PT, R2.reu, UR4,    PT       {!2};
      ISETP.GE.U32.AND  P1, PT, R2,     UR4,    PT       {!1};
      ...
      QSPC.E.G  P0,     RZ,     [R6]                     {!1,+1.W};
      ISETP.GE.U32.AND.EX  P2, PT, R3,  UR5,    PT,     P2       {!4,Y};
      ISETP.GE.U32.AND.EX  P1, PT, R3,  UR5,    !P2,    P1       {!4,Y};
      PLOP3.(s0&~s1&s2)  P0, PT, P0,    P1,     PT,     0x20,   0x0      {!4,Y,^1};
      SEL       R2,     RZ,     0x1,    !P0              {!4,Y};


## ISETP

    ISETP.GE.U32.AND    DST0, DST1, SRC0, SRC1, PR0 // PT as PR1
    ISETP.GE.U32.AND.EX DST0, DST1, SRC0, SRC1, PR0, PR1

  * If the higher bits are strictly less (not GT), then fail (F).
  * If the higher bits are strictly greater, then pass (T).
  * However, if they are equal at this level, then delegate back to the
    lower level (inductive input).  Note, the base case would omit `.EX`,
    so `GE` would be T for equal and F for `GT`.

