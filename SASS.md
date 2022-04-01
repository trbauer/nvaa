


## IADD3

Ternary integer addition.  This instruction has two major formats:

1. Ternary 32b addition (`IADD3 ...`) accepts three parameters and writes
   out one or two destination predicates with overflow bits.

2. Extended addition 32b (`IADD3.X ...`).
   This accepts carry-ins.  It's unclear if the instruction will also
   carry out; presumably the X encoding does something different.
   Otherwise, why not just use a single instruction?


### IADD3 Accumulates to predicates

    IADD3   DstR[, DstP1 [, DstP2]], Src0, Src1, Src2

    DstR = Src0 + Src1 + Src2
    DstP1 = Low carry-out bit; logically bit[32] of the sum
    DstP2 = High carry-out bit; logically bit[33] of the sum

    0xFFFFFFFF + 0xFFFFFFFF + 0xFFFFFFFF has a carry out of two bits.


### IADD3.X extended addition propagates carry bits in predciates

    IADD3.X  Rdst, Src0, Src1, Src2 [, SrcPr3 [, SrcPr4]]
    SrcPr3 = carry in
    SrcPr4 = carry in

It appears in sequences such as

    IADD3        R6, P1, R2, c[0x0][0x170], RZ ;
    IADD3.X      R7,     ....c[0x0][0x174], RZ, P1, !PT;
    STG.E... [R6]; // writes to R6..R7

The predicate register `P1` above is used as a carry-in bit.
The second carry-in is `!PT` or 0.

64b addition will emit sequences such as the following.

    ...
    IADD3            R11, P0, P1, R34, R32, R11         {!4,Y};
    IADD3.X          R33,         R35, R33, R29, P0, P1 {!2};
    ... last adds will zip the live ranges together


The scheduling seems ...


### IADD3 Opens

- Can we use both carry-ins and carry outs at the same time?
- What is the semantic impact of .X?  Just to enable carry-ins?



## IMAD
Integer multiply-add has several formats.

     IMAD             Dst, Src0, Src1, Src2;

     Dst = Src0*Src1 + Src2


### IMAD.WIDE

Performs a 32b x 32b and stores the resulting 64b in successive registers.

     IMAD.WIDE[.U32]  Dst, Src0, Src1, Src2;
     Dst[0] =          Src0*Src1 + Src2
     Dst[1] = OVERFLOW(Src0*Src1 + Src2)

The .U32 zero-extends the result.
Leaving that option off sign extends the result.

     MAD.WIDE.U32  DST, -0x2, 0x4, RZ;
     // (-0x2 is 0xFFFFFFFE)

gives

       DST[0] = 0xFFFFFFF8
       DST[1] = 0x00000003

but

     MAD.WIDE       DST, -0x2, 0x4, RZ; // sign extend

leads to

      DST[0] = 0xFFFFFFF8
      DST[1] = 0xFFFFFFFF

This would be `-8` as a 64b value.



### IMAD.X (with Predicate Carry In)

     IMAD.X     Dst, Src0, Src1, Src2, Src3Pr;

     Dst = Src0*Src1 + Src2 + Src3Pr


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


### Address Seqeunces
Load effective address.

`R2-R3` is 64b index.
Output is `R4-R5 = ((R2-3 + const64) * 2^3)`??
Or is it
Output is `R4-R5 = ((R2-3 * 2^3) + const64)`??

    T buf[N];
    ...
    buf + ix*sizeof(T)

    `R2-R3` are 64b `ix` (LO and HI)
    `c[0x0][0x170]` = LO(buf)
    `c[0x0][0x174]` = HI(buf)

    LEA       R4|P0, R2,  c0[0x170],      0x3;
    LEA.HI.X  R5,    R2,  c0[0x174], R3,  0x3, P0;

    STG.E.64  [R4.64], ...;

Semantics:

    R4|P0 = LO32(R2 << 3) + c0[0x170] + RZ
    R5    = LO32(R3 << 3) + c0[0x174] + HI32(R2 << 3) + P0


## LOP3

The `LOP3` instruction has several predicate inputs and an optional
predicate destination.  The predicate output appears to be a non-zero flag.

[ what're the extra arguments? Output non-zero or zero?? ]



## Sampler Messages


    const sampler_t sampler0 =
        CLK_NORMALIZED_COORDS_FALSE |
        CLK_ADDRESS_CLAMP_TO_EDGE |
        CLK_FILTER_NEAREST;

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
s2 comes from `c0[0x17C]` and `UR14` from `c0[0x168]`.
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
