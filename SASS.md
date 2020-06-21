


## IADD3

Ternary integer addition.  This instruction has two major formats:

1. Ternary 32b addition (`IADD3 ...`) accepts three parameters and writes
   out one or two destination predicates with overflow bits.

2. Extended addition 32b (`IADD3.X ...`).
   This accepts carry-ins.  It's unclear if the instruction will also
   carry out; presumably the X encoding does something different
   otherwise why not make it all one.


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

64b addition will emit sequences such as the following.

  ...
  IADD3            R11, P0, P1, R34, R32, R11 {!4,Y};
  IADD3.X          R33,         R35, R33, R29, P0, P1 {!2};
  ... last adds will zip the live ranges together


The scheduling seems


### IADD3 Opens

- Can we use both carry-ins and carry outs at the same time
- What is the semantic impact of .X?



## IMAD
Integer multiply-add has several formats.

     IMAD             Dst, Src0, Src1, Src2;

     Dst = Src0*Src1 + Src2


### IMAD.WIDE

Performs a 32b x 32b and stores the resulting 64b in successive registers.

     IMAD.WIDE[.U32]  Dst, Src0, Src1, Src2;
     Dst[0] =          Src0*Src1 + Src2
     Dst[1] = OVERFLOW(Src0*Src1 + Src2)

This is used in generating 64b multiplication results
The .U32 zero-extends the result.  Leaving that option
off sign extends the result.

     MAD.WIDE.U32  DST, -0x2, 0x4, RZ;
     // (-0x2 is 0xFFFFFFFE)

gives

       DST[0] = 0xFFFFFFF8
       DST[1] = 0x00000003

but

     MAD.WIDE       DST, -0x2, 0x4, RZ;

leads to

      DST[0] = 0xFFFFFFF8
      DST[1] = 0xFFFFFFFF

This would be `-8` as a 64b value.



### IMAD.X (with Predicate Carry In)

     IMAD.X     Dst, Src0, Src1, Src2, Src3Pr;

     Dst = Src0*Src1 + Src2 + Src3Pr


### 64b Multiplication

  Signed multiplication uses the following sequence.
  // R3:R2 *s R5:R4
  IMAD             R7,  R2, R5,  RZ;
  IMAD.WIDE.U32    R8,  R2, R4,  RZ;
  IMAD             R11, R3, R4,  R7;
  IADD3            R9,  R9, R11, RZ;
  // output is R9:R8


  // R3:R2 *u R5:R4
  Unsigned multiplication uses the following sequence.
  IMAD             R7,  R2, R5,  RZ
  IMAD.WIDE.U32    R8,  R2, R4,  RZ
  IMAD             R11, R3, R4,  R7
  IADD3            R9,  R9, R11, RZ
  // output is R9:R8




## LOP3

The `LOP3` instruction has several predicate inputs and an optional
predicate destination.  The predicate output appears to be a non-zero flag.

[ what're the extra arguments? ]

