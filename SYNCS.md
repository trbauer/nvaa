# SYNCS Info

// https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-mbarrier

The `SYNCS` instruction is related to the `mbarrier` ptx abstractions.
Roughly, speaking mbarriers seem to be 64b shared memory state consisting of three bitfields

- *expected* (expected thread arrivals)
- *next_expected* phase initial expected arrivals
- *pending_tx* (pending memory transactions)
- other stuff (phase parity etc...)

The symbol *arrived* can be thought of as decrementing *expected*.

The `SYNCS` operation are all fancy 64b atomic on various parts of the 64b bitfield.


## MBarrier Initialization - `SYNCS.EXCH.64`

Initialization is as simple as an atomic exchange.
```
      S2UR      UR5,    SR_CgaCtaId     {!1,+1.W}; // create group cluster shared memory addr to barrier
      UMOV      UR4,    0x400           {!1}; // shared memory address
      UMOV      UR6,    0x1fffc0        {!1}; // barrier's initial binary value (expect 32)
      UMOV      UR7,    0x7fff0000      {!1}; // barrier's initial binary value
      ...
      UPRMT     UR4,    UR5,    0x654,  UR4 {!1,^1}; // (UR5[7:0] << 24) | UR4[23:0]
      // cluster group shared memory address
      ...
      FENCE.VIEW.ASYNC.S                {!10,+1.W}; // flush everything
      SYNCS.EXCH.64  URZ, [UR4], UR6    {!2,+1.W,+2.R}; // swap the values, dumping the old value (URZ)
```
The pending count seems to be stored negative. E.g. `0x1fffc0` is init of 32.
The value -32 is `0xFFFFFFE0`, so shifted left 1 gives `...FC0`.
Thus, the bottom bit some other status bit (parity?).
The syntax seems to indicate arrivals are counted up from negative 0 (`A1` means 1 arrives).

## MBarrier Thread Arrival and ExpectTX `SYNCS.ARRIVE.TRANS64.*`
Signaling thread arrival is fused with increasing the expected count

- `SYNCS.ARRIVE.TRANS64` -- `mbarrier.arrive.expect_tx.*` -- arrive = 1; expect_tx += Src1
- `SYNCS.ARRIVE.TRANS64.OPTOUT` -- `mbarrier.arrive_drop.expect_tx.*`
- `SYNCS.ARRIVE.TRANS64.A1T0` -- `mbarrier.arrive*` (without Src1) -- arrived += 1, expect_tx += 0
- `SYNCS.ARRIVE.TRANS64.ART0` -- `mbarrier.arrive*` (with Src1) -- arrived += Src1, expect_tx += 0
- `SYNCS.ARRIVE.TRANS64.RED.A0TR` - `mbarrier.expect_tx.*` -- arrived = 0, expect_tx += Src1
- `SYNCS.ARRIVE.TRANS64.RED.A0TX` - `mbarrier.complete_tx.*` -- simulate completion

```
// mbarrier.arrive.shared.b64  arv_tkn, [mbar]; // implicit arrive count
SYNCS.ARRIVE.TRANS64.A1T0  RZ, [UR4], RZ
```

Signal mbarrier arrival (`mbarrier.arrive`) with arriving count of 1 and 0 new `expected_tx`s.
`SYNCS` signal `ARRIVE` to barrier in shared memory address `UR4`.
The destination operand is `RZ` because the sample I used to generate this
clobbers the variable, but usually that would be the arrival token
(for parity check with wait).  The `Src1` operand is `RZ` because `A1T0` has fixed arrivals.

```
    // mbarrier.arrive.shared.b64  arv_tkn, [mbar], 1; // explicit arrive count of 1
      // (fancy way of writing 0x1 to R0)
      HFMA2.MMA  R0,    -RZ,    RZ,     0,      5.9604644775390625e-08 {!1}; // 0x00000001
      SYNCS.ARRIVE.TRANS64.A1T0  RZ, [UR4], RZ     {!1,^1};
```
`ART0` probably stands for "arrive register (`R2`/Src1) number of times and do
not increase outstanding memory transactions (`T0`).
Interestingly, the compiler did not match the immediate value of 1 and use the prior code.
```
   // mbarrier.arrive.shared.b64  arv_tkn, [mbar], 4; // explicit arrive of 4
      MOV       R2,     0x4                        {!1};
      ...
      SYNCS.ARRIVE.TRANS64.ART0  RZ, [UR4], R2     {!1,+2.R};
```

### Expect and Arrive
```
      // mbarrier.complete_tx.shared.b64   [mbar],     32; // simulate completions
      IMAD.MOV  R0,     RZ,     RZ,     0x20             {!1,^1};
      ...
      SYNCS.ARRIVE.TRANS64.RED.A0TX  RZ, [UR4], R0       {!2,+2.R,^3};
```

### Arrive and Drop
```
  // mbarrier.arrive_drop.shared.b64  arv_tkn, [shMem]; // implicit arrive count
      SYNCS.ARRIVE.TRANS64.OPTOUT.A1T0  RZ, [UR4], RZ    {!1};
  // mbarrier.arrive_drop.shared.b64  arv_tkn, [shMem], 4; // explicit arrive of 4
      SYNCS.ARRIVE.TRANS64.OPTOUT.ART0  R2, [UR4], R2    {!2,+1.W,^2};
```
The `OPTOUT` option is added to regular arrive operations.


## Wait/Phase Check- `SYNCS.PHASECHK.TRANS64`


```
SYNCS.PHASECHK.TRANS64 PR,  [R    +UR      ], R
SYNCS.PHASECHK.TRANS64 DST, [SRC0.R+SRC0.UR], SRC1
```
DST = if the phase represented in SRC1 completed



`SYNCS.ARRIVE.TRANS64` -- `mbarrier.arrive.expect_tx.*` -- arrive 1; expect_tx += Src1
- `SYNCS.ARRIVE.TRANS64.OPTOUT` -- `mbarrier.arrive_drop.expect_tx.*`

```
  // mbarrier.test_wait.shared.b64    complete, [mbar], arv_tkn;
      SYNCS.PHASECHK.TRANS64  P0, [UR4], R3              {!2,+2.W,^2};
  // mbarrier.try_wait.shared.b64    complete, [mbar], arv_tkn;
      SYNCS.PHASECHK.TRANS64.TRYWAIT  P0, [UR4], R5      {!2,+1.W};
```

### Try_wait loop

Try-wait takes an addition time in nanoseconds to block.
```
waitLoopSusp:
             mbarrier.try_wait.shared.b64    complete, [mbar], arv_tkn, 0x1234
  @!complete bra         waitLoopSusp;
```
Generates.
```
      SYNCS.ARRIVE.TRANS64.OPTOUT.ART0  R2, [UR4], R2    {!2,+1.W,^2};
      .... R2-R3 come from above
.L_x_0:
  // mbarrier.try_wait.shared.b64    complete, [mbar], arv_tkn, 0x1234;
      MOV       R7,     UR4                              {!4,Y}; // unnecessary?
      SYNCS.PHASECHK.TRANS64.TRYWAIT  P0, [R7+URZ], R3   {!1,+1.W,+1.R};
      DEPBAR.LE  SB1,   0x0                              {!4,Y};
@!P0  NANOSLEEP.SYNCS  0x1234                            {!5};
@!P0  SYNCS.PHASECHK.TRANS64  P0, [R7+URZ], R3           {!2,+0.W};
@!P0  BRA       `(.L_x_0)                                {!5,^0};
```
