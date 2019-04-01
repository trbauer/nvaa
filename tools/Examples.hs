module Examples where

import Analysis
import Sample
import NVT.Bits
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Process
import Text.Printf
-- import qualified Data.Map.Strict as DM
import qualified Data.ByteString as S
import qualified Data.Map.Strict as DM


-- showLoadControls :: IO ()
-- showLoadControls =
--   twiddleField (75,2) (sString "000EA200001EE900`0000000004067381")
--
-- findLdPredLowBit = do
--  -- 87 seemed to twidding things
--  forM_ [87..100] $ \pix -> twiddleFieldsB [(82,2),(pix,1)]
--    (sString "000EA200001EE900`0000000004067381")

call_rel, call_abs :: IO [Sample]
call_rel = sString ("000FEA0003C00000`0000047000007944")
call_abs = sString ("000FEA0003C00000`0000000000007943")
ret_rel, ret_abs :: IO [Sample]
ret_rel = sString ("000FEA0003C3FFFF`FFFFF7300C007950")
ret_abs = sString ("000FEE0003E00000`0000000014007950") -- R20

-- want to infer the bit patterns of all opcodes
-- for each opcode X
--   load all samples (with bits)
--   find the longest common field starting from 0 where all samples share the same value
--

stg :: IO [Sample] -- STG.E.SYS [R12], R17 {!2,+1.R} ;       // examples/sm_75/samples\boxFilter_kernel.sass:13563
stg = sString ("0001E4000010E900`000000110C007386")

-- LDG.E.SYS R3, [UR38+0x14] {!1,+5.W} ;  // examples/sm_75/samples\cdpQuadtree.sass:7890
ldg_ur_off = sString "000F22000C1EE900`00001426FF037981" --

-- LDG with R
-- 000EA200001EE900`0000200018187381:        LDG.E.SYS R24, [R24+0x20] {!1,+3.W} ;  // examples/sm_75/samples\BezierLineCDP.sass:1625
-- 00016800001EE900`0000000008067381:        LDG.E.SYS R6, [R8] {!4,+6.W,+1.R} ;    // examples/sm_75/samples\bisect_large.sass:6743
-- LDG with UR
-- 000EA8000C1EE900`00000404FF047981:        LDG.E.SYS R4, [UR4+0x4] {!4,+3.W} ;    // examples/sm_75/samples\bisect_large.sass:5800
-- 000EE8000C1EE900`00000006FF057981:        LDG.E.SYS R5, [UR6] {!4,+4.W} ;        // examples/sm_75/samples\bisect_large.sass:5802
-- 000EA8000C1EE900`00000004FF027981:        LDG.E.SYS R2, [UR4] {!4,+3.W} ;        // examples/sm_75/samples\bitonic.sass:3258
-- 000EA8000C1EE900`00000404FF058981:  @!P0  LDG.E.SYS R5, [UR4+0x4] {!4,+3.W} ;    // examples/sm_75/samples\bitonic.sass:3260
-- 000EE8000C1EE900`00000006FF037981:        LDG.E.SYS R3, [UR6] {!4,+4.W} ;        // examples/sm_75/samples\bitonic.sass:3262
-- 000F22000C1EE900`00000406FF048981:  @!P0  LDG.E.SYS R4, [UR6+0x4] {!1,+5.W} ;    // examples/sm_75/samples\bitonic.sass:3264
--                          ^^ FF means RZ, so it doesn't show up as part of the equation

-- LDG.E.SYS R0, [R24+0x20] {!1,+3.W} ;   // examples/sm_75/samples\BezierLineCDP.sass:1600
ldg_v_off  = sString "000EA200001EE900`0000200018007381"
-- 000F6200001EE900`0000000004077381:        LDG.E.SYS R7, [R4] {!1,+6.W} ;         // examples/sm_75/samples\alignedTypes.sass:2896
-- 000EA8000C1EE900`00000004FF027981:        LDG.E.SYS R2, [UR4] {!4,+3.W} ;        // examples/sm_75/samples\bisect_large.sass:5798
--                  ........

-- 000FE40007FFE011`0000001210117210:        IADD3 R17, R16, R18, R17 {!2} ;        // examples/sm_75/samples\bisect_small.sass:1870
-- 000FE2000FFFE0FF`0000000402027C10:        IADD3 R2, R2, UR4, RZ {!1} ;           // examples/sm_75/samples\bitonic.sass:3248


-- Uniform register
-- 0002620000000000`0000001A00117309:        POPC R17, R26 {!1,+2.W,+2.R} ;         // examples/sm_75/samples\cdpAdvancedQuicksort.sass:4699
-- 000E240000000000`0000000400047309:        POPC R4, R4 {!2,+1.W} ;                // examples/sm_75/samples\cdpQuadtree.sass:7096
-- 000EA20000000000`0000003D0004D309:  @!P5  POPC R4, R61 {!1,+3.W} ;               // examples/sm_75/samples\particleSystem_cuda.sass:88390
-- 000EA20000000000`0000004A004A7309:        POPC R74, R74 {!1,+3.W} ;              // examples/sm_75/samples\particleSystem_cuda.sass:88748
-- 000E620000000000`0000000900048309:  @!P0  POPC R4, R9 {!1,+2.W} ;                // examples/sm_75/samples\particleSystem_cuda.sass:88937
-- 000E620000000000`000000790004D309:  @!P5  POPC R4, R121 {!1,+2.W} ;              // examples/sm_75/samples\particleSystem_cuda.sass:89956
--
-- 000E660008000000`0000000400077D09:        POPC R7, UR4 {!3,+2.W} ;               // examples/sm_75/samples\cdpAdvancedQuicksort.sass:4828
--           ......
-- 000E620008000000`0000000400087D09:        POPC R8, UR4 {!1,+2.W} ;               // examples/sm_75/samples\cdpAdvancedQuicksort.sass:4983
--
-- D09 vs 309
--

--
-- 000E640008201400`0000000400037D06:        I2F R3, UR4 {!2,+2.W} ;                // examples/sm_75/samples\boxFilter_kernel.sass:8696
-- 000E260000201400`0000001400147306:        I2F R20, R20 {!3,+1.W} ;               // examples/sm_75/samples\MonteCarlo_kernel.sass:31865
-- 000E240000201400`00000014000A7306:        I2F R10, R20 {!2,+1.W} ;               // examples/sm_75/samples\postProcessGL.sass:1469
-- 000E240000201400`00000013000B7306:        I2F R11, R19 {!2,+1.W} ;               // examples/sm_75/samples\postProcessGL.sass:1470
-- seems that D vs
--
-- 000FE2000FFFE0FF`000000050E0E7C10:        IADD3 R14, R14, UR5, RZ {!1} ;         // examples/sm_75/samples\Mandelbrot_cuda.sass:11118
-- 000FE4000FFFE0FF`0000000408087C10:        IADD3 R8, R8, UR4, RZ {!2} ;           // examples/sm_75/samples\Mandelbrot_cuda.sass:11868
-- 000FC8000FF3E0FF`0000000711117C10:        IADD3 R17, P1, R17, UR7, RZ {!4,Y} ;   // examples/sm_75/samples\recursiveGaussian_cuda.sass:7735
-- 000FE4000F8E00FF`00000006FF067E24:        IMAD.U32 R6, RZ, RZ, UR6 {!2} ;        // examples/sm_75/samples\cdpAdvancedQuicksort.sass:5454

-- 000FE2000fFFE0FF`0000001210117210
--           ......
--
fadd_rr = sString "000FE40000000000`0000000513057221" -- FADD R5, R19, R5 {!2} ;                // examples/sm_75/samples\bindlessTexture_kernel.sass:894
fadd_rc = sString "002FE40000000000`8000600005057621" -- FADD R5, R5, -c[0x0][0x180] {!2,^2} ;  // examples/sm_75/samples\bicubicTexture_cuda.sass:3098
fadd_ri = sString "000FE20000000000`3F0000000B0B7421" -- FADD R11, R11, 0.5 {!1} ;              // examples/sm_75/samples\bicubicTexture_cuda.sass:2553
--
ffma_rrr = sString "000FE4000000001F`0000001E1D1D7223" -- FFMA R29, R29, R30, R31 {!2} ;         // examples/sm_75/samples\bicubicTexture_cuda.sass:2350
-- yield flag kills the .reuse
-- ffma_rrr = sString "000FC80000000004`0000000903007223" -- FFMA R0, R3, R9, R4 {!4,Y} ;           // examples/sm_75/samples\BezierLineCDP.sass:2071
ffma_rrc = sString "000FC60000000006`0000600005057623" -- FFMA R5, R5, R6, c[0x0][0x180] {!3,Y} ; // examples/sm_75/samples\bicubicTexture_cuda.sass:2229
ffma_rri = sString "000FC40000000004`BF0000001E047423" -- FFMA R4, R30, R4, -0.5 {!2,Y} ;        // examples/sm_75/samples\bicubicTexture_cuda.sass:2309
ffma_rir = sString "000FC40000000009`3E2AAAAB04047823" -- FFMA R4, R4, 0.16666667163372039795, R9 {!2,Y} ; // examples/sm_75/samples\bicubicTexture_cuda.sass:2504
ffma_rcr = sString "001FCC0000000004`0000580005047A23" -- FFMA R4, R5, c[0x0][0x160], R4 {!6,Y,^1} ; // examples/sm_75/libs\cudnn64_7-cudnn64_7.1310.sm_75.sass:98825

-- imads = [ imad_x_rrrp, ]
imad_x_rrrp = sString "000FE200018E060D`000000FFFF0F7224" -- IMAD.X R15, RZ, RZ, R13, P3 {!1} ;     // examples/sm_75/libs\cublas64_100-Program.1023.sm_75.sass:18479
imad_rrr = sString "000FE200078E02FF`0000001C0A007224" -- IMAD R0, R10, R28, RZ {!1} ;           // examples/sm_75/libs\cublas64_100-Program.1016.sm_75.sass:22489
imad_rcr = sString "001FCA00078E0200`0000000016167A24" -- IMAD R22, R22, c[0x0][0x0], R0 {!5,Y,^1} ; // examples/sm_75/samples\BezierLineCDP.sass:1504
imad_rir = sString "000FE400078E020A`0000000409127824" -- IMAD R18, R9, 0x4, R10 {!2} ;          // examples/sm_75/samples\boxFilter_kernel.sass:12081
-- imad_rir = sString "000FC800078E0212`00000004090A7824" -- IMAD R10, R9, 0x4, R18 {!4,Y} ;        // examples/sm_75/samples\boxFilter_kernel.sass:12015
imad_iadd_rir = sString "000FCA00078E0A07`0000000105087824" -- IMAD.IADD R8, R5, 0x1, -R7 {!5,Y} ;    // examples/sm_75/samples\boxFilter_kernel.sass:12090
imad_mov_rrr = sString "000FE200078E0004`000000FFFF057224" -- IMAD.MOV.U32 R5, RZ, RZ, R4 {!1} ;     // examples/sm_75/samples\BezierLineCDP.sass:1554
imad_u32_rru = sString "000FD6000F8E00FF`00000004FF067E24" -- IMAD.U32 R6, RZ, RZ, UR4 {!11,Y} ;     // examples/sm_75/samples\boxFilter_kernel.sass:12378
imad_wide_u32_rcr = sString "040FE200078E0008`00005E00020A7A25" -- IMAD.WIDE.U32 R10, R2.reuse, c[0x0][0x178], R8 {!1} ; // examples/sm_75/libs\cublas64_100-Program.147.sm_75.sass:100774
imad_shl_u32_rir = sString "000FCA00078E00FF`0000000405067824" -- IMAD.SHL.U32 R6, R5, 0x4, RZ {!5,Y} ;  // examples/sm_75/libs\cublas64_100-Program.147.sm_75.sass:100757

imad_x1 = sString "000FCA00078E0200`0000000000007224" -- IMAD R0, R0, R0, R0  {!5,Y}
-- [11:9] 100b
imad_x2 = sString "000FCA00078E0200`0000000000007824" -- IMAD.MOV R0, R0, 0x0, -R0  {!5,Y}
imad_x3 = sString "000FCA00078E0A00`0000000105087824" -- IMAD.IADD R8, R5, 0x1, -R7 {!5,Y} ;



{-
0b000000001  000FEA0003C00000`0000000000007801  PMTRIG 0x0
0b000000010  000FEA0003C00000`0000000000007802  MOV R0, 0x0, 0x0
0b000000011  000FEA0003C00000`0000000000007803  P2R R0, PR, R0, 0x0
0b000000100  000FEA0003C00000`0000000000007804  R2P PR, R0, 0x0
0b000000101  000FEA0003C00000`0000000000007805  CS2R.32 R0, SR_LANEID
0b000000110  000FEA0003C00000`0000000000007806  VOTE.ALL R0, P0, PT
0b000000111  000FEA0003C00000`0000000000007807  SEL R0, R0, 0x0, PT
0b000001000  000FEA0003C00000`0000000000007808  FSEL R0, R0, 0, PT
0b000001001  000FEA0003C00000`0000000000007809  FMNMX R0, R0, 0, PT
0b000001010  000FEA0003C00000`000000000000780A  FSET.BF.F.AND R0, R0, 0, PT
0b000001011  000FEA0003C00000`000000000000780B  FSETP.F.AND P0, P4, R0, 0, PT
0b000001100  000FEA0003C00000`000000000000780C  ISETP.F.U32.AND P0, P4, R0, 0x0, PT, P0
0b000001101  000FEA0003C00000`000000000000780D  CSMTEST.INVALID0.F.AND P0, P4, 0x0, PT
..
0b000010000  000FEA0003C00000`0000000000007810  IADD3 R0, P0, P4, R0, 0x0, R0
0b000010001  000FEA0003C00000`0000000000007811  LEA R0, P0, R0, 0x0, 0x0
0b000010010  000FEA0003C00000`0000000000007812  LOP3.LUT P0, R0, R0, 0x0, R0, 0x0, PT
0b000010011  000FEA0003C00000`0000000000007813  IABS R0, 0x0
0b000010100  000FEA0003C00000`0000000000007814  VABSDIFF.U32 R0, P0, R0, 0x0, R0
0b000010101  000FEA0003C00000`0000000000007815  VABSDIFF4.U8 R0, P0, R0, 0x0, R0
0b000010110  000FEA0003C00000`0000000000007816  PRMT R0, R0, 0x0, R0
0b000010111  000FEA0003C00000`0000000000007817  IMNMX.U32 R0, R0, 0x0, PT
..
0b000011001  000FEA0003C00000`0000000000007819  SHF.L.S64 R0, R0, 0x0, R0
0b000011010  000FEA0003C00000`000000000000781A  SGXT.U32 R0, R0, 0x0
0b000011011  000FEA0003C00000`000000000000781B  BMSK R0, R0, 0x0
0b000011100  000FEA0003C00000`000000000000781C  PLOP3.LUT P0, P4, PT, P0, P0, 0x0, 0x0
..
0b000100000  000FEA0003C00000`0000000000007820  FMUL R0, R0, 0
..
0b000100010  000FEA0003C00000`0000000000007822  FSWZADD R0, R0, R0, PPPPPPPP
0b000100011  000FEA0003C00000`0000000000007823  FFMA R0, R0, 0, R0
0b000100100  000FEA0003C00000`0000000000007824  IMAD.MOV.U32 R0, R0, 0x0, R0
0b000100101  000FEA0003C00000`0000000000007825  IMAD.WIDE.U32 R0, P0, R0, 0x0, R0
..
0b000100111  000FEA0003C00000`0000000000007827  IMAD.HI.U32 R0, P0, R0, 0x0, R0
0b000101000  000FEA0003C00000`0000000000007828  DMUL R0, R0, 0
..
0b000101011  000FEA0003C00000`000000000000782B  DFMA R0, R0, 0, R0
..
0b000110001  000FEA0003C00000`0000000000007831  HFMA2 R0, R0, 0, 0, R0
0b000110010  000FEA0003C00000`0000000000007832  HMUL2 R0, R0, 0, 0
..
0b000111000  000FEA0003C00000`0000000000007838  I2I.U8.S32.SAT R0, 0x0
0b000111001  000FEA0003C00000`0000000000007839  I2IP.U8.S32.SAT R0, R0, 0x0, R0
..
0b000111011  000FEA0003C00000`000000000000783B  LDSM.16.M88 R0, [R0]
..
0b000111110  000FEA0003C00000`000000000000783E  F2FP.PACK_AB R0, R0, 0
..
0b010000010  000FEA0003C00000`0000000000007882  UMOV UR0, 0x0
..
0b010000110  000FEA0003C00000`0000000000007886  VOTEU.ALL UR0, UP0, PT
..
0b010011100  000FEA0003C00000`000000000000789C  UPLOP3.LUT UP0, UP4, UP7, UP0, UP0, 0x0, 0x0
..
0b100000000  000FEA0003C00000`0000000000007900  FLO.U32 R0, P0, 0x0
0b100000001  000FEA0003C00000`0000000000007901  BREV R0, 0x0
0b100000010  000FEA0003C00000`0000000000007902  FCHK P0, R0, 0
..
0b100001000  000FEA0003C00000`0000000000007908  MUFU.COS R0, 0
0b100001001  000FEA0003C00000`0000000000007909  POPC R0, 0x0
..
0b100011000  000FEA0003C00000`0000000000007918  NOP
0b100011001  000FEA0003C00000`0000000000007919  S2R R0, SR_LANEID
0b100011010  000FEA0003C00000`000000000000791A  DEPBAR
..
0b100011101  000FEA0003C00000`000000000000791D  BAR.SYNC 0x0, R0
...
0b100100000  000FEA0003C00000`0000000000007920  AL2P R0, R0, 0x0
..
0b100100011  000FEA0003C00000`0000000000007923  ISBERD R0, [R0]
..
0b100100101  000FEA0003C00000`0000000000007925  PIXLD R0, P0
0b100100111  000FEA0003C00000`0000000000007927  ISBEWR.???0 [R0], R0
..
0b101000001  000FEA0003C00000`0000000000007941  BSYNC B0
0b101000010  000FEA0003C00000`0000000000007942  BREAK B0
*0b101000011* 000FEA0003C00000`0000000000007943  CALL.ABS.NOINC 0x0
0b101000100  000FEA0003C00000`0000000000007944  CALL.REL.NOINC 0x10
0b101000101  000FEA0003C00000`0000000000007945  BSSY B0, 0x20
0b101000110  000FEA0003C00000`0000000000007946  YIELD
0b101000111  000FEA0003C00000`0000000000007947  BRA.DEC 0x40
0b101001000  000FEA0003C00000`0000000000007948  WARPSYNC 0x0
0b101001001  000FEA0003C00000`0000000000007949  BRX.DEC R0
0b101001010  000FEA0003C00000`000000000000794A  JMP.DEC 0x0
..
0b101001100  000FEA0003C00000`000000000000794C  JMX.DEC R0
0b101001101  000FEA0003C00000`000000000000794D  EXIT.NO_ATEXIT

0b101001111  000FEA0003C00000`000000000000794F  RTT
0b101010000  000FEA0003C00000`0000000000007950  RET.REL.NODEC R0 0x10
0b101010001  000FEA0003C00000`0000000000007951  IDE.EN 0x0
0b101010010  000FEA0003C00000`0000000000007952  RPCMOV.32 Rpc.LO, 0x0
...
0b101010100  000FEA0003C00000`0000000000007954  RPCMOV.64 Rpc, 0x0
...
0b101010110  000FEA0003C00000`0000000000007956  BMOV.32 B0, 0x0
0b101010111  000FEA0003C00000`0000000000007957  BMOV.64 ATEXIT_PC, 0x0
...
0b101011010  000FEA0003C00000`000000000000795A  NANOTRAP.RAND 0x0
0b101011011  000FEA0003C00000`000000000000795B  KILL
0b101011100  000FEA0003C00000`000000000000795C  BPT.INT
0b101011101  000FEA0003C00000`000000000000795D  NANOSLEEP.RAND 0x0
...
0b110000000  000FEA0003C00000`0000000000007980  LD.EU.U8.CONSTANT.CTA.ZD P0, R0, [R0]
...
0b110000011  000FEA0003C00000`0000000000007983  LDL.EU.U8 R0, [R0]
0b110000100  000FEA0003C00000`0000000000007984  LDS.U8.ZD P0, R0, [R0]
...
0b110001001  000FEA0003C00000`0000000000007989  SHFL.IDX P0, R0, R0, 0x0, R0
...
0b110001110  000FEA0003C00000`000000000000798E  RED.XOR.EU.INVALID0.CTA [R0], R0
...
0b110010010  000FEA0003C00000`0000000000007992  MEMBAR.SC.CTA
...
0b110011000  000FEA0003C00000`0000000000007998  SULD.P.1D.EU.INVALID0.CTA.INVALID0.IGN P0, R0, [R0], R0
...
0b110011010  000FEA0003C00000`000000000000799A  SULD.D.1D.EU.U8.INVALID0.CTA.IGN P0, R0, [R0], R0
...
0b110011100  000FEA0003C00000`000000000000799C  SUST.P.1D.EU.INVALID0.CTA.INVALID0.IGN [R0], R0, R0
...
0b110011110  000FEA0003C00000`000000000000799E  SUST.D.1D.EU.U8.INVALID0.CTA.IGN [R0], R0, R0
...
0b110101011  000FEA0003C00000`00000000000079AB  ERRBAR
...
0b111000011  000FEA0003C00000`00000000000079C3  S2UR UR0, SR_LANEID

-- OTHER GENERATED
0b110001111  0003ea0000800100`000000000000798f  CCTL.E.PF2 [R0]  {!5,+2.R} prefetch
0b110010000  0003EA0000800000`0000000000007990  CCTLL.PF2  [R0] {!5,+2.R}; prefetch
xxxxxxxxxxx  000FE20003A00000`000022220000795D NANOSLEEP.WARP   0x2222 {!1}; // __nanosleep()
-}

cctl_pf2 = sString "0003EA0000800100`000000000000798F" -- CCTL.E.PF2 [R0]  {!5,+2.R}
cctl_ivall = sString "000FEA0002000000`00000000FF00798F" -- CCTL.IVALL  {!5}


membar_sc_cta = sString "000FEA0000000000`0000000000007992" -- MEMBAR.SC.CTA  {!5} ;
membar_sc_gpu = sString "001FEA0000002000`0000000000007992" -- MEMBAR.SC.CTA  MEMBAR.SC.GPU  {!5,^1}
pmembar_gpu = sString "000FEA0000012000`0000000000005992" -- @P5   MEMBAR.GPU  {!5} ; // examples/sm_75/libs\cublas64_100-Program.1208.sm_75.sass:799
