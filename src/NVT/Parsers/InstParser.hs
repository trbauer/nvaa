module NVT.Parsers.InstParser where

import NVT.Parsers.Parser
import NVT.Bits
import NVT.Diagnostic
import NVT.Floats
import NVT.Loc
import NVT.IR
import NVT.Encoders.Codable

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Debug.Trace
import Text.Parsec((<|>),(<?>))
import Text.Printf
import qualified Text.Parsec           as P


type LabelIndex = [(String,PC)]
type Unresolved a = LabelIndex -> Either Diagnostic a
--
type LabelRefs = [(PC,String)]
type PIResult a = ParseResult LabelRefs a
   -- Either Diagnostic (a,LabelRefs,[Diagnostic])

parseResolvedInst ::
  PC ->
  FilePath ->
  Int ->
  String ->
  Either Diagnostic (Inst,[Diagnostic]) -- not a PIResult because we drop labels
parseResolvedInst pc fp lno syntax =
  case parseUnresolvedInst pc fp lno syntax of
    Left err -> Left err
    Right (ci,lrefs,ws) ->
      case ci [] of
        Left err -> Left err
        Right i -> Right (i,ws)


parseUnresolvedInst ::
  PC ->
  FilePath ->
  Int ->
  String ->
  PIResult (Unresolved Inst)
parseUnresolvedInst pc = runPI (pInst pc)




-------------------------------------------------------------------------------
sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
sequenceUnresolved as lbl_ix = sequence (map ($lbl_ix) as)
-- sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
-- sequenceUnresolved as lbl_ix = resolve as []
--   where resolve [] ras = reverse ras
--         resolve (ua:uas) ras =
--           case ua lbl_ix of
--             Left err -> Left err
--             Right a -> resolve uas (a:ras)

runPI ::
  PI a ->
  FilePath ->
  Int ->
  String ->
  PIResult a
runPI pa fp lno inp =
    case runPID p1 init_pst fp inp of
      Left err -> Left err
      Right (a,st,ws) -> Right (a,pisLabelReferences st,ws)
  where p1 = do
          sp <- P.getPosition
          P.setPosition (P.setSourceLine sp lno)
          pa

testPI :: Show a => PI a -> String -> IO ()
testPI = testPIF show
testPIU :: Show a => LabelIndex -> PI (Unresolved a) -> String -> IO ()
testPIU lix = testPIF fmt
  where fmt ua =
          case ua lix of
            Left err -> show err
            Right a -> show a
testPIF :: (a -> String) -> PI a -> String -> IO ()
testPIF fmt pa inp =
  case runPI (pa <* P.eof) "<interactive>" 1 inp of
    Left d -> putStrLn $ dFormatWithLines (lines inp) d

    Right (a,lbls,_) -> do
      mapM_ print lbls
      putStrLn (fmt a)



type PI a = PID PISt a

init_pst :: PISt
init_pst = PISt [] 0

data PISt =
  PISt {
    pisLabelReferences :: ![(PC,String)]
  , piSectionOffset :: !Int64
  } deriving Show

pSectionOffset :: PI Int64
pSectionOffset = piSectionOffset <$> pGet

-------------------------------------------------------------------------------
{-
parseUnresolvedInsts ::
  LabelIndex ->
  PC ->
  FilePath ->
  Int ->
  String ->
  PIResult (Unresolved [Inst])
parseUnresolvedInsts lbl_ix0 pc fp lno inp =
  runPI (pBlocks pc lbl_ix0) fp lno inp

testBlocks :: String -> IO ()
testBlocks inp = testPIF fmt (pBlocks 0 []) inp
  where fmt :: (Unresolved [Inst],LabelIndex) -> String
        fmt (uis,_) =
          case uis [] of
            Left err -> dFormat err
            Right is -> concatMap (\i -> show i ++ "\n") is

pBlocks ::
  PC ->
  LabelIndex ->
  PI (Unresolved [Inst],LabelIndex)
pBlocks pc0 li0 = seqResult <$> pInstOrLabel [] pc0 li0
  where seqResult (uis,li) = (sequenceUnresolved uis,li)

        pInstOrLabel :: [Unresolved Inst] -> PC -> LabelIndex -> PI ([Unresolved Inst],LabelIndex)
        pInstOrLabel ruis pc lbl_ix =
            P.try pLabel <|> pInstNoFail <|> pEof
          where pInstNoFail = do
                  ui <- P.try (pInst pc)
                  pInstOrLabel (ui:ruis) (pc+16) lbl_ix
                pLabel = pWithLoc $ \loc -> do
                  lbl <- pLabelChars
                  pSymbol ":"
                  case lbl`lookup`lbl_ix of
                    Just x ->
                      pSemanticError loc "label already defined"
                    Nothing -> do
                      pInstOrLabel ruis pc ((lbl,pc):lbl_ix)
                pEof = do
                  P.eof
                  return (reverse ruis,lbl_ix)
-}
box :: a -> [a]
box a = [a]

pInst :: PC -> PI (Unresolved Inst)
pInst pc = pWhiteSpace >> body
  where body = pWithLoc $ \loc -> do
          prd <- P.option PredNONE pPred
          op <- pOp
          (ios,m_lop3_opt) <- pInstOpts op
          --
          let pComplete :: [Dst] -> [Unresolved Src] -> PI (Unresolved Inst)
              pComplete dsts srcs =
                pCompleteInst loc prd op ios dsts srcs []

          let -- general variable operand (e.g. src1 in unary/binary ops)
              -- (note unary usually uses src1 instead of src0)
              pSrcX :: PI (Unresolved Src)
              pSrcX
                | oIsFP op = resolved <$> pSrcRCUF
                | otherwise = resolved <$> pSrcRCUI

              -- used to parse SRC1, SRC2 in ternary
              pSrc_RX_XR :: PI [Unresolved Src]
              pSrc_RX_XR = P.try (tryOrd pR pSrcX) <|> (tryOrd pSrcX pR)
                where pR = resolved <$> pSrcR

                      tryOrd p1 p2 = do
                        src1 <- p1
                        src2 <- pSymbol "," >> p2
                        return [src1,src2]

              -- UR, R, IMM
              pSrcURI :: PI (Unresolved Src)
              pSrcURI = P.try (resolved <$> pSrcUR) <|> pSrcI32OrL32 pc op

             -- e.g. NOP, ERRBAR, ...
              pNullaryOp :: PI (Unresolved Inst)
              pNullaryOp = pComplete [] []

              -- e.g. BFREV, F2F
              pUnaryOp :: PI (Unresolved Inst)
              pUnaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcX
                pComplete [dst] [src0]

              -- e.g. MOVM
              pUnaryOpR :: PI (Unresolved Inst)
              pUnaryOpR = do
                dst <- pDstR
                let pTrap = pWithLoc $ \loc -> do
                      pSrcX
                      pSemanticError loc "this op requires reg operand"
                src0 <- pSymbol "," >> (pSrcR <|> pTrap)
                pComplete [dst] [resolved src0]

              pUUnaryOp :: PI (Unresolved Inst)
              pUUnaryOp = do
                dst <- pDstUR
                src0 <- pSymbol "," >> pSrcURI
                pComplete [dst] [src0]

              pS2XROp :: PI Dst -> PI (Unresolved Inst)
              pS2XROp pDst = do
                dst <- pDst
                src <- pSymbol "," >> pSrcSR
                pComplete [dst] [resolved src]

              -- CS2R and S2R
              pSR2RFamily :: PI (Unresolved Inst)
              pSR2RFamily = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcSR
                pComplete [dst] [resolved src0]

              --  IMNMX   R3, R0, 0x4, !PT {!4,Y,^1};
              pSelectOp :: PI (Unresolved Inst)
              pSelectOp = do
                dst <- pDst
                srcs <- pSymbol "," >> pSrc_RX_XR
                src2_pr <- pSymbol "," >> resolved <$> pSrcP
                pComplete [dst] (srcs ++ [src2_pr])

              -- a simple binary op without fancy stuff
              --   e.g. BMSK
              pBinaryOp :: PI (Unresolved Inst)
              pBinaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcX
                pComplete [dst] [resolved src0,src1]

              -- a simple binary op without fancy stuff
              --   e.g. BMSK    R22, R19,  R22
              pBinaryOpRR :: PI (Unresolved Inst)
              pBinaryOpRR = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcR
                pComplete [dst] (resolveds [src0,src1])

              -- HADD2, HMUL2
              pBinaryOpH2 :: PI (Unresolved Inst)
              pBinaryOpH2 = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcRH2
                src1s <- pSymbol "," >> pSrcXH2s
                pComplete [dst] (resolveds $ src0:src1s)

              -- simple ternary ops (no predicates/carry out/carry)
              pTernaryOp :: PI (Unresolved Inst)
              pTernaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src12 <- pSymbol "," >> pSrc_RX_XR
                pComplete [dst] (resolved src0:src12)

              pTernaryOpH2 :: PI (Unresolved Inst)
              pTernaryOpH2 = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcRH2
                let pXR :: PI [Unresolved Src]
                    pXR = do
                      src1s <- pSymbol "," >> pSrcXH2s
                      src2 <- pSymbol "," >> pSrcRH2
                      return $ resolveds $ src1s ++ [src2]
                    pRX :: PI [Unresolved Src]
                    pRX = do
                      src1 <- pSymbol "," >> pSrcRH2
                      src2s <- pSymbol "," >> pSrcXH2s
                      return $ resolveds (src1:src2s)
                src12s <- P.try pXR <|> pRX
                pComplete [dst] (resolved src0:src12s)

              -- e.g. HMMA/DMMA uses all R
              pTernaryOpRRR :: PI (Unresolved Inst)
              pTernaryOpRRR = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcR
                src2 <- pSymbol "," >> pSrcR
                pComplete [dst] (resolveds [src0,src1,src2])

              -- e.g. DSETP/FSETP
              pSETP :: PI (Unresolved Inst)
              pSETP = do
                dst0 <- pDstP
                dst1 <- pSymbol "," >> pDstP
                src0 <- pSymbol "," >> resolved <$> pSrcR
                src1 <- pSymbol "," >> pSrcX
                src2 <- pSymbol "," >> resolved <$> pSrcP
                pComplete [dst0,dst1] [src0,src1,src2]

              pLDX :: PI (Unresolved Inst)
              pLDX = pLdOp  loc prd op ios
              pSTX :: PI (Unresolved Inst)
              pSTX = pStOp loc prd op ios
              pATX :: PI (Unresolved Inst)
              pATX = pAtOp loc prd op ios

              -- TEX, TLD
              --
              -- starts just before the comma preceding
              -- the soup of trailing parameters
              -- TEX.SCR.B.LL     R4,  R6,  R12, R4,             2D {!1,Y,+6.W,^6};
              --                                   ^
              -- TEX.SCR.LL       RZ,  R2,  R2,  R4,  0x0, 0x5e, ARRAY_2D, 0x1 {!1,Y,+6.W};
              --                                   ^
              -- TLD.SCR.LZ  RZ, R8, R18, 0x0, 0x64, 1D, 0x3 {!3,Y};
              --                        ^
              pTextureOp :: Dst -> [Src] -> PI (Unresolved Inst)
              pTextureOp dst srcs = do
                -- maybe a pair of imm
                pSymbol ","
                let pNoImms = do
                      src4 <- SrcTex <$> pSyntax
                      return ([sRC_I32_0, sRC_I32_0],src4)
                    pImmsFirst = do
                      src3a <- pSrcI32 op
                      src3b <- pSymbol "," >> pSrcI32 op
                      src4 <- pSymbol "," >> SrcTex <$> pSyntax
                      return ([src3a,src3b],src4)
                (src3s,src4) <- P.try pNoImms <|> pImmsFirst
                src5_opt <- P.option [] $ pSymbol "," >> box <$> pSrcI32 op
                pComplete [dst] (resolveds $ srcs ++ src3s ++ [src4] ++ src5_opt)

          --
          case op of
            OpARRIVES -> do
              -- ARRIVES.LDGSTSBAR.64 [URZ+0x800]
              -- disassembler permits R#
              (r_addr,ur_off,i_off) <- pLDST_Addrs op
              pComplete [] (resolveds [r_addr,ur_off,i_off])

            ld_st_op
              | oIsAT op -> pATX

            --                  DST   SRC0
            -- B2R.RESULT       R16        {!12,Y,+2.W};
            -- B2R.RESULT       R36        {!12,Y,+2.W};
            -- B2R.RESULT       RZ,   P1   {!2,+2.W};
            -- B2R              RZ,   0xf
            --
            -- [83:81] sets predicate (no sign)
            -- [79:78]
            --    0 means: B2R        RZ, 0xF  (imm is [57:54])
            --    1 means: B2R.RESULT RZ, P1  (pr is [83:81])
            --    2 means: B2R.WARP   RZ
            OpB2R -> do
              dst <- pDstR -- [23:16]
              src0 <- P.option sRC_PT $ pSymbol "," >> (pSrcP <|> pSrcI8)
              pComplete [dst] [resolved src0]

            --                                   [57:54]  [53:42]  [90:87]
            --       BAR.SYNC                     0x0      [0x0]        {!6};
            -- @P1   BAR.SYNC.DEFER_BLOCKING      0x0                   {!6};
            --       BAR.RED.POPC                 0x0,              P1  {!6};
            --       BAR.RED.AND.DEFER_BLOCKING   0x0,              P0  {!6};
            --       BAR.ARV                      0x0,      0x0         {!6}
            --       BAR.SYNCALL.DEFER_BLOCKING                         {!6}
            --       BAR.RED.AND.DEFER_BLOCKING   0x0,      0x1,    P0  {!6}
            OpBAR -> do
              let pIIX = do
                    src0 <- pSrcInt32
                    src1 <- pSymbol "," >> pSrcInt32
                    src2 <- P.option sRC_PT $ pSymbol "," >> pSrcP
                    return [src0,src1,src2]
                  pIX = do
                    src0 <- pSrcInt32
                    src2 <- P.option sRC_PT $ pSymbol "," >> pSrcP
                    return [src0,sRC_I32_0,src2]
              srcs <- P.option [sRC_I32_0,sRC_I32_0,sRC_PT] $ P.try pIIX <|> pIX
              pComplete [] (resolveds srcs)

            -- Move Convergence Barrier State
            --                  DST  SRC0   SRC1
            -- BMOV.32          0's  B6,     R24   {!3,+3.R};
            -- BMOV.32.CLEAR    R22, B6      0's   {!3,+4.W,^4};
            -- BMOV.32.PQUAD    0's  B6,     R2 -- bit [84] enables PQUAD
            OpBMOV -> do
              dsts <- P.option [] $ box <$> pDstR <* pSymbol "," -- [23:16]
              src0 <- pSrcB -- [29:24]
              maybe_src1 <- P.option [] $ box <$> (pSymbol "," >> pSrcR) -- [39:32]
              pComplete dsts $ resolveds (src0:maybe_src1)

            --
            --               DST  SRC0  SRC1
            -- @P0   BMSK    R22, R19,  R22 {!2};
            --       BMSK    R5,  R4,   0x1 {!2};
            --       BMSK.W  R88, RZ,   R86   ....
            --
            -- [75] is .W
            OpBMSK -> pBinaryOp

            --
            -- BREV   R28, R28 {!2,+1.W};
            -- BREV   R26, 0x1a {!1,+3.W}
            OpBREV -> pUnaryOp

            -- BPT.TRAP         0x1 {!5,^1};
            OpBPT -> do
              src0 <- pSrcI32 op
              pComplete [] [resolved src0]

            -- @!P0  BRA            `(.L_1) {!5};
            -- @!P1  BRA      !P2,  `(.L_18) {!5};
            --       BRA.DIV  ~URZ, `(.L_989) {!6};
            -- @!P4  BRA.INC         0x210 {!12,Y}
            --
            -- label [81:64]:[63:32]
            -- also
            --   [90:87] = the predicate source
            --   [86:85] = {[default], .INC, .DEC, .INVALID3}
            --
            OpBRA -> do
              maybe_src_pr <- P.option [] $ box <$> (pSrcP <* pSymbol ",")
              src0 <- pSrcLbl pc op
              pComplete [] (resolveds maybe_src_pr ++ [src0])

            -- @!P1  BREAK            !P2, B1 {!1};
            -- src predicate is [90:87] and PT 0b0111 is the default
            OpBREAK -> do
              src_pr <- P.option sRC_PT $ (pSrcP <* pSymbol ",")
              src0 <- pSrcB
              pComplete [] (resolveds [src_pr, src0])

            -- BRX      R8  -0x1a50 {!5};
            -- BRX !P6, R12 -0xb50 {!5};
            OpBRX -> do
              src_pr <- P.option sRC_PT $ (pSrcP <* pSymbol ",")
              src0 <- pSrcR
              -- no comma
              src1 <- pSrcI32 op
              pComplete [] (resolveds [src_pr, src0, src1])

            -- BRXU         UR4 -0x1840 {!5,Y};
            -- BRXU     P6, UR4 -0x1840 {!5,Y};
            -- []
            OpBRXU -> do
              src_pr <- P.option sRC_PT $ (pSrcP <* pSymbol ",")
              src0 <- pSrcUR
              -- no comma
              src1 <- pSrcI32 op
              pComplete [] (resolveds [src_pr,src0, src1])

            -- BSSY             B0, `(.L_31) {!12,Y};
            OpBSSY -> do
              src0 <- pSrcB
              pSymbol ","
              src1 <- pSrcLbl pc op
              pComplete [] (resolved src0:[src1])

            -- BSYNC      B1 {!5}; // PT implied
            -- BSYNC !P5, B2 {!5};
            OpBSYNC -> do
              src0 <- P.option sRC_PT $ P.try $ pSrcP <* pSymbol ","
              src1 <- pSrcB
              pComplete [] $ resolveds [src0,src1]

            -- CALL.ABS.NOINC   `(cudaLaunchDeviceV2) {!5,Y,^1};
            OpCALL -> do
              src0 <- pSrc pc op -- I think this can be register too
              pComplete [] [src0]

            -- CCTL.IVALL        {!5,Y};
            OpCCTL -> pNullaryOp

            --      CS2R     R6, SRZ {!1};
            --      CS2R.32  R5, SR_CLOCKLO {!7,Y,^2};
            --      CS2R     R4, SR_CLOCKLO {!7,Y};
            -- @P0  CS2R     R30, SRZ {!2};
            OpCS2R -> pSR2RFamily

            --
            --       DADD             R20,  R8,   c[0x3][0x8] {!2,+2.W,+1.R,^1};
            --       DADD             R14, -RZ, -R6 {!6,Y,+1.W,^1};
            -- @P1   DADD             R20, -RZ,  -R14 {!10,Y,+1.W,^3};
            OpDADD -> pBinaryOp

            --   DEPBAR.LE  SB0, 0x0 {!4,Y}; (bit [44] is sb, [43:38] is imm val)
            --   DEPBAR.LE  SB0, 0x7, {5,4,3,2,1,0}
            --   DEPBAR (unsupported)
            --
            -- [37:32] = activates the bit set
            OpDEPBAR -> do
              src0 <- pSrcSB <* pSymbol ","
              src1 <- pSrcImmNonBranch32 op
              src2 <-
                P.option sRC_I32_0 $ do
                  pSymbol ","
                  let pBitIx = pInt <* pWhiteSpace
                  pSymbol "{"
                  b0 <- pBitIx
                  bs <- P.many $ pSymbol "," >> pBitIx
                  pSymbol "}"
                  return (SrcI32 (foldl' setBit 0 (b0:bs)))
              pComplete [] (resolveds [src0, src1, src2])

            -- DFMA        R8,   R36, R36, R8  {!6,Y,+1.W,^1};
            -- DFMA        R10,  R6,  R10, 0.5 {!4,Y,^1};
            OpDFMA -> pTernaryOp

            -- DMMA.884     R12,   R78,        R84,         R12 {!2,+3.R,^3};
            -- DMMA.884     R8,    R76,        R84.reuse,   R8 {!10,+4.W,^3};
            -- DMMA.884     R16, -|R70|.reuse, R86,       -|R16|
            OpDMMA -> pTernaryOpRRR

            -- DMUL        R6,   R10, R10.reuse    {!1,+1.W};
            -- DMUL        R6,   R40, c[0x2][0xc0] {!4,Y,+1.W,^6};
            -- DMUL        R38,  R2,  0.5          {!4,Y};
            OpDMUL -> pBinaryOp

            --                       DST0  DST1  SRC0   SRC1           SRC2
            --  @P0   DSETP.MAX.AND  P1,   P2,   R20,   R22,           PT {!2,+1.W,^1};
            --        DSETP.MAX.AND  P0,   P1,   R4,    c[0x0][0x170], PT {!2,+2.W,^2};
            --  @!P1  DSETP.NEU.AND  P0,   PT,   |R2|, +INF,           PT {!6,Y,+1.W,^1};
            --  @!P1  DSETP.NEU.AND  P0,   PT,   |R2|, |UR0|,          PT
            -- DST0 = [83:81] ([23:16] is 0's)
            -- DST1 = [86:84]
            -- SRC2 = [90:87]
            OpDSETP -> pSETP

            -- ERRBAR            {Y,^1};
            OpERRBAR -> pNullaryOp

            -- @!P0  EXIT              {!5};
            OpEXIT -> pNullaryOp

            -- F2F.F32.F64    R13, |R2| {!12,Y,+3.W,+2.R,^2};
            -- F2F.F64.F32    R14, R14 {!2,+1.W};   // examples/sm_80/samples/conjugateGradientMultiBlockCG.sass:3004
            -- F2F.F64.F32    R12, c[0x0][0x178] {!6,+1.W,^1};
            OpF2F -> pUnaryOp

            -- @P1   F2FP.BF16.PACK_AB R6, RZ, R6 {!2};
            -- (also the others)
            OpF2FP -> pBinaryOp

            -- F2I.FTZ.U32.TRUNC.NTZ R3, R2 {!2,+3.W,+2.R};
            -- F2I                   R13, R13 {!1,+5.W};
            --
            -- (in syntax option order)
            --  [76:75]  {.S8, .S16, [.S32], ..}
            --  [79:78]  {[default], .FLOOR, .CEIL, .TRUNC}
            --  [80]     {[default], .FTZ}
            --  [77]     {[default], .NTZ}
            OpF2I -> pUnaryOp

            -- @P0   FADD.FTZ         R4,  R0,       1 {!1};
            --       FADD.FTZ         R3, -R5,       -RZ {!4,Y};
            --       FADD             R4,  R0,       -c[0x0][0x17c] {!2,Y,^1};
            --       FADD             R3,  R0.reuse, R9.reuse {!1};
            --       FADD.FTZ         R5, -RZ,       -UR4 {!4,Y};
            OpFADD -> pBinaryOp

            --                 DST  SRC0  SRC1
            -- FCHK             P0, R5,    R0            {!2,+1.W};
            -- FCHK             P0, R5,    c[0x0][0x180] {!2,+1.W};
            -- FCHK             P1, R27,   R28           {!2,+1.W};
            -- dst is [83:81]
            OpFCHK -> do
              dst <- pDstP
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              pComplete [dst] (resolved src0:[src1])

            -- @!P1  FFMA         R39, R39, R10, R2 {!2};
            --       FFMA         R10, -R3.reuse, R5, R6 {!2};
            -- ... many more
            OpFFMA -> pTernaryOp

            -- FLO.U32          R19, UR4 {!1,+2.W};
            -- FLO.U32          R3,  R2 {!2,+2.W,+1.R,^1};
            -- FLO.U32.SH       R44, R44 {!2, +2.W, ^2}
            OpFLO -> pUnaryOp

            -- FMNMX  R13, R13, R14, !PT {!5,Y,^3,^4};
            OpFMNMX -> pSelectOp

            --       FMUL.FTZ         R5, R4, R3 {!2,^1};   // examples/sm_80/samples/BezierLineCDP.sass:1573
            -- @P0   FMUL.FTZ         R7, R4, 0.5 {!2};
            --       FMUL             R21, R16, c[0x0][0x190] {!2,^3}
            OpFMUL -> pBinaryOp

            -- FRND.FLOOR       R4,  R3 {!2,+1.W};
            -- FRND.F64         R16, R14 {!2,+3.W,+1.R};
            OpFRND -> pUnaryOp

            -- @!P1  FSEL             R4, R6, c[0x0][0x180], !P0 {!2,^2}; // examples/sm_80/samples/dmmaTensorCoreGemm.sass:7377
            OpFSEL -> pSelectOp

            -- FSET.BF.GE.AND   R7, R6, c[0x0][0x1b8], PT {!1,^3}; // examples/sm_80/libs/nppif64_11/Program.329.sm_80.sass:7325
            OpFSET -> pSelectOp

            -- FSETP.EQ.OR      P0, PT, |R7|, RZ, !P0 {!2};
            --   [75:74] = {.AND,.OR,.XOR,.INVALID3}
            --   [79:76] = {.F,.LT,.EQ,.LE,..,.GEU,.T}
            --   [80] = {[default],.FTZ}
            OpFSETP -> pSETP

            -- HADD2.F32        R4, -RZ.H0_H0, R5.reuse.H1_H1 {!2,^1};
            OpHADD2 -> pBinaryOpH2

            -- HFMA2            R25, R35.reuse.H0_H0, R2.reuse, R25 {!2,^2};
            OpHFMA2 -> pTernaryOpH2

            -- HMMA.16816.F32.BF16 R12, R4, R8, R12 {!7,+6.W,+6.R,^3}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7223
            -- HMMA.1688.F32.TF32 R52, R96.reuse, R2, R52 {!8,+3.W,^2};
            --
            -- HMMA.<SHAPE>.DT.ST
            -- [76] flips  DT (dest type) between F16 and F32
            -- [78] selects {.1688 vs .1684} there's also HMMA.16816 (different type)
            -- [83:82] {[.F16?], .BF16, .TF32, .INVALID3}
            OpHMMA -> pTernaryOpRRR

            -- @!P0 HMUL2   R10, R34.H0_H0, R10.H0_H0 {!2,Y};
            -- @!P0 HMUL2   R10, R34.H0_H0, 0.0078125, 5.9604644775390625e-07
            -- @!P0 HMUL2   R10, R34.H0_H0, c[0x0] [0x0].H0_H0
            OpHMUL2 -> pBinaryOpH2

            --  HSET2.BF.EQ.AND   R0,  R40, RZ.H0_H0, PT {!5,Y,^3};
            OpHSET2 -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcRH2
              src1s <- pSymbol "," >> pSrcXH2s
              src_p <- pSymbol "," >> pSrcP
              pComplete [dst] (resolveds $ src0:src1s ++ [src_p])

            -- I think:         DST  DST  DST        SRC0        [SRC1]     SRCPR
            --  HSETP2.NE.AND    P1,  PT,  R64,       RZ,                      PT {!2};
            --  HSETP2.NEU.AND   P0,  PT,  R40.H0_H0, RZ.H0_H0,                PT {!1,^6};
            --  HSETP2.GEU.AND   P4,  PT,  R30.H0_H0, 8.5...e-06, 8.52...e-06, PT {!1};
            -- I believe the last is pairs of fp16 values
            OpHSETP2 -> do
              dst_p_lo <- pDstP
              dst_p_hi <- pSymbol "," >> pDstP
              dst_r <- pSymbol "," >> pDstRH2
              let dsts = [dst_p_lo,dst_p_hi,dst_r]
              --
              src0 <- pSymbol "," >> pSrcRH2
              src1s <- pSymbol "," >> pSrcXH2s
              src_p <- pSymbol "," >> pSrcP
              --
              pComplete dsts (resolveds ([src0] ++ src1s ++ [src_p]))

            -- I2F           R4,  R16 {!1,+2.W,+1.R}; // examples/sm_80/samples/BezierLineCDP.sass:1739
            -- I2F.U32.RP    R4,  0x20 {!1,+1.W};   // examples/sm_80/samples/bf16TensorCoreGemm.sass:7074
            -- I2F           R14, c[0x0][0x178] {!1,+1.W};
            -- I2F.U8        R17, R6.B3 {!1,+5.W}; // examples/sm_80/samples/boxFilter_kernel.sass:8973
            -- I2F           R5,  UR4 {!2,+2.W};
            --
            -- [79:78] rounding {[.RN?],.RM,.RP,.RZ}
            -- [85:84] source type {.S8,.S16,[.S32],INVALID}
            OpI2F -> do
              dst <- pDstR
              let byte_selectors = [ModB1,ModB2,ModB3] -- only if .U8
              let pSrcCCX_WithSfx = do
                    SrcCon ms s o <- pSrcCCX
                    ms_sfx <- pAddModRegSuffixesFrom byte_selectors
                    return (SrcCon (ms`msUnion`ms_sfx) s o)
              pSymbol ","
              src <-
                pSrcR_WithModSuffixes byte_selectors <|>
                  pSrcUR_WithModSuffixes byte_selectors <|>
                  pSrcCCX_WithSfx <|>
                  pSrcInt32
              pComplete [dst] [resolved src]


            -- I2I.U16.S32.SAT  R10, R10 {!2};        // examples/sm_80/libs/nppial64_11/Program.28.sm_80.sass:142955
            -- I2I.U16.S32.SAT R10, 0xa {!2}
            --
            -- [77:76] = {.U8,.S8,.U16,.S16} (first option)
            OpI2I -> pUnaryOp

            -- I2IP.S8.S32.SAT  R4, R5, R4, RZ {!2,^5}; // examples/sm_80/libs/cusolver64_10/Program.2588.sm_80.sass:497642
            OpI2IP -> pTernaryOp

            --       IABS   R11, R26 {!1};        // examples/sm_80/samples/cdpQuadtree.sass:7402
            -- @P6   IABS   R0,  R5 {!1};          // examples/sm_80/libs/nppist64_11/Program.112.sm_80.sass:42042
            OpIABS -> pUnaryOp

            --
            -- IADD3      R2,         R4, 0xffffffe,     RZ          {!2,Y,^1};
            -- IADD3.X    R5,         R5, c[0x0][0x164], RZ, P0, !PT {!4,Y};
            -- IADD3      R4, P3, P6, R4, R9,            RZ          {!5,Y}
            --
            -- [74] is .X
            -- [75] ~ on src2
            -- [80:77] carry-in2
            -- [83:81] carry-out1
            -- [86:84] carry-out2
            -- [90:87] carry-in1
            OpIADD3 -> do
              dst <- pDst
              (dst_co1,dst_co2) <-
                P.option (dST_PT,dST_PT) $ P.try $ do
                  p0 <- pSymbol "," >> pDstP
                  p1 <- P.option dST_PT (P.try $ pSymbol "," >> pDstP)
                  return (p0,p1)
              src0 <- pSymbol "," >> pSrcR
              src12s <- pSymbol "," >> pSrc_RX_XR
              (src_ci1,src_ci2) <-
                P.option (sRC_NPT,sRC_NPT) $ do
                  ci1 <- pSymbol "," >> pSrcP
                  ci2 <- pSymbol "," >> pSrcP
                  return (ci1,ci2)
              pComplete
                [dst,dst_co1,dst_co2]
                ([resolved src0] ++ src12s ++ resolveds [src_ci1,src_ci2])

            -- IDP.4A.U8.S8 R69, R37, c[0x2][0x0], R40 {!1}
            --
            -- [77:76] selects between .4A (00b) and .2A.LO (01b) and .2A.HI (11b)
            -- [74:73] are
            --  (.4A)
            --    0x00 -> 4A.U8.U8
            --    0x01 -> 4A.S8.U8
            --    0x02 -> 4A.U8.S8
            --    0x03 -> 4A.S8.S8
            --  (.2A.{LO,HI})
            --    0x00 -> 2A.*.U16.U8
            --    0x01 -> 2A.*.S16.U8
            --    0x02 -> 2A.*.U16.S8
            --    0x03 -> 2A.*.S16.S8
            OpIDP -> pTernaryOp

            -- IMAD.HI          R14,     R15,  0x55555556, RZ {!1};
            -- IMAD             R8,      R11,  0x8, R8 {!1};
            -- IMAD.X           R71,     R135, 0x1, R142, P1 {!1}; // examples/sm_80/libs/cusolver64_10/Program.2624.sm_80.sass:33630
            -- IMAD.HI.U32      R39, P1, R37,  R39, R40 {!7,Y}; // examples/sm_80/libs/cufft64_10/Program.126.sm_80.sass:12267
            -- IMAD.WIDE.U32    R4,  P0, R2,   R9,  R4 {!4,Y}; // examples/sm_80/libs/cufft64_10/Program.126.sm_80.sass:12858
            --
            -- [90:87] is optional carry-in predicate
            --    only present in .X
            -- [83:81] is carry out predicate (.HI.U32)
            --    only present on .HI (opcode 0x027) or .WIDE (0x025)
            -- [75] ~ on src2 (or floating src)
            -- [74] is .X
            -- [73] is .U32
            -- IMAD.HI is a different opcode (0x027, 0x025, 0x024)
            OpIMAD -> do
              dst <- pDstR
              dst_co <- P.option dST_PT $ P.try $ pSymbol "," >> pDstP
              --
              src0 <- pSymbol "," >> pSrcR
              src12s <- pSymbol "," >> pSrc_RX_XR
              src3_ci <- P.option sRC_NPT $ P.try $ pSymbol "," >> pSrcP
              --
              pComplete [dst,dst_co] $
                resolved src0:src12s ++ [resolved src3_ci]

            -- IMMA.16816.U8.U8 R8, R2.reuse.ROW, R26.COL, R8 {!4,+2.W,+1.R,^3};
            OpIMMA -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcR_MMA
              src1 <- pSymbol "," >> pSrcR_MMA
              src2 <- pSymbol "," >> pSrcR
              pComplete [dst] $ resolveds [src0,src1,src2]

            --       IMNMX       R2,  R2, c[0x0][0x174], !PT {!4,Y}; // examples/sm_80/samples/boxFilter_kernel.sass:10553
            -- @P0   IMNMX.U32   R12, R8, R13, PT {!2,^3};
            --       IMNMX       R9,  R9, 0x20, !PT {!5,Y}; // examples/sm_80/samples/cdpQuadtree.sass:7464
            OpIMNMX -> pSelectOp

            -- ISETP.GT.U32.AND P0, PT, R6, 0xfd, PT {!4,Y};
            -- ISETP.GE.AND.EX  P0, PT, R7, c[0x0][0x16c], PT, P0 {!1};
            -- ISETP.LT.AND     P0, PT, R13, UR5, PT {!1}; // examples/sm_80/samples/binomialOptions_kernel.sass:2656
            --
            -- [71:68] = second predicate src (only on .EX)
            -- [72] = .EX (extended)
            -- [73] = .U32
            -- [75:74] = {AND,OR,XOR,INVALID3}
            -- [78:76] = {F,LT,EQ,LE,GT,NE,GE,T}
            -- [90:87] = first predicate src
            OpISETP -> do
              dst0 <- pDstP
              dst1 <- pSymbol "," >> pDstP
              src0 <- pSymbol "," >> resolved <$> pSrcR
              src1 <- pSymbol "," >> pSrcX
              src2 <- pSymbol "," >> resolved <$> pSrcP
              -- .EX has an extra predicate
              src3 <- P.option sRC_PT $ pSymbol "," >> pSrcP -- 0b0111 (PT) if not set
              pComplete [dst0,dst1] [src0,src1,src2,resolved src3]

            --       LD.E        R4,   [R10.64] {!4,+3.W}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7204
            -- @!P2  LD.E.128    R136, [R172+0x80] {!2,+6.W,+2.R}; // examples/sm_80/libs/cusolver64_10/Program.2641.sm_80.sass:181621
            -- @!P2  LD.E        R0,   [R136.64+UR10+0x80] {!1,+5.W}; // examples/sm_80/libs/cusolver64_10/Program.2626.sm_80.sass:294524
            --
            -- [75:73] = {.U8, .S8, .U16, .S16, [.32], .64, .128, INVALID}
            OpLD -> pLDX

            --
            --   LDC              R5, c[0x3][R3+0x8] {!1,+2.W};
            --   LDC.U16          R3, c[0x0][0x16e] {!1,+2.W}; // examples/sm_80/samples/FunctionPointers_kernels.sass:2954
            --
            -- IR and encoding treat this as:
            --   LDC              R5, R3, c[0x3][0x8] {!1,+2.W};
            --
            -- [75:73] = {U8, S8, U16, S16, [.32], .64, INVALID6, INVALID7}
            OpLDC -> do
              dst <- pDstR <* pSymbol ","
              srcs <- pXLdcSrcs pSrcR SrcRZ
              pComplete [dst] (resolveds srcs)

            -- LDG.E       R7,   [R4.64] {!4,+3.W}; // examples/sm_80/samples/alignedTypes.sass:3280
            -- LDG.E.128   R128, [R114.64+0x50000] {!4,+4.W,^1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:10134
            OpLDG -> pLDX

            -- LDGDEPBAR         {!4,+1.W};           // examples/sm_80/samples/bf16TensorCoreGemm.sass:8061
            OpLDGDEPBAR -> pNullaryOp

            --      LDGSTS.E.BYPASS.128.ZFILL            [R42],         [R80.64] {!1,+5.R};
            --      LDGSTS.E.BYPASS.128.ZFILL            [R35+0x1200],  [R22.64] {!4,+3.R};
            --      LDGSTS.E.BYPASS.LTC128B.128.CONSTANT [R129+0x2000], [R130.64+0x80], P4 {!6,+6.R};
            --   (000BEC000A108D7F`0200008082817FAE)
            --                      ^^^^%%%SSDD....
            -- @P3  LDGSTS.E.BYPASS.128                  [R231+0x880], [R198.64+UR12+0x80] {!1,+5.R}; // examples/sm_80/libs/cusolver64_10/Program.2625.sm_80.sass:88682
            --
            -- [90:87] is the optional src predicate
            OpLDGSTS -> do
              pSymbol "["
              src0 <- pSrcR_WithModSuffixes [] -- I don't think a scale is permitted here
              imm <- P.option 0 $ fromIntegral <$> pIntTerm
              pSymbol "]"
              pSymbol ","
              (r_addr,ur_off,i_off) <- pLDST_Addrs op
              src_p <- P.option sRC_PT $ pSymbol "," >> pSrcP
              pComplete [] (resolveds [src0,SrcI32 imm,r_addr,ur_off,i_off,src_p])

            -- @P0   LDL.U8           R15, [R4+0x6] {!4,+5.W}; // examples/sm_80/libs/nvjpeg64_11/Program.36.sm_80.sass:39875
            --       LDL.LU           R38, [R1+0x14] {!4,+6.W,+1.R}; // examples/sm_80/libs/cufft64_10/Program.6.sm_80.sass:3108769
            OpLDL -> pLDX

            --       LDS              R4,  [R38.X4+UR4] {!4,+1.W}; // examples/sm_80/libs/cublas64_11/Program.217.sm_80.sass:73276
            OpLDS -> pLDX

            --       LDSM.16.M88.4    R80, [R80] {!6,+1.W}; // examples/sm_80/samples/cudaTensorCoreGemm.sass:4571
            OpLDSM -> pLDX

            --     LEA            R10, P1,  R6,   R4,                0x2     {!4,Y}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7191
            --     LEA            R6,       R3,   0xc0800000,        0x17    {!1}; // examples/sm_80/samples/BezierLineCDP.sass:1322
            --     LEA.HI         R20,      R20,  0x1,           RZ, 0x1c    {!1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7141
            -- @P0 LEA.HI.X       R2,       R2,   c[0x0][0x16c], R7, 0x1, P1 {!2}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7824
            --     LEA.HI.X.SX32  R9,       R9,   RZ,                0x1, P1 {!2}; // examples/sm_80/samples/boxFilter_kernel.sass:9014
            --
            -- [71:64]  src2 (only enabled if .HI and not .SX32)
            -- [72]     ~ on src0
            -- [73]     .SX32
            -- [74]     .X (activates the carry-in predicate)
            -- [79:75]  the shift amount (0x0 to 0x1F)?
            -- [80]     .HI
            -- [84:81]  carry-out predicate (always enabled; PT defaults)
            -- [90:87]  carry-in predicate expression (defults to !PT)
            OpLEA -> do
              dst <- pDstR
              dst_co <- P.option dST_PT $ P.try $ pSymbol "," >> pDstP
              --
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              src2 <- P.option sRC_RZ $ P.try $ pSymbol "," >> pSrcR
              src3 <- pSymbol "," >> pSrcI8 -- the shift
              src4_ci <- P.option sRC_NPT $ P.try $ pSymbol "," >> pSrcP
              --
              pComplete
                [dst,dst_co]
                [resolved src0,src1,resolved src2,resolved src3,resolved src4_ci]

            -- LEPC R130 {!2,^4}
            OpLEPC -> do
              dst <- pDstR
              pComplete [dst] []

            --  LOP3.LUT             R15, R16, R15,         RZ, 0xfc, !PT {!2}; // synthetic old form
            --  LOP3.(s0|s1)         R15, R16, R15,         RZ, 0xfc, !PT {!1}; // examples/sm_75/samples\BlackScholes.sass:1220
            --  LOP3.(s0&s1)    P2,  RZ,  R35, 0x7fffffff,  RZ, 0xc0, !PT {!4,Y}; // examples/sm_75/samples\bilateral_kernel.sass:1557
            --  LOP3.LUT.PAND        R15 ...
            OpLOP3 -> do
              dsts <- pDstsP_R
              --
              src0 <- pSymbol "," >> pSrcR
              src12s <- pSymbol "," >> pSrc_RX_XR
              src3_lut <- pSymbol "," >> pLop3LutOptSrc m_lop3_opt
              src4 <- pSymbol "," >> pSrcP
              --
              pComplete dsts (resolved src0:src12s ++ resolveds [src3_lut,src4])

            -- MATCH.ALL         R18, R34
            -- MATCH.ANY         R18, R34
            -- MATCH.ANY.U64     R18, R34
            -- MATCH.ALL     PT, R18, R34
            OpMATCH -> do
              dsts <- pDstsP_R
              src <- pSrcR
              pComplete dsts [resolved src]

            --       MEMBAR.ALL.GPU    {!6};   // examples/sm_80/samples/conjugateGradientMultiBlockCG.sass:2811
            -- @P5   MEMBAR.GPU        {!6};   // examples/sm_80/libs/cusolver64_10/Program.2588.sm_80.sass:124843
            OpMEMBAR -> pNullaryOp

            --        MOV       R1,  c[0x0][0x28] {!7,Y}; // examples/sm_80/samples/alignedTypes.sass:2811
            --        MOV       R3,  0x20 {!1,^1};     // examples/sm_80/samples/alignedTypes.sass:2822
            --        MOV       R10, R4, 0xe {!2}
            -- [75:72] src mask
            OpMOV -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcX
              src1 <- P.option sRC_I32_15 $ pSymbol "," >> pSrcI8
              pComplete [dst] $ [src0,resolved src1]

            -- MOVM.16.MT88     R12, R13 {!4,+1.W,+2.R,^3}; // examples/sm_80/libs/cusolver64_10/Program.2634.sm_80.sass:2866
            --
            -- there appears to be no others (bit [76] causes it to wiggle)
            --   MOVM.INVALID6.MT88 R25, R25 {!1,+1.W}
            -- the assemble fails to emit output
            OpMOVM -> pUnaryOp

            --       MUFU.RCP         R7,   R6 {!1,+1.W};     // examples/sm_80/samples/BezierLineCDP.sass:1326
            -- @P0   MUFU.RSQ         R4,   R3 {!2,+1.W};     // examples/sm_80/samples/BezierLineCDP.sass:1501
            --       MUFU.EX2         R15,  c[0x0][0x0] {!2,+1.W}
            --       MUFU.RCP.F16     R15,  R26         {!2,+1.W}
            --       MUFU.EX2         R15,  -|R26| {!2,+1.W}
            --
            -- [76:74] = {COS,SIN,EX2,LG2,RCP,RSQ,RCP64H,RSQ64H,SQRT,TANH,INVALID10..INVALID15}
            -- [73] = enables .F16 (RCP64H,RSQ64H are illegal)
            OpMUFU -> do
              dst <- pDst
              src <- pSymbol "," >> pSrcX
              pComplete [dst] [src]

            -- NOP     {Y};   // examples/sm_80/samples/alignedTypes.sass:2852
            OpNOP -> pNullaryOp

            -- NANOSLEEP  R8 {!3}; -- R8 encodes in src1
            -- NANOSLEEP  c[0x0][0x0] {!3};
            -- NANOSLEEP  0x8 {!3};
            OpNANOSLEEP -> do
              src <- pSrcX
              pComplete [] [src]

            -- P2R       R37, PR, RZ,  0x1 {!2}; // examples/sm_80/samples/cdpQuadtree.sass:7520
            -- P2R.B1    R82, PR, R82, 0x78 {!2}; // examples/sm_80/libs/cusolver64_10/Program.2588.sm_80.sass:1425029
            -- P2R       R77, PR, RZ,  UR56 {!4}
            --
            -- [77:76] this is {[.B0], .B1, B2, .B3}
            OpP2R -> do
              dst <- pDstR
              pSymbol "," >> pSymbol "PR"
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              pComplete [dst] [resolved src0,src1]

            -- PLOP3.((s0|s1)&s2) P0, PT, P0, P1, PT, 0xa8, 0x0 {!13,Y};
            -- PLOP3.(s0&s1&s2)   P0, PT, PT, PT, UP0, 0x80, 0x0 {!1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:9440
            OpPLOP3 -> do
              dst0 <- pDstP
              dst1 <- pSymbol "," >> pDstP
              --
              src0 <- pSymbol "," >> pSrcP
              src1 <- pSymbol "," >> (pSrcP <|> pSrcUP)
              src2 <- pSymbol "," >>  (pSrcP <|> pSrcUP)
              src3_lut <- pSymbol "," >> pLop3LutOptSrc m_lop3_opt -- [??]
              src4_unk <- pSymbol "," >> pSrcI8 -- bits [23:16]
              --
              pComplete [dst0,dst1] (resolveds [src0,src1,src2,src3_lut,src4_unk])

            -- @P0   POPC   R0, R3 {!2,+1.W};     // examples/sm_80/samples/cdpQuadtree.sass:8186
            --       POPC   R5, UR6 {!1,+2.W};    // examples/sm_80/libs/nppif64_11/Program.42.sm_80.sass:3462
            OpPOPC -> pUnaryOp

            -- PRMT             R3, R4, 0x7604, R4 {!4,Y,^1};
            --
            -- [74:72] = {[default],.F4E,.B4E,.RC8,.ECL,.ECR,.RC16,INVALID7}
            OpPRMT -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcR
              src12s <- pSymbol "," >> pSrc_RX_XR
              pComplete [dst] $ resolved src0:src12s

            -- QSPC.E.S   P1, RZ, [R12] {!2,+1.W};
            -- QSPC.E.G   P1, RZ, [R12] {!2,+1.W};
            --
            -- [74] = {.G,.S}
            -- [84:81]  = dst predicate
            OpQSPC -> do
              dsts <- pDstsP_R
              (r_addr,ur_off,i_off) <- pSymbol "," >> pLDST_Addrs op
              pComplete dsts (resolveds [r_addr,ur_off,i_off])

            -- @P0  R2P  PR, R167.B1,  0x18 {!2};
            --      R2P  PR, R32,      0x7e {!13,Y}; // examples/sm_80/samples/MonteCarlo_kernel.sass:27677
            --      R2P  PR, R32.B1,   0x7f {!13,Y}; // examples/sm_80/samples/MonteCarlo_kernel.sass:27748
            --      R2P  PR, R32.B2,   0x7f {!13,Y}; // examples/sm_80/samples/MonteCarlo_kernel.sass:27822
            --      R2P  PR, R32.B3,   0x7f {!13,Y}; // examples/sm_80/samples/MonteCarlo_kernel.sass:27896
            -- no dst predicate unlike R2UR
            OpR2P -> do
              pSymbol "PR" -- this always seems to be present, but I can't make it wiggle
              src0 <- pSymbol "," >> pSrcR_WithModSuffixes [ModB1,ModB2,ModB3,ModREU] -- [31:24]
              src1 <- pSymbol "," >> pSrcX -- R, C, I  ([63:32])
              pComplete [] ([resolved src0, src1])

            -- @P3   R2UR     UR18, R4 {!8,+4.W,+2.R,^4};
            --       R2UR     UR5,  R13 {!1,+4.W,+2.R,^2}; // examples/sm_80/libs/cublas64_11/Program.1001.sm_80.sass:10784
            -- @P2   R2UR     UR20, R2 {!8,+3.W,+2.R,^3}; // examples/sm_80/libs/cublas64_11/Program.1008.sm_80.sass:80782
            --       R2UR     UR10, R2 {!1,+3.W};   // examples/sm_80/libs/cusolver64_10/Program.1863.sm_80.sass:561858
            --       R2UR P6, UR7,  R9 {!1,+3.W,+1.R}
            -- [84:81] is the dst predicate (defaults to PT if absent)
            OpR2UR -> do
              dst_p <- P.option dST_PT $ pDstP <* pSymbol ","
              dst <- pDstUR
              src <- pSymbol "," >> pSrcR
              pComplete [dst] [resolved src]

            -- RED.E.ADD.STRONG.GPU            [R4.64],      R11 {!4,+1.R};
            -- RED.E.ADD.F32.FTZ.RN.STRONG.GPU [R20.64+0x4], R2 {!5,+2.R};
            --
            -- [72] = {INVALID, .E}
            -- [76:73]:
            --     0 = [default=??.F32?,
            --     1 = .S32,
            --     2 = .64,
            --     3 = F32.FTZ.RN
            --     4 = .F16x2.RN,
            --     5 = .S64
            --     6 = .F64.RN
            --     7 = INVALID7
            --     8..16 = INVALID8...INVALID16 (so it's a 4b field)
            --  [77] = .PRIVATE
            --  [79:78] = {[default],.CTA,.SM,.GPU}
            --  [80] = {.STRONG,.CONSTANT}
            --  [86:84] = {EF, [default], EL, LU, EU, NA, INVALID6, INVALID7}
            --  [89:87] = {ADD, MIN, MAX, INC, DEC, AND, OR, XOR}
            OpRED -> do
              (src0_addr,src0_ur_off,src0_i_off) <- pLDST_Addrs op
              pSymbol ","
              src1 <- pSrcR
              pComplete [] (resolveds [src0_addr,src0_ur_off,src0_i_off,src1])

            -- REDUX.SUM.S32     UR6, R4 {!2,+1.W};
            -- REDUX.MAX.S32     UR6, R3 {!1,+1.W};
            --
            -- [73] = {[default], .S32}
            -- [80:78] = {[default AND?], .OR, .XOR, .SUM, .MIN, .MAX, INVALID6, INVALID7}
            OpREDUX -> do
              dst <- pDstUR <* pSymbol ","
              src0 <- pSrcR
              pComplete [dst] [resolved src0]

            -- RET.ABS.NODEC    R20 0x0 {!5,^1};
            -- RET.REL.NODEC    R2 `(_Z27compute_bf16gemm_async_copyPK13__nv_bfloat16S1_PKfPfff) {!5}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:9638
            --
            -- [85] = {.REL, .ABS}
            -- [86] = {[default], .NODEC}
            -- [90:87] = optional predicate source
            OpRET -> do
              src0 <- P.option sRC_PT $ P.try $ pSrcP <* pSymbol ","
              src1 <- pSrcR
              -- no comma
              src2 <- pSrcLbl pc op
              pComplete [] [resolved src0,resolved src1,src2]

            --       S2R  R3, SR_TID.X {!2,+1.W}; // examples/sm_80/samples/alignedTypes.sass:3463
            OpS2R -> pS2XROp pDstR

            --       S2UR   UR4,  SR_CTAID.X {!1,+1.W}; // examples/sm_80/samples/conjugateGradientCudaGraphs.sass:771
            OpS2UR -> pS2XROp pDstUR

            -- @P0   SEL   R5, RZ, 0xffffffff, P1 {!2};
            OpSEL -> pSelectOp

            -- SGXT.U32    R11, R11, 0x2 {!2}
            -- SGXT        R11, R11, 0x2 {!2}
            -- SGXT.W      R11, R11, 0x2 {!2}
            -- SGXT.W.U32  R11, R11, 0x2 {!2}
            -- SGXT.U32    R11, R11, c[0x0][0x0] {!2};
            -- SGXT.U32    R11, R11, UR2 {!2};
            OpSGXT -> pBinaryOp

            -- SHF.R.S32.HI  R5,  RZ, 0x1f, R0 {!1,^1}; // examples/sm_80/samples/alignedTypes.sass:3537
            -- SHF.L.U32     R3,  R2.reuse, c[0x0][0x170], RZ {!2}; // examples/sm_80/samples/fastWalshTransform.sass:1326
            --
            --   [74:73] = {.S64, .U64, .S32, .U32}
            --   [75] = {[default], .W}
            --   [76] = {.L, .R}
            --   [80] = {[default], .HI}
            OpSHF -> pTernaryOp

            --         SHFL.UP      P1, R0, R23, 0x1,  RZ {!1,+2.W}; // examples/sm_80/samples/cdpAdvancedQuicksort.sass:4267
            --         SHFL.DOWN    PT, R4, R15, 0x1,  0x1f {!4,+2.W}; // examples/sm_80/samples/c++11_cuda.sass:19700
            --         SHFL.IDX     P0, R4, R4,  R3,   R6 {!2,+1.W}; // examples/sm_80/samples/cdpAdvancedQuicksort.sass:3491
            --         SHFL.BFLY    PT, R5, R33, 0x10, 0x1f {!4}; // examples/sm_80/samples/conjugateGradientMultiDeviceCG.sass:6817
            -- dst-pred is [83:81]
            OpSHFL -> do
              dst0 <- pDstP
              dst1 <- pSymbol "," >> pDstR
              --
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX -- technically only R|I
              src2 <- pSymbol "," >> pSrcX -- technically only R|I
              --
              pComplete [dst0,dst1] [resolved src0,src1,src2]

            --       ST.E   [R18.64], R23 {!4,+1.R,^3}; // examples/sm_80/samples/cdpQuadtree.sass:8603
            OpST -> pSTX

            --       STG.E  [R16.64+0x20], R3 {!1,+1.R}; // examples/sm_80/samples/BezierLineCDP.sass:1641
            -- [75:73] {U8, S8, U16, S16, [default == .32], .64, .128, INVALID7}
            -- [76] enables uniform register ([67:64])
            -- [77]
            OpSTG -> pSTX

            -- @P0   STL.U16          [R1+0x20], R3 {!1,+3.R}; // examples/sm_80/samples/cdpSimplePrint.sass:1131
            OpSTL -> pSTX

            -- @!P0  STS.128 [R3.X16+0x80], R4 {!4}; // examples/sm_80/samples/ParticleSystem_cuda.sass:68659
            OpSTS -> pSTX

            -- SUST.D.BA.2D.STRONG.SM.TRAP [R18], R3, 0x0, 0x58 {!1};
            -- SUST.D.BA.2D.STRONG.SM.TRAP [R18], R3, 0x0, 0x18, 0x1 {!1};
            OpSUST -> do
              (src0_r_addr,src0_ur_off,src0_i_off) <- pLDST_Addrs op <* pSymbol ","
              src1 <- pSrcR <* pSymbol ","
              src2 <- pSrcImmNonBranch32 op
              src3s <- P.option [] $ box <$> (pSymbol "," >> pSrcImmNonBranch32 op)
              pComplete [] $
                resolveds ([src0_r_addr,src0_ur_off,src0_i_off] ++ src3s)

            -- TEX.SCR.LL.NODEP R14, R16, R14, R20, 0x0, 0x5a, 2D      {!1,Y}
            -- TEX.SCR.LL       RZ,  R7,  R6,  R18, 0x0, 0x58, 2D, 0x1 {!3,Y,+6.W};
            -- TEX.SCR.B.LL     R4,  R6,  R12, R4,             2D      {!1,Y,+6.W,^6};
            --
            --   src3a imm is [58:54]
            --   src3b imm is 14b [53:40]
            --   src4 surface shape selector is [63:60]
            --   src5 0x1
            OpTEX -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcR
              src2 <- pSymbol "," >> pSrcR
              pTextureOp dst [src0,src1,src2]

            -- TLD.SCR.LZ       RZ, R6, R14, 0x0, 0x66, 1D, 0x3 {!1,Y,+6.W};
            OpTLD -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcR
              pTextureOp dst [src0,src1]

            --          dst   dst  ...
            -- UFLO.U32  UR4,      UR4 {!6,Y};  // examples/sm_80/libs/cusolver64_10/Program.1728.sm_80.sass:1649
            -- UFLO.U32  UR6, UP6, UR4 {!1}
            --
            -- dst-pred [84:81] is UPT by default
            OpUFLO -> do
              dst <- pDstUR
              dst_p <- P.option dST_UPT $ P.try $ pSymbol "," >> pDstUP
              src0 <- pSymbol "," >> pSrcURI
              pComplete [dst,dst_p] [src0]

            --                  DST  CO1   CO2  SRC0  SRC1          SRC2   CI1   CI2
            -- UIADD3           UR9,            URZ,  -UR4,          URZ             {!1};
            -- UIADD3.X         UR4,            UR4,   UR7,          URZ,  UPT, !UPT {!2,Y};
            -- UIADD3           UR5,           -UR9,   -0x7fffffff,  URZ             {!6,Y};
            -- UIADD3           UR22, UP0, UP1, UR14,  UR16,        -UR22            {!1};
            -- UIADD3.X         UR23,           UR15,  UR27,        ~UR23, UP0,  UP1 {!1};
            -- UIADD3.X         UR5,  UP1, UP2, URZ,   UR5,          URZ,  UP0, !UPT {!1}
            -- no constant buffer version
            --  [83:81] carry-out1; defaults UPT
            --  [86:84] carry-out2; defaults UPT
            -- given [74] .X
            --  [90:87] carry-in1; defaults !UPT
            --  [80:77] carry-in2; defaults !UPT
            OpUIADD3 -> do
              dst <- pDstUR
              dst_cos <-
                P.option [dST_UPT,dST_UPT] $ P.try $ do
                  dst_co1 <- pSymbol "," >> pDstUP
                  dst_co2 <- P.option dST_UPT $ P.try $ pSymbol "," >> pDstUP
                  return [dst_co1,dst_co2]
              --
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              --
              src_cis <-
                P.option [sRC_UPF,sRC_UPF] $ do
                  src_ci1 <- pSymbol "," >> pSrcUP
                  src_ci2 <- pSymbol "," >> pSrcUP
                  return [src_ci1,src_ci2]
              pComplete (dst:dst_cos) (resolved src0:src1:resolved src2:resolveds src_cis)

            --                  DST  CO1    SRC0  SRC1          SRC2   CI1
            -- UIMAD            UR6,        UR6,  0xc0,         URZ          {!2};
            -- UIMAD.WIDE.U32   UR6, UP0,   UR8,  0x24924924,   UR4          {!2};
            -- UIMAD.WIDE.U32.X UR4,        UR10, 0x24924924,   UR8,   UP0   {!4,Y};
            -- UIMAD.WIDE.U32.X UR4,        UR10, UR11,         UR8,   UP0   {!4,Y};
            OpUIMAD -> do
              dst <- pDstUR
              dst_co <-
                P.option dST_UPT $ P.try $ do
                  pSymbol "," >> pDstUP
              --
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              src_ci <-
                P.option sRC_UPF $ do
                  pSymbol "," >> pSrcUP
              --
              pComplete
                [dst,dst_co]
                [resolved src0,src1,resolved src2,resolved src_ci]

            -- UISETP.GE.AND     UP0, UPT, UR6,  UR8,   UPT      {!2}; // examples/sm_80/samples/bilateral_kernel.sass:1602
            -- UISETP.NE.AND.EX  UP1, UPT, UR30, 0x1,   UPT, UP0 {!2}; // examples/sm_80/libs/cusolver64_10/Program.2624.sm_80.sass:266602
            -- probably close to OpISETP
            -- dst/src preds are same as UIADD3
            --    [72] .EX
            --    [73] .U32
            -- second input predicates defaults to UPT
            OpUISETP -> do
              dst0 <- pDstUP
              dst1 <- pSymbol "," >> pDstUP
              src0 <- pSymbol "," >> resolved <$> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> resolved <$> pSrcUP
              -- .EX has an extra predicate
              src3 <- P.option sRC_UPT $ pSymbol "," >> pSrcUP -- 0b0111 (PT) if not set
              pComplete [dst0,dst1] [src0,src1,src2,resolved src3]

            -- ULDC       UR5, c[0x0][0x160] {!1}; // examples/sm_80/libs/cublas64_11/Program.224.sm_80.sass:54823
            -- ULDC.64    UR4, c[0x0][0x118] {!2}; // examples/sm_80/libs/cublas64_11/Program.224.sm_80.sass:56237
            OpULDC -> do
              dst <- pDstUR
              srcs <- pSymbol "," >> pXLdcSrcs pSrcUR SrcURZ
              pComplete [dst] (resolveds srcs)

            --                  DST  CO   SRC0  SRC1       SRC2  shift CI
            -- ULEA             UR15,     UR16, UR10,            0x3       {!4,Y,^1};
            -- ULEA             UR7, UP0, UR6,  UR17,            0x3       {!2};
            -- ULEA.HI.X.SX32   UR4,      UR4,  UR12,            0x1,  UP0 {!1};
            -- ULEA.HI          UR4,      UR4,  0x7fffffff, URZ, 0x1f      {!2};
            -- ULEA             UR5,      UR8,  0xfffffffc,      0x2       {!1};
            -- ULEA.HI.SX32     UR6,      UR6,  0xfffffffc,      0x1b      {!1};
            --
            OpULEA -> do
              dst <- pDst
              --
              dst_p <- P.option dST_UPT $ P.try $ pSymbol "," >> pDstUP
              --
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- P.option sRC_URZ $ P.try $ pSymbol "," >> pSrcUR
              src3_sh <- pSymbol "," >> pSrcI8
              src4_ci <- P.option sRC_UPF $ pSymbol "," >> pSrcUP
              --
              pComplete
                [dst,dst_p]
                (resolved src0:src1:resolveds [src2,src3_sh,src4_ci])

            --                  DST   SRC0
            -- UMOV             UR5,  URZ    {!10,Y};
            -- UMOV             UR4,  0x800  {!1};
            -- UMOV             UR4,  32@lo(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei)) {!1};
            --
            -- There's no lane mask like MOV
            OpUMOV -> do
              dst <- pDstUR <* pSymbol ","
              src0 <- pSrcURI
              pComplete [dst] [src0]

            -- ULOP3.(s0|s1)         UR16, UR6, 0x1,        URZ, 0xfc, !UPT {!2};
            -- ULOP3.(s0&s1)         UR7,  UR9, 0xffffffe0, URZ, 0xc0, !UPT {!2};
            -- ULOP3.LUT.PAND  UP2,  UR4,  UR4, UR5,        URZ, 0xc0, !UPT ...
            OpULOP3 -> do
              dst_p <- P.option [dST_UPT] $ do
                box <$> pDstUP <* pSymbol ","
              dst <- pDstUR <* pSymbol ","
              --
              src0 <- pSrcUR <* pSymbol ","
              src1 <- pSrcURI <* pSymbol ","
              src2 <- pSrcUR <* pSymbol ","
              src3_lut <- pLop3LutOptSrc m_lop3_opt <* pSymbol ","
              src4_pr <- pSrcUP
              --
              pComplete (dst_p++[dst]) (resolved src0:src1:resolveds [src2,src3_lut,src4_pr])

            -- UPLOP3.LUT       UP0, UPT, UPT, UPT, UPT, 0x80, 0x0 {!3,Y};
            --
            -- no idea on the layout here, but no one seems to make legit use of it
            -- (UP0 seems to have no consumers)
            OpUPLOP3 -> do
              dst0 <- pDstUP
              dst1 <- pSymbol "," >> pDstUP
              --
              src0 <- pSymbol "," >> pSrcUP
              src1 <- pSymbol "," >> pSrcUP
              src2 <- pSymbol "," >> pSrcUP
              src3_lut <- pSymbol "," >> pLop3LutOptSrc m_lop3_opt -- [??]
              src4_unk <- pSymbol "," >> pSrcI8 -- bits [23:16]
              --
              pComplete [dst0,dst1] (resolveds [src0,src1,src2,src3_lut,src4_unk])

            -- UPOPC   UR10, UR7 {!4,Y};     // examples/sm_80/libs/cublas64_11/Program.217.sm_80.sass:40870
            -- UPOPC   UR4,  0x4 {!4,Y};
            OpUPOPC -> pUUnaryOp

            -- UPRMT  UR11, UR11, 0x8880, URZ {!1}; // examples/sm_80/libs/cublas64_11/Program.735.sm_80.sass:5609
            -- UPRMT  UR7,  UR7,  UR16,   URZ {!1}
            OpUPRMT -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              pComplete [dst] [resolved src0,src1,resolved src2]

            -- USEL   UR4,  UR4,  UR6, !UP0 {!2}; // examples/sm_80/samples/cdpAdvancedQuicksort.sass:4908
            -- USEL   UR4,  UR4,  0x6, !UP0 {!2};
            OpUSEL -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2_p <- pSymbol "," >> pSrcUP
              pComplete [dst] [resolved src0,src1,resolved src2_p]

            -- USGXT       UR4, UR4, 0x18 {!6,Y,^1}; // examples/sm_80/samples/convolutionTexture.sass:984
            -- USGXT       UR4, UR5, UR24 {!3,Y,^2};
            -- USGXT.W     UR4, UR5, 0x18 {!3,Y,^2};
            OpUSGXT -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              pComplete [dst] [resolved src0,src1]

            -- USHF.L.U32     UR6, UR5,  0x3, URZ {!1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7765
            -- USHF.R.S32.HI  UR8, URZ, 0x1f, UR7 {!2,Y}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:8324
            OpUSHF -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              pComplete [dst] [resolved src0,src1,resolved src2]

            --                DST   DST?
            --    VOTE.ANY    R5,   PT,  P1 {!4,Y};
            --    VOTE.ALL    R6,   PT,  PT {!1,^1};
            OpVOTE -> do
              dst <- pDstR
              dst_p <- pSymbol "," >> pDstP -- dst I think [83:81]
              src0 <- pSymbol "," >> pSrcP -- [90:87]
              pComplete [dst,dst_p] [resolved src0]

            --    VOTEU.ANY   UR36, UPT,  PT {!1};   // examples/sm_80/samples/cdpAdvancedQuicksort.sass:3566
            --    VOTEU.ALL   UR4,  UPT,  PT {!1};    // examples/sm_80/samples/globalToShmemAsyncCopy.sass:4529
            --    VOTEU.ANY   UR4,  UPT, !P1 {!12,Y}; // examples/sm_80/samples/reduction_kernel.sass:118610
            OpVOTEU -> do
              dst <- pDstUR
              dst_p <- pSymbol "," >> pDstUP -- dst I think [83:81]
              src0 <- pSymbol "," >> pSrcP -- [90:87]
              pComplete [dst,dst_p] [resolved src0]

            --       WARPSYNC                0xffffffff {!4};
            -- @!P2  WARPSYNC                0xffffffff {!4};
            --       WARPSYNC.EXCLUSIVE      R2 {!4};
            --       WARPSYNC                R19 {!4};
            -- (synthetic)
            --       WARPSYNC           !P4, 0xffffffff
            --
            -- [86] .EXCLUSIVE
            -- [90:87] predicate source (default to PT)
            OpWARPSYNC -> do
              src0 <- P.option sRC_PT $ pSrcP <* pSymbol ","
              src1 <- pSrcX
              pComplete [] [resolved src0,src1]

            --          YIELD       {!1};
            --          YIELD   !P5 {!1};
            -- /*2050*/ YIELD    (*"RELOCATOR OPCODE, YIELD, 280"*) {!1};
            --              8272        R_CUDA_YIELD_CLEAR_PRED4_87
            --              8272        R_CUDA_YIELD_OPCODE9_0    280
            --
            -- [90:87] predicate source (default to PT)
            OpYIELD -> do
              let pRelocator = do
                    pSymbol "(*\""
                    P.many (P.noneOf (";\""))
                    pSymbol "\"*)"
                    pWhiteSpace
                    return sRC_PT
              src0 <- P.option sRC_PT $ P.try (pRelocator <|> pSrcP) -- [90:87]
              pComplete [] [resolved src0]

            -- TODO: disable and force everything through a pre-chosen path
            _ -> do
              pSemanticError loc $ "unsupported op"
              -- dsts <- pDsts op
              -- unless (null dsts) $ do
              --   pSymbol_ ","
              -- unresolved_srcs <- pSrcs pc op
              -- pComplete dsts unresolved_srcs

        pLdOp :: Loc -> Pred -> Op -> InstOptSet -> PI (Unresolved Inst)
        pLdOp loc prd op ios = do
          let pDstPred = do
                p <- pDstP
                pSymbol_ ","
                return [p]
          dst_p <- if InstOptZD`iosElem`ios then pDstPred else return []
          dst_r <- pDstR
          pSymbol_ ","
          (r_addr,ur_off,i_off) <- pLDST_Addrs op

          pCompleteInst
            loc prd op ios (dst_p++[dst_r]) (resolveds [r_addr,ur_off,i_off]) []

        pAtOp :: Loc -> Pred -> Op -> InstOptSet -> PI (Unresolved Inst)
        pAtOp loc prd op ios = do
          dsts <- pDstsP_R
          (r_addr,ur_off,i_off) <- pSymbol "," >> pLDST_Addrs op
          -- sometimes src1, src2 are absent
          src1 <- P.option sRC_RZ $ pSymbol "," >> pBareSrcR
          src2 <- P.option sRC_RZ $ pSymbol "," >> pBareSrcR -- CAS has extra param
          pCompleteInst loc prd op ios dsts (resolveds [r_addr,ur_off,i_off,src1,src2]) []

        pStOp :: Loc -> Pred -> Op -> InstOptSet -> PI (Unresolved Inst)
        pStOp loc prd op ios = do
          (r_addr,ur_off,i_off) <- pLDST_Addrs op
          r_data <- pSymbol "," >> pBareSrcR
          pCompleteInst loc prd op ios [] (resolveds [r_addr,ur_off,i_off,r_data]) []

        --  R (+ UR) (+ IMM)
        --       UR  (+ IMM)
        --       IMM (+ UR)
        --
        -- TODO: support for LDS offsets and the other [91:90] stuff
        pLDST_Addrs :: Op -> PI (Src,Src,Src)
        pLDST_Addrs op = do
          let pBareSrcUR :: PI Src
              pBareSrcUR = pLabel "ureg" $ Src_UR msEmpty <$> pSyntax
          let pR_UR_IMM = do
                -- technically LDS/STS don't permit .U32 or .64
                src_addr <-
                  if oIsSLM op
                    then pSrcR_WithModSuffixes [ModX4,ModX8,ModX16]
                    else pSrcR_WithModSuffixes [ModU32,Mod64]
                (_,src_ur,src_imm) <-
                  P.option (sRC_RZ,sRC_URZ,sRC_I32_0) $ do
                    pSymbol "+"
                    P.try pUR_IMM <|> pIMM_UR
                return (src_addr,src_ur,src_imm)
              pUR_IMM :: PI (Src,Src,Src)
              pUR_IMM = do
                ur <- pBareSrcUR
                imm <- P.option sRC_I32_0 $ pSymbol "+" >> pSrcI32 op
                return (sRC_RZ,ur,imm)
              pIMM_UR :: PI (Src,Src,Src)
              pIMM_UR = do
                imm <- pSrcI32 op
                ur <- P.option sRC_URZ $ pSymbol "+" >> pBareSrcUR
                return (sRC_RZ,ur,imm)
          pSymbol "["
          r <- P.try pUR_IMM <|> P.try pIMM_UR <|> pR_UR_IMM
          pSymbol "]"
          return r

        -- for LOP3 and PLOP3 we may or may not have the LUT code as a source,
        -- but we want it to match
        --
        -- PLOP3.((s0|s1)&s2) P0, PT, P0, P1, PT, 0xa8, 0x0 {!13,Y};
        --                                        ^^^^
        --  LOP3.(s0&s1)    P2,  RZ,  R35, 0x7fffffff,  RZ, 0xc0, !PT {!4,Y}; // examples/sm_75/samples\bilateral_kernel.sass:1557
        --                                                  ^^^^
        pLop3LutOptSrc :: Maybe Word8 -> PI Src
        pLop3LutOptSrc m_lop3_opt =
          case m_lop3_opt of
            -- if they didn't give us an explicit code, it better be here
            Nothing -> pSrcI8
            Just lop3_opt -> do
              -- if they gave us a code earlier, it better match
              -- anything parsed here
              loc_hex_lut <- pGetLoc
              m_hex_lut <- P.optionMaybe (pImmA :: PI Word8)
              case m_hex_lut of
                Just hex_lut
                  | hex_lut /= lop3_opt ->
                    pSemanticError
                      loc_hex_lut
                      ("mismatch between inst opt logical option expression and classic-form LUT operand; inst. opt expression evaluates to " ++ printf "0x02X" lop3_opt)
                _ -> return () -- better that they omit it
              return (SrcImm (Imm32 (fromIntegral lop3_opt)))

        pModSetSuffixesOptOneOf :: [Mod] -> PI ModSet
        pModSetSuffixesOptOneOf ms = do
          let msSingleton s = msFromList [s]
              opts = map (\m -> ("." ++ drop 3 (show m),m))
          P.option msEmpty $ msSingleton <$> pOneOf (opts ms)

        -- [Pred, ] Reg
        pDstsP_R :: PI [Dst]
        pDstsP_R = do
          dst_pred <- P.option [] $ P.try $ do
            p <- pDstP <* pSymbol ","
            return [p]
          dst_reg <- pDstR
          return $ dst_pred ++ [dst_reg]

        pBareSrcR :: PI Src
        pBareSrcR = pLabel "reg" $ Src_R msEmpty <$> pSyntax

        -- up to two predicate sources
        -- pOptPreds = do
        --   P.option [] $ do
        --     p_src0 <- pSymbol "," >> pPredSrcP
        --     P.option [p_src0] $ do
        --       p_src1 <- pSymbol "," >> pPredSrcP
        --       return [p_src0,p_src1]

        pCompleteInst ::
          Loc ->
          Pred ->
          Op ->
          InstOptSet ->
          [Dst] ->
          [Unresolved Src] ->
          [Pred] ->
          PI (Unresolved Inst)
        pCompleteInst loc prd op opts dsts unresolved_srcs src_preds = do
          dep_info <- pDepInfo
          P.try $ pSymbol ";"
          return $ \lbls -> do
            srcs <- sequence (map ($lbls) unresolved_srcs)
            return $
              Inst {
                iLoc = loc
              , iPc = pc
              , iPredication = prd
              , iOp = op
              , iOptions = opts
              , iDsts = dsts
              , iSrcs = srcs
              , iSrcPreds = src_preds
              , iDepInfo = dep_info
              }


pPred :: PI Pred
pPred = do
    pSymbol "@"
    sign <- P.option False $ pSymbol "!" >> return True
    tryP sign <|> tryUP sign
  where tryP sign = PredP sign <$> pSyntax
        tryUP sign = PredUP sign <$> pSyntax

pPredSrcP :: PI Pred
pPredSrcP = do
    sign <- P.option False $ pSymbol "!" >> return True
    tryP sign
  where tryP sign = PredP sign <$> pSyntax


pOp :: PI Op
pOp = pWithLoc $ \loc -> do
  op_str <- pIdentifier
  case reads ("Op"++op_str) of
    [(op,"")] -> return op
    _ -> pSemanticError loc $ "unrecognized mnemonic"


pInstOpts :: Op -> PI (InstOptSet,Maybe Word8)
pInstOpts op
  | is_lop3 = P.try pLop3Code <|> pOptSeq Nothing
  | otherwise = pOptSeq Nothing
  where is_lop3 = op `elem` [OpLOP3,OpPLOP3,OpULOP3,OpUPLOP3]

        pLop3Code :: PI (InstOptSet,Maybe Word8)
        pLop3Code = do
          pSymbol "."
          e <- pLop3Expr
          pOptSeq (Just e)

        pOptSeq :: Maybe Word8 -> PI (InstOptSet,Maybe Word8)
        pOptSeq mlut = do
            ios <- iosFromList . concat <$> P.many pToken
            return (ios,mlut)
          where pToken = do
                  pSymbol "."
                  P.try pIgnore <|> pInstOpt

        pIgnore :: PI [InstOpt]
        pIgnore
          | op == OpIMAD = pSymbols ["MOV","SHL","IADD"] >> return []
          -- | op == Op "IMAD" = pSymbols [] >> return []
          | is_lop3 = pSymbols ["LUT"] >> return []
          | otherwise = fail "pseudo option"

        pInstOpt :: PI [InstOpt]
        pInstOpt = pLabel "instruction option" $ do
          io <- box <$> pSyntax
          return io



pLop3Expr :: PI Word8
pLop3Expr = pOrExpr
  where pOrExpr = pXOrExpr `P.chainl1` pOr
          where pOr = pSymbol "|" >> return (.|.)
        pXOrExpr = pAndExpr `P.chainl1` pXOr
          where pXOr = pSymbol "^" >> return xor
        pAndExpr = pNegExpr `P.chainl1` pAnd
          where pAnd = pSymbol "&" >> return (.&.)
        pNegExpr = pNeg <|> pAtomExpr
          where pNeg = do
                  pSymbol "~"
                  complement <$> pAtomExpr
        pAtomExpr = pGrp <|> pAtom <|> pW8
          where pGrp = do {pSymbol "("; e<-pOrExpr; pSymbol ")"; return e}
                pAtom = pOneOf [("s0",0xF0),("s1",0xCC),("s2",0xAA)]
                pW8 = pHexImm <* pWhiteSpace


pDsts :: Op -> PI [Dst]
pDsts op = do
    dst_pred_opt <- pDstPredOpt
    dst <- pDst
    return $ dst_pred_opt ++ [dst]
  where pDstPredOpt
          | oHasDstPred op = do
              dp <- pDstP
              pSymbol ","
              return [dp]
          | otherwise = return []


pDst :: PI Dst
pDst = pDstR <|> pDstP <|> pDstUP <|> pDstUR <|> pDstB
pDstR :: PI Dst
pDstR = pLabel "dst reg" $ DstR <$> pSyntax
pDstB :: PI Dst
pDstB = pLabel "dst barrier reg" $ DstB <$> pSyntax
pDstP :: PI Dst
pDstP = pLabel "dst pred reg" $ DstP <$> pSyntax
pDstUP :: PI Dst
pDstUP = pLabel "dst unif pred reg" $ DstUP <$> pSyntax
pDstUR :: PI Dst
pDstUR = pLabel "dst unif reg" $ DstUR <$> pSyntax


resolved :: a -> Unresolved a
resolved = const . Right
resolveds :: [a] -> [Unresolved a]
resolveds = map resolved

-- TODO: remove
pSrcsN :: Int -> PC -> Op -> PI [Unresolved Src]
pSrcsN 0 _  _  = return []
pSrcsN n pc op = do
  src0 <- pSrc pc op
  srcs <- sequence $ replicate (n - 1) (pSymbol "," >> pSrc pc op)
  return (src0:srcs)
-- TODO: remove
pSrcs :: PC -> Op -> PI [Unresolved Src]
pSrcs pc op = P.sepBy (pSrc pc op) (pSymbol ",")


-- floating point (fp32 or fp64)
pSrcRCUF :: PI Src
pSrcRCUF = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcFlt32

-- pSrcRCUH2 :: PI Src
-- would parse a pair of fp16 and merge into a W32
-- need an FP32 to FP16 conversion routine

-- integral
pSrcRCUI :: PI Src
pSrcRCUI = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcInt32


-- TODO: remove
pSrc :: PC -> Op -> PI (Unresolved Src)
pSrc pc = pWithLoc . pSrcAt pc
pSrcAt :: PC -> Op -> Loc -> PI (Unresolved Src)
pSrcAt pc op loc = P.try pNonLabel <|> pSrcLblAt pc op loc
  where pNonLabel :: PI (Unresolved Src)
        pNonLabel = alwaysSucceeds <$> pCases
          where pCases =
                  P.try pImmNonLabel <|>
                  pSrcCCX <|>
                  pSrcR <|>
                  pSrcSR <|>
                  pSrcUR <|>
                  pSrcUP <|>
                  pSrcP <|>
                  pSrcB

        pImmNonLabel
          | oIsBranch op = pSrcI49 op
          | otherwise = pSrcI32 op

        alwaysSucceeds :: Src -> Unresolved Src
        alwaysSucceeds src _ = Right (srcIntern src)

pSrcLbl :: PC -> Op -> PI (Unresolved Src)
pSrcLbl pc = pWithLoc . pSrcLblAt pc
pSrcLblAt :: PC -> Op -> Loc -> PI (Unresolved Src)
pSrcLblAt pc op loc = pLabel "label expression" $ do
  e <- pLExpr pc
  let eval :: Unresolved Src
      eval lix
        | oIsBranch op = SrcImm . Imm49 . fromIntegral <$> evalLExpr lix e
        | otherwise = do
          val <- evalLExpr lix e
          if val < -2^31 || val > 2^31-1
            then Left (dCons loc "value overflows 32b imm")
            else return (SrcI32 (fromIntegral val))
  -- FIXME: for BR we need to use big op
  return eval


-- imm32, but can be a symbol too
pSrcI32OrL32 :: PC -> Op -> PI (Unresolved Src)
pSrcI32OrL32 pc op = pLabel "imm operand" $
  P.try (pSrcLbl pc op) <|> resolved <$> pSrcI32 op


-- LDC wedges src0 into the src1 expression
-- c[0x3][R3+0x8]
-- cx[UR3][R0+0x8]
pXLdcSrcs :: PI Src -> Src -> PI [Src]
pXLdcSrcs pSrcXR xrz = pLabel "const addr" $ pInd <|> pDir
  where pDir = do
          pSymbol "c"
          pSymbol "["
          s <- pSignedInt
          pSymbol "]"
          pSurfOff (SurfImm s)
        --
        pInd = do
          P.try (pSymbol "cx")
          pSymbol "["
          u <- pSyntax
          pSymbol "]"
          pSurfOff (SurfReg u)
        --
        --  ...[R13+0x10]
        --  ...[R13-0x10]
        --  ...[R13+-0x10]
        --  ...[R13]
        --  ...[0x10] -- means RZ+...
        pSurfOff :: Surf -> PI [Src]
        pSurfOff surf = do
          pSymbol "["
          let pOffsetTerm = pIntTerm
              -- handles both [R13] and [R13+...]
              pRegSum = P.try $ do
                r <- pSrcXR
                off <- P.option 0 pOffsetTerm
                return [r, SrcCon msEmpty surf off]
              --
              pImmOnly = do
                off <- pOffsetTerm <|> pInt
                return [xrz, SrcCon msEmpty surf off]
          --
          srcs <- pRegSum <|> pImmOnly
          pSymbol "]"
          return srcs

pSrcR_MMA :: PI Src
pSrcR_MMA = pLabel "reg src" $ pSrcR_WithModSuffixes [ModREU,ModROW,ModCOL]


pSrcCCXH2 :: PI Src
pSrcCCXH2 = pLabel "const src" $ do
  SrcCon ms s o <- pSrcCCX
  ms_sfx <- pAddModRegSuffixesFrom [ModH0_H0,ModH1_H1]
  return $ SrcCon (ms`msUnion`ms_sfx) s o


pSrcRH2 :: PI Src
pSrcRH2 = pLabel "reg src" $ pSrcR_WithModSuffixes [ModREU,ModH0_H0,ModH1_H1]
pSrcURH2 :: PI Src
pSrcURH2 = pLabel "uniform reg src" $ pSrcUR_WithModSuffixes [ModREU,ModH0_H0,ModH1_H1]

pDstRH2 :: PI Dst
pDstRH2 = pLabel "dst reg" $ do
  dst_r <- pSyntax
  ms_sfx <- pAddModRegSuffixesFrom [ModH0_H0,ModH1_H1]
  return $ DstRms ms_sfx dst_r

-- e.g. R10, R10.H0_H0, or c[0][0], c[0][0].H1_H1, or pair of imm16
-- TODO: pair ImmH2
pSrcXH2s :: PI [Src]
pSrcXH2s =
  box <$> P.try pSrcRH2 <|>
    box <$> P.try pSrcCCXH2 <|>
    box <$> P.try pSrcURH2 <|>
    pSrcImmH2


pSrcCCX :: PI Src
pSrcCCX = pLabel "const src" $ P.try pConstInd <|> P.try pConstDir
  where pConstDir :: PI Src
        pConstDir = do
          let pCon = do
                pSymbol "c"
                pSymbol "["
                s <- pInt
                pSymbol "]"
                pSymbol "["
                o <- pSignedInt
                pSymbol "]"
                return (SurfImm s,o)
          ((s,o),ms_neg_abs) <- pWithNegAbs pCon
          return $ SrcCon ms_neg_abs s o

        pConstInd :: PI Src
        pConstInd = do
          ((u,o),ms_neg_abs) <-
            pWithNegAbs $ do
              pSymbol "cx"
              pSymbol "["
              u <- pSyntax
              pSymbol "]"
              pSymbol "["
              o <- pSignedInt
              pSymbol "]"
              return (SurfReg u,o)
          return $ SrcCon ms_neg_abs u o


pSignedInt :: PI Int
pSignedInt = do
  sign <- P.option id $ pSymbol "-" >> return negate
  sign <$> pInt


pSrcImmH2 :: PI [Src]
pSrcImmH2 = do
  -- TODO: these are really supposed to be packed into a single operand
  s0 <- pSrcFlt32
  s1 <- pSymbol "," >> pSrcFlt32
  return [s0,s1]

-- rejects 32@hi... and 32@lo...
pSrcI49 :: Op -> PI Src
pSrcI49 op = pLabel "imm49" $ srcIntern <$> pSrcImmNonBranch49 op
pSrcImmNonBranch49 :: Op -> PI Src
pSrcImmNonBranch49 op = do
  imm <- fromIntegral <$> pIntImm64
  P.notFollowedBy (P.char '@')
  pWhiteSpace
  return $ SrcImm (Imm49 imm)

-- cannot be a label
pSrcI8 :: PI Src
pSrcI8 = pLabel "imm8" $ srcIntern <$> pIt
  where pIt = pLexeme $ do
          w8 <- pImmA :: PI Word8 -- will fail gracefully on overflow
          return (SrcImm (Imm32 (fromIntegral w8)))

pSrcI32 :: Op -> PI Src
pSrcI32 op = pLabel "imm32" $ srcIntern <$> pSrcImmNonBranch32 op
pSrcImmNonBranch32 :: Op -> PI Src
pSrcImmNonBranch32 op = do
    imm <- pVal32
    P.notFollowedBy (P.char '@')
    pWhiteSpace
    return $ SrcI32 imm
  where pVal32
          -- TODO: should we convert integer to FP?
          | oIsFP op = pFltImm32 <|> pIntImm32
          | otherwise = pIntImm32

pSrcFlt32 :: PI Src
pSrcFlt32 = pLabel "imm operand" $ SrcI32 <$> pFltImm32

pSrcInt32 :: PI Src
pSrcInt32 = pLabel "imm operand" $ SrcI32 <$> pIntImm32


-- e.g. +0x10 or -0x10 or +-0x10
pIntTerm :: PI Int
pIntTerm =  pPlusX <|> pMinus
  where pOff modf = modf <$> pInt
        pMinus = pSymbol "-" >> pOff negate
        pPlusX = do
          pSymbol "+"
          pMinus <|> pOff id


pNegationBitwise :: PI ModSet
pNegationBitwise =
  pSymbol "-" >> return (msSingleton ModANEG)
pNegationNumeric :: PI ModSet
pNegationNumeric =
  pSymbol "~" >> return (msSingleton ModBNEG)
pNegationLogical :: PI ModSet
pNegationLogical = P.option msEmpty $
  pSymbol "!" >> return (msSingleton ModLNEG)

pNegationBitwiseOrNumericOpt :: PI ModSet
pNegationBitwiseOrNumericOpt = P.option msEmpty $
  pNegationNumeric <|> pNegationBitwise

-- how to make this work with <$>...<*>...<*>...
-- pWithNegAbsC :: PI a -> PI (Bool -> Bool -> PI a)

pWithNegAbs :: PI a -> PI (a,ModSet)
pWithNegAbs pBody = do
  ms_neg <- pNegationBitwiseOrNumericOpt
  let pWithAbs = do
        pSymbol "|"
        r <- pBody
        pSymbol "|"
        return (r,True)
  (r,abs) <- pWithAbs <|> (pBody >>= \r -> return (r,False))
  let char x a = if x then msSingleton a else msEmpty
  let ms = msIntern $ msUnion ms_neg $ char abs ModABS
  return (r,ms)


pAddModRegSuffixesFrom :: [Mod] -> PI ModSet
pAddModRegSuffixesFrom ms = msIntern . msFromList <$> P.many (pOneOf opts)
  where opts = map (\m -> (msFormatSuffix m,m)) ms

pSrcR :: PI Src
pSrcR = pSrcR_WithModSuffixes [ModREU]

pSrcR_WithModSuffixes :: [Mod] -> PI Src
pSrcR_WithModSuffixes ms = pLabel "reg operand" $ do
  (reg,ms_pfx) <- pWithNegAbs (pLabel "reg" pSyntax)
  ms_sfx <- pAddModRegSuffixesFrom ms
  return $ SrcReg (msIntern (ms_pfx`msUnion`ms_sfx)) (RegR reg)

pSrcUR_WithModSuffixes :: [Mod] -> PI Src
pSrcUR_WithModSuffixes ms = pLabel "uniform reg operand" $ do
  (reg,ms_pfx) <- pWithNegAbs (pLabel "uniform reg" pSyntax)
  ms_sfx <- pAddModRegSuffixesFrom ms
  return $ SrcReg (msIntern (ms_pfx`msUnion`ms_sfx)) (RegUR reg)


pSrcSB :: PI Src
pSrcSB = pLabel "sb reg operand" $ SrcReg msEmpty . RegSB <$> pSyntax

pSrcSR :: PI Src
pSrcSR = pLabel "sys reg operand" $ SrcReg msEmpty . RegSR <$> parse
  where parse = P.try pUnderbarTidCtaid <|> pSyntax
        pUnderbarTidCtaid =
          -- we accept these with a dot, but we also allow
          -- the more IR-consistent form here (SR_TID.X)
          --
          -- the regular form pSyntax will replace the _ with a .
          -- to match nvdisasm's output; thus we favor the nvdisasm
          -- version in output and whatnot
          (pSymbol "SR_TID_X" >> return SR_TID_X) <|>
          (pSymbol "SR_TID_Y" >> return SR_TID_Y) <|>
          (pSymbol "SR_TID_Z" >> return SR_TID_Z) <|>
          (pSymbol "SR_CTAID_X" >> return SR_TID_X) <|>
          (pSymbol "SR_CTAID_Y" >> return SR_TID_Y) <|>
          (pSymbol "SR_CTAID_Z" >> return SR_TID_Z)

pSrcUR :: PI Src
pSrcUR = pLabel "uniform src operand" $ do
  (ur,ms) <- pWithNegAbs pSyntax
  return (Src_UR ms ur)

pSrcP :: PI Src
pSrcP = pLabel "predicate src operand" $ do
  ms <- pNegationLogical
  Src_P ms <$> pSyntax
pSrcUP :: PI Src
pSrcUP = pLabel "uniform predicate src operand" $ do
  ms <- pNegationLogical
  Src_UP ms <$> pSyntax


pSrcB :: PI Src
pSrcB = pLabel "convergence barrier reg operand" $ Src_B msEmpty <$> pSyntax


pLExpr :: PC -> PI LExpr
pLExpr pc = pBitExpr
  where pBitExpr = pAddExpr
        pAddExpr = P.chainl1 pMulExpr pAddOp
          where pAddOp = pOp "+" LExprAdd <|> pOp "-" LExprSub
        pMulExpr = P.chainl1 pUnrExpr pMulOp
          where pMulOp = pOp "*" LExprMul <|> pOp "/" LExprDiv <|> pOp "/" LExprMod
        pOp sym cons = pWithLoc $ \loc -> pSymbol sym >> return (cons loc)

        pUnrExpr = pUnOp "-" LExprNeg <|> pUnOp "~" LExprCompl <|> pPrimExpr
          where pUnOp sym cons = pWithLoc $ \loc -> do
                  pSymbol sym
                  e <- pUnrExpr
                  return $ cons loc e
        pPrimExpr = pWithLoc pPrimExprBody
        pPrimExprBody loc =
            pTryFunc "fun@fdesc" LExprFunFDesc <|>
            pTryFunc "32@lo" LExprLo32 <|>
            pTryFunc "32@hi" LExprHi32 <|>
            P.try pTickExpr <|> -- `(...)
            P.try pLabel <|>
            pGroup <|>
            pLit
          where -- e.g. foo or foo@srel
                pLabel :: PI LExpr
                pLabel = do
                  ident <- pLabelChars
                  when (ident `elem` ["c","cx"]) $
                    P.notFollowedBy (P.char '[') -- not const operands (c[... or cx[...)
                  pModify $ \ps -> ps{pisLabelReferences = (pc,ident):pisLabelReferences ps}
                  cons <-
                    P.option id $ pWithLoc $ \loc -> do
                      --
                      pSymbol "@srel"
                      soff <- pSectionOffset
                      return (LExprSRel loc soff)
                  pWhiteSpace
                  return $ cons $ LExprLabel loc ident
                pGroup = do
                  pSymbol "("
                  e <- pBitExpr
                  pSymbol ")"
                  return e
                pLit = LExprImm loc <$> (pImmA <* pWhiteSpace)
                --
                pTryFunc :: String -> (Loc -> LExpr -> LExpr) -> PI LExpr
                pTryFunc sym cons = P.try $ do
                  pSymbol sym
                  pSymbol "("
                  e <- pBitExpr
                  pSymbol ")"
                  return (cons loc e)
                --
                pTickExpr = pSymbol "`" >> pPrimExpr


pLabelChars :: PI String
pLabelChars = pDotLabel <|> pNonDotLabel
  where pDotLabel = do
          pfx <- P.option "" (P.char '.' >> return ".")
          (pfx++) <$> pNonDotLabel

        pNonDotLabel = do
          let pOtherIdentChar = P.oneOf "_$"
          c0 <- P.letter <|> pOtherIdentChar
          sfx <- P.many $ P.alphaNum <|> pOtherIdentChar
          return (c0:sfx)

evalLExpr :: LabelIndex -> LExpr -> Either Diagnostic Int64
evalLExpr lix = eval
  where eval :: LExpr -> Either Diagnostic Int64
        eval le =
          case le of
            LExprAdd loc ll l2 -> applyBin loc ll l2 (+)
            LExprSub loc ll l2 -> applyBin loc ll l2 (-)
            LExprMul loc ll l2 -> applyBin loc ll l2 (*)
            LExprDiv loc ll l2 -> applyBinDiv loc ll l2 div
            LExprMod loc ll l2 -> applyBinDiv loc ll l2 mod
            LExprNeg _ l -> applyUnr l negate
            LExprCompl _ l -> applyUnr l complement
            LExprImm _ val -> return val
            --
            LExprLo32 _ le -> applyUnr le (.&.0xFFFFFFFF)
            LExprHi32 _ le -> applyUnr le ((.&.0xFFFFFFFF) . (`shiftR`32))
            LExprSRel loc soff le ->
              case eval le of
                Left d -> Left d
                Right val -> return (val - soff)
            LExprFunFDesc _ le ->
              -- FIXME: implement this at some point
              -- I don't know what a "function descriptor"
              eval le
            --
            LExprLabel loc sym ->
              case sym `lookup` lix of
                Nothing -> err loc $ sym ++ ": unbound symbol"
                Just val -> return (fromIntegral val)

        applyBin = applyBinG False
        applyBinDiv = applyBinG True

        applyBinG ::
          Bool ->
          Loc ->
          LExpr -> LExpr -> (Int64 -> Int64 -> Int64) ->
          Either Diagnostic Int64
        applyBinG div loc e1 e2 f = do
          v1 <- eval e1
          v2 <- eval e2
          if div && v2 == 0 then err loc "division by 0"
            else return (f v1 v2)

        applyUnr :: LExpr -> (Int64 -> Int64) -> Either Diagnostic Int64
        applyUnr le f = f <$> eval le

        err :: Loc -> String -> Either Diagnostic a
        err loc = Left . dCons loc

-- Label expressions
--
-- EXAMPLE:
-- _Z10testKerneli: 32@lo((_Z10testKerneli + .L_3@srel))
-- ...
--  /*0120*/       MOV              R20, 32@lo((_Z10testKerneli + .L_3@srel)) {!2};  /* 000FE40000000F00`0000000000147802 */
--  /*0130*/       MOV              R21, 32@hi((_Z10testKerneli + .L_3@srel)) {!7,Y};  /* 000FCE0000000F00`0000000000157802 */
--  /*0140*/       CALL.ABS.NOINC   `(__assertfail) {!5,Y};                    /* 000FCA0003C00000`0000000000007943 */
-- .L_3:
-- ....
--
-- 32@lo((_Z10testKerneli + .L_3@srel)) must mean the low 32 bits of the sum of
-- _Z10testKerneli (device function offset) and the section-relative offset of
-- the label .L_3 (0x150 in this example).  The CUDA binary has:
-- Relocation section '.rela.text._Z10testKerneli' at offset 0xe98 contains 2 entries:
--  Offset          Info           Type           Sym. Value    Sym. Name + Addend
-- 000000000130  000a00000039 unrecognized: 39      0000000000000000 _Z10testKerneli + 150
-- 000000000120  000a00000038 unrecognized: 38      0000000000000000 _Z10testKerneli + 150
--
-- .section .rela.text._Z10testKerneli     RELA
-- 288    _Z10testKerneli    R_CUDA_ABS32_LO_32    336 (0x120)
-- 304    _Z10testKerneli    R_CUDA_ABS32_HI_32    336 (0x130)
--
-- EXAMPLE: 32@lo(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei))
-- computeBezierLinePositions(int, BezierLine*, int)
--  /*04F0*/       UMOV             UR4, 32@lo(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei)) {!1};  /* 000FE20000000000`0000000000047882 */
--  ...
--  /*0510*/       UMOV             UR5, 32@hi(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei)) {!1};  /* 000FE20000000000`0000000000057882 */
-- % readelf -r has
-- 00000000000004f0  000000180000003e unrecognized: 3e      0000000000000000 _Z26computeBezierLinePositionsiP10BezierLinei
-- 0000000000000510  000000180000003f unrecognized: 3f      0000000000000000 _Z26computeBezierLinePositionsiP10BezierLinei
-- ..
--   .section .rel.text._Z21computeBezierLinesCDPP10BezierLinei	REL
--   1296    _Z26computeBezierLinePositionsiP10BezierLinei    R_CUDA_FUNC_DESC32_HI_32
--   1264    _Z26computeBezierLinePositionsiP10BezierLinei    R_CUDA_FUNC_DESC32_LO_32
data LExpr =
  ---------------------------------------------------------------------
  -- binary additive expressions
    LExprAdd !Loc !LExpr !LExpr
  | LExprSub !Loc !LExpr !LExpr
  -- binary multiplicative expressions
  | LExprMul !Loc !LExpr !LExpr
  | LExprDiv !Loc !LExpr !LExpr
  | LExprMod !Loc !LExpr !LExpr
  -- unary expressions
  | LExprNeg   !Loc !LExpr -- -E
  | LExprCompl !Loc !LExpr -- ~E
  --
  ---------------------------------------------------------------------
  -- primary expressions
  --
  -- an immediate
  | LExprImm !Loc !Int64
  -- a symbol
  | LExprLabel !Loc !String -- e.g. ".L_15"
  --
  -- label functions
  | LExprLo32 !Loc !LExpr -- 32@lo(<LExpr>)
  | LExprHi32 !Loc !LExpr -- 32@hi(<LExpr>)
  --
  -- I think this is section relative offset
  -- .L_3@srel would be the offset of .L_3 relative to the section we are in
  | LExprSRel !Loc !Int64 !LExpr -- <LExpr>@srel (section offset is given)
  --
  -- R_CUDA_FUNC_DESC32_HI_32/R_CUDA_FUNC_DESC32_LO_32
  -- usually (always?) nested in a 32@lo(...)
  | LExprFunFDesc !Loc !LExpr -- fun@fdesc(<LExpr>)
  deriving Show


pIntImm32 :: PI Word32
pIntImm32 = do
  -- Were we to use Int32, then fromIntegral to Word64 would sign extend
  -- and create a value that won't fit in encoding.
  --
  -- The function negate :: Word32 -> Word32 is still two's-complement
  let pImmS32 = pImmA :: PI Word32
  maybeNegate <- P.option id (pSymbol "-" >> return negate)
  maybeNegate <$> pImmS32
pIntImm64 :: PI Word64
pIntImm64 = do
  let pImmS64 = pImmA :: PI Word64
  maybeNegate <- P.option id (pSymbol "-" >> return negate)
  maybeNegate <$> pImmS64

pFltImm32 :: PI Word32
pFltImm32 = do
    let pNegSign = pSymbol "-" >> return (flip complementBit 31)
        pSignFunc =
          pNegSign <|> (P.option id (pSymbol "+" >> return id))
    neg <- pSignFunc
    neg <$> (pInf <|> pQNan <|> P.try pNonInf <|> pWholeNumberFlt)
  where pInf = pSymbol "INF" >> return 0x7F800000
        -- pNan = pSymbol "NAN" >> return 0x7FC00000
        pQNan = pSymbol "QNAN" >> return 0x7FC00000
        pNonInf = fromIntegral . floatToBits . doubleToFloat <$> pFloating
        pWholeNumberFlt = do
          f32 <- read <$> P.many1 P.digit :: PI Float
          -- do I want a not-followed by?
          pWhiteSpace
          return (floatToBits f32)



-- {!8,Y,+2.R,+3.W,^4,^1}
pDepInfo :: PI DepInfo
pDepInfo = diIntern <$> parseIt
  where parseIt = P.option diDefault $ pSymbol "{" >> pTokenLoop 0 diDefault

        pTokenLoop :: Int -> DepInfo -> PI DepInfo
        pTokenLoop ix di = pEnd <|> (pMaybeSep >> pToken)
          where pEnd = do pSymbol "}" >> return di
                pMaybeSep = unless (ix == 0) $ pSymbol_ ","
                pToken =
                  pDist <|>
                    pWait <|>
                      P.try (pAlloc diAllocRd 'R' (\b -> di{diAllocRd = Just b})) <|>
                        pAlloc diAllocWr 'W' (\b -> di{diAllocWr = Just b}) <|>
                          pYield

                pDist = do
                  i <- pSymbol "!" >> pOneOf [(show i,i)|i<-[0..15]]
                  pTokenLoop (ix+1) di{diStalls = i}
                pYield = pSymbol "Y" >> pTokenLoop (ix+1) di{diYield = True}
                pWait = do
                  ix <- P.char '^' >> pBarrier
                  pTokenLoop (ix+1) di{diWaitSet = setBit (diWaitSet di) (ix-1)}

                pAlloc f what cons = do
                  P.char '+'
                  b_ix <- pBarrier
                  P.char '.' >> P.char what
                  case f di of
                    Nothing -> pTokenLoop (ix+1) (cons b_ix)
                    Just _ -> fail $ what:"-dependency barrier already set"

                pBarrier = pOneOf [(show i,i)|i<-[1..7]]


-----------------
pInstOptDebug :: PI InstOpt
pInstOptDebug = do
    pTrace $ concatMap (\(s,_) -> show s ++ "\n") (orderByLen es)
    pOneOfKeyword "" es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]

pSyntax :: (Enum s,Syntax s) => PI s
pSyntax = pLabel tnm $ pOneOfKeyword tnm es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]
        tnm = formatTypeName (snd (head es))

