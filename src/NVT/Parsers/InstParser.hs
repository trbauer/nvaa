module NVT.Parsers.InstParser where

import NVT.Parsers.NVParser
import NVT.Parsers.OperandParsers
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


parseInst ::
  PC ->
  FilePath ->
  Int ->
  String ->
  PIResult Inst
parseInst pc = runPI_WLNO (pInst pc)


pInst :: PC -> PI Inst
pInst pc = pWhiteSpace >> pInstNoPrefix pc

pInstNoPrefix :: PC -> PI Inst
pInstNoPrefix pc = body
  where body = pWithLoc $ \loc -> do
          op <- P.lookAhead $ P.option () (pPred >> return ()) >> pOp
          let pred_true
                | oIsU op = pred_upt
                | otherwise = pred_pt
          prd <- P.option pred_true pPred
          _ <- pOp
          (ios,m_lop3_opt) <- pInstOpts op
          --
          let pComplete :: [Dst] -> [Src] -> PI Inst
              pComplete dsts srcs = do
                dep_info <- pDepInfo
                P.try $ pSymbol ";"
                return $
                  Inst {
                    iLoc = loc
                  , iId = 0
                  , iPc = pc
                  , iPredication = prd
                  , iOp = op
                  , iOptions = ios
                  , iDsts = dsts
                  , iSrcs = srcs
                  , iDepInfo = dep_info
                  }

          let box x = [x]

          let -- general variable operand (e.g. src1 in unary/binary ops)
              -- (note unary usually uses src1 instead of src0)
              pSrcX :: PI Src
              pSrcX
                | oIsFP op = pSrcRCUF op
                | otherwise = pSrcRCUI pc op

              -- used to parse SRC1, SRC2 in ternary
              pSrc_RX_XR :: PI [Src]
              pSrc_RX_XR = P.try (tryOrd pSrcR pSrcX) <|> (tryOrd pSrcX pSrcR)
                where tryOrd p1 p2 = do
                        src1 <- p1
                        src2 <- pSymbol "," >> p2
                        return [src1,src2]

              -- UR, R, IMM
              pSrcURI :: PI Src
              pSrcURI = P.try pSrcUR <|> pSrc_I32OrL32 pc op

              -- [Pred, ] Reg
              pDstsP_R :: PI [Dst]
              pDstsP_R = do
                dst_pred <- P.option [] $ P.try $ do
                  p <- pDstP <* pCommaOrColon
                  return [p]
                dst_reg <- pDstR
                return $ dst_pred ++ [dst_reg]

              pBareSrcR :: PI Src
              pBareSrcR = pLabel "reg" $ Src_R msEmpty <$> pSyntax

              pModSetSuffixesOptOneOf :: [Mod] -> PI ModSet
              pModSetSuffixesOptOneOf ms = do
                let msSingleton s = msFromList [s]
                    opts = map (\m -> ("." ++ drop 3 (show m),m))
                P.option msEmpty $ msSingleton <$> pOneOf (opts ms)

              -- for LOP3 and PLOP3 we may or may not have the LUT code as a source,
              -- but we want it to match
              --
              -- PLOP3.((s0|s1)&s2) P0, PT, P0, P1, PT, 0xa8, 0x0 {!13,Y};
              --                                        ^^^^
              --  LOP3.(s0&s1)    P2,  RZ,  R35, 0x7fffffff,  RZ, 0xc0, !PT {!4,Y}; // examples/sm_75/samples\bilateral_kernel.sass:1557
              --                                                  ^^^^
              pLop3LutOptSrc :: PI Src
              pLop3LutOptSrc =
                case m_lop3_opt of
                  -- if they didn't give us an explicit code, then it better be here
                  Nothing -> pSymbol "," >> pSrcI8
                  --
                  -- otherwise we can try for it, but if it's absent, we can ignore it
                  Just lop3_opt -> do
                    -- if they gave us a code earlier, it better match
                    -- anything parsed here
                    loc_hex_lut <- pGetLoc
                    hex_lut <- P.option lop3_opt $ P.try $ pSymbol "," >> pImmA :: PI Word8
                    unless (hex_lut == lop3_opt) $
                      pSemanticError
                        loc_hex_lut
                        ("mismatch between inst opt logical option expression and classic-form LUT operand; inst. opt expression evaluates to " ++ printf "0x02X" lop3_opt)
                    return (SrcI8 (fromIntegral lop3_opt))

              ------------------------------
              -- Prior to SM9.0
              --  [ R (+ UR) (+ IMM) ]
              --  [      UR  (+ IMM) ]
              --  [     IMM  (+ UR)  ]
              --
              -- SM9.0:
              --  desc[UR][ R (+ IMM) ]
              --  desc[UR][      IMM ]
              --
              -- TODO: support for LDS offsets and the other [91:90] stuff
              pLDST_Addrs :: Op -> PI Src
              pLDST_Addrs op = P.try pAddrDesc <|> pAddrOld
                where pAddrOld = do
                        pSymbol "["
                        (r_r_ms,ur,imm) <-
                          P.try pUR_IMM <|> P.try pIMM_UR <|> pR_UR_IMM
                        pSymbol "]"
                        return $ SrcAddr r_r_ms ur imm

                      pAddrDesc = do
                        pKeyword "desc" >> pSymbol "["
                        u <- pBareSrcUR
                        pSymbol "]"
                        pSymbol "["
                        ((r,r_ms),i) <- P.try pRZ_IMM <|> pR_IMM
                        pSymbol "]"
                        return $ SrcDescAddr u (r,r_ms) i

                      pR_IMM :: PI ((R,ModSet),Int)
                      pR_IMM = do
                        r_r_ms <- pR
                        imm <- P.option 0x0 pImmTerm
                        return (r_r_ms,imm)

                      pRZ_IMM :: PI ((R,ModSet),Int)
                      pRZ_IMM = do
                        imm <- pImmAtom
                        return ((RZ,msEmpty),imm)

                      --------------------------------------------------
                      pR :: PI (R,ModSet)
                      pR = do
                        r <-
                          if oIsSLM op
                            then pSrcR_WithModSuffixes [ModX4,ModX8,ModX16]
                            else pSrcR_WithModSuffixes [ModU32,Mod64]
                        case r of
                          SrcReg ms (RegR r) -> return (r,ms)

                      pR_UR_IMM :: PI ((R,ModSet),UR,Int)
                      pR_UR_IMM = do
                        -- technically LDS/STS don't permit .U32 or .64
                        r_r_ms <- pR
                        (_,src_ur,src_imm) <-
                          P.option (error "unreachable",URZ,0x0) $ do
                            pSymbol "+"
                            P.try pUR_IMM <|> pIMM_UR
                        return (r_r_ms,src_ur,src_imm)

                      pUR_IMM :: PI ((R,ModSet),UR,Int)
                      pUR_IMM = do
                        ur <- pBareSrcUR
                        imm <- P.option 0x0 pImmTerm
                        return ((RZ,msEmpty),ur,imm)

                      pIMM_UR :: PI ((R,ModSet),UR,Int)
                      pIMM_UR = do
                        imm <- pImmAtom
                        ur <- P.option URZ $ pSymbol "+" >> pBareSrcUR
                        return ((RZ,msEmpty),ur,imm)

                      pBareSrcUR :: PI UR
                      pBareSrcUR = pLabel "ureg" pSyntax

                      pImmTerm :: PI Int
                      pImmTerm = do
                        sgn <-
                          (pSymbol "+" >> return id) <|>
                            (pSymbol "-" >> return negate)
                        sgn . fromIntegral <$> pIntImm32

                      pImmAtom :: PI Int
                      pImmAtom = P.try $ do
                        sgn <- P.option id (pSymbol "-" >> return negate)
                        sgn . fromIntegral <$> pIntImm32

              -------------------------------------------------------------------
              --

              -- e.g. NOP, ERRBAR, ...
              pNullaryOp :: PI Inst
              pNullaryOp = pComplete [] []

              -- e.g. BFREV, F2F
              pUnaryOp :: PI Inst
              pUnaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcX
                pComplete [dst] [src0]

              -- e.g. MOVM
              pUnaryOpR :: PI Inst
              pUnaryOpR = do
                dst <- pDstR
                let pTrap = pWithLoc $ \loc -> do
                      pSrcX
                      pSemanticError loc "this op requires reg operand"
                src0 <- pSymbol "," >> (pSrcR <|> pTrap)
                pComplete [dst] [src0]

              pUUnaryOp :: PI Inst
              pUUnaryOp = do
                dst <- pDstUR
                src0 <- pSymbol "," >> pSrcURI
                pComplete [dst] [src0]

              pS2XROp :: PI Dst -> PI Inst
              pS2XROp pDst = do
                dst <- pDst
                src <- pSymbol "," >> pSrcSR
                pComplete [dst] [src]

              -- CS2R and S2R
              pSR2RFamily :: PI Inst
              pSR2RFamily = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcSR
                pComplete [dst] [src0]

              --  IMNMX   R3, R0, 0x4, !PT {!4,Y,^1};
              pSelectOp :: PI Inst
              pSelectOp = do
                dst <- pDst
                srcs <- pSymbol "," >> pSrc_RX_XR
                src2_pr <- pSymbol "," >> pSrcP
                pComplete [dst] (srcs ++ [src2_pr])

              -- a simple binary op without fancy stuff
              --   e.g. BMSK
              pBinaryOp :: PI Inst
              pBinaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcX
                pComplete [dst] [src0,src1]

              -- a simple binary op without fancy stuff
              --   e.g. BMSK    R22, R19,  R22
              pBinaryOpRR :: PI Inst
              pBinaryOpRR = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcR
                pComplete [dst] [src0,src1]

              -- HADD2, HMUL2
              pBinaryOpH2 :: PI Inst
              pBinaryOpH2 = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcRH2
                -- technically this will parse two operands if immediate
                -- since each half value is split up
                src1 <- pSymbol "," >> pSrcXH2 op
                pComplete [dst] [src0,src1]

              -- simple ternary ops (no predicates/carry out/carry)
              pTernaryOp :: PI Inst
              pTernaryOp = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src12 <- pSymbol "," >> pSrc_RX_XR
                pComplete [dst] (src0:src12)

              pTernaryOpH2 :: PI Inst
              pTernaryOpH2 = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcRH2
                let pXR :: PI [Src]
                    pXR = do
                      src1 <- pSymbol "," >> pSrcXH2 op
                      src2 <- pSymbol "," >> pSrcRH2
                      return [src1,src2]
                    pRX :: PI [Src]
                    pRX = do
                      src1 <- pSymbol "," >> pSrcRH2
                      src2 <- pSymbol "," >> pSrcXH2 op
                      return [src1,src2]
                src12s <- P.try pXR <|> pRX
                pComplete [dst] (src0:src12s)

              -- e.g. HMMA/DMMA uses all R
              pTernaryOpRRR :: PI Inst
              pTernaryOpRRR = do
                dst <- pDstR
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcR
                src2 <- pSymbol "," >> pSrcR
                pComplete [dst] [src0,src1,src2]

              -- e.g. DSETP/FSETP
              pSETP :: PI Inst
              pSETP = do
                dst0 <- pDstP
                dst1 <- pSymbol "," >> pDstP
                src0 <- pSymbol "," >> pSrcR
                src1 <- pSymbol "," >> pSrcX
                src2 <- pSymbol "," >> pSrcP
                pComplete [dst0,dst1] [src0,src1,src2]

              pLDX :: PI Inst
              pLDX = do
                let pDstPred = do
                      p <- pDstP
                      pCommaOrColon
                      return [p]
                dst_p <- if InstOptZD`iosElem`ios then pDstPred else return []
                dst_r <- pDstR

                src_addr <- pSymbol "," >> pLDST_Addrs op
                -- TODO: fix the IR list length
                pComplete (dst_p++[dst_r]) [src_addr]

              pATX :: PI Inst
              pATX = do
                dsts <- pDstsP_R
                src_addr <- pSymbol "," >> pLDST_Addrs op
                -- src1, src2 are absent depending on how many operands the atomic has
                -- e.g. CAS has two
                src1 <- P.option sRC_RZ $ pSymbol "," >> pBareSrcR
                src2 <- P.option sRC_RZ $ pSymbol "," >> pBareSrcR -- CAS has extra param
                pComplete dsts [src_addr,src1,src2]

              pSTX :: PI Inst
              pSTX = do
                src_addr <- pLDST_Addrs op
                r_data <- pSymbol "," >> pBareSrcR
                pComplete [] [src_addr,r_data]

              -- TEX, TLD
              --
              -- starts just before the comma preceding the soup of trailing parameters
              -- TEX.SCR.B.LL     R4,  R6,  R12, R4,             2D {!1,Y,+6.W,^6};
              --                                   ^
              -- TEX.SCR.LL       RZ,  R2,  R2,  R4,  0x0, 0x5e, ARRAY_2D, 0x1 {!1,Y,+6.W};
              --                                   ^
              -- TLD.SCR.LZ  RZ, R8, R18, 0x0, 0x64, 1D, 0x3 {!3,Y};
              --                        ^
              pTextureOp :: [Dst] -> [Src] -> PI Inst
              pTextureOp dsts srcs = do
                -- maybe a pair of imm
                pSymbol ","
                let pNoImms = do
                      src4 <- SrcTex <$> pSyntax
                      return ([sRC_I32_0, sRC_I32_0],src4)
                    pImmsFirst = do
                      src3a <- pSrc_I32 op
                      src3b <- pSymbol "," >> pSrc_I32 op
                      src4 <- pSymbol "," >> SrcTex <$> pSyntax
                      return ([src3a,src3b],src4)
                (src3s,src4) <- P.try pNoImms <|> pImmsFirst
                src5_opt <- P.option [] $ pSymbol "," >> box <$> pSrc_I32 op
                pComplete dsts (srcs ++ src3s ++ [src4] ++ src5_opt)

          --
          case op of
            OpARRIVES -> do
              --
              -- ARRIVES.LDGSTSBAR.64 [URZ+0x800]
              -- disassembler permits R#
              --
              -- SM90:
              --  @!P0  ARRIVES.LDGSTSBAR.64.TRANSCNT  [UR4]  {!1}; // 000FE20008000A84`00000000FF0089B0
              src_addr <- pLDST_Addrs op
              pComplete [] [src_addr]

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
              pComplete [dst] [src0]

            --                                   [57:54]  [53:42]  [90:87]
            --       BAR.SYNC                     0x0      [0x0]        {!6};
            -- @P1   BAR.SYNC.DEFER_BLOCKING      0x0                   {!6};
            --       BAR.RED.POPC                 0x0,              P1  {!6};
            --       BAR.RED.AND.DEFER_BLOCKING   0x0,              P0  {!6};
            --       BAR.ARV                      0x0,      0x0         {!6}
            --       BAR.SYNCALL.DEFER_BLOCKING                         {!6}
            --       BAR.RED.AND.DEFER_BLOCKING   0x0,      0x1,    P0  {!6}
            --
            -- SM90:
            --       BAR.RED.AND.DEFER_BLOCKING   0x0,         P2 {!6};      // 000FEC0001014400`0000000000007B1D
            --       BAR.SYNC.DEFER_BLOCKING      0x0             {!6};      // 000FEC0000010000`0000000000007B1D
            --       BAR.SYNC    R13,    R13                      {!6,+2.R}; // 0003EC0000000000`0000000D0000731D
            -- @P6   BAR.SYNC.DEFER_BLOCKING  R4, 0x100           {!1,+4.R}  // 0007E20000010000`000400040000651D
            --       BAR.ARV     R4,     0x100                    {!6,+3.R}; // 0005EC0000002000`000400040000751D
            OpBAR -> do
              let pOptP = P.option [] $ pSymbol "," >> (box <$> pSrcP)
              let pXIP = do
                    src0 <- pSrcInt32 <|> pSrcR
                    src1 <- pSymbol "," >> (pSrcInt32 <|> pSrcR)
                    src2 <- pOptP
                    return $ [src0,src1] ++ src2
                  pIX = do
                    src0 <- pSrcInt32
                    src2 <- pOptP
                    return (src0:src2)
              srcs <- P.option [] $ P.try pXIP <|> pIX
              pComplete [] srcs

            -- Move Convergence Barrier State
            --                  DST  SRC0   SRC1
            -- BMOV.32          0's  B6,     R24   {!3,+3.R};
            -- BMOV.32.CLEAR    R22, B6      0's   {!3,+4.W,^4};
            -- BMOV.32.PQUAD    0's  B6,     R2 -- bit [84] enables PQUAD
            OpBMOV -> do
              dsts <- P.option [] $ box <$> pDstR <* pSymbol "," -- [23:16]
              src0 <- pSrcB -- [29:24]
              maybe_src1 <- P.option [] $ box <$> (pSymbol "," >> pSrcR) -- [39:32]
              pComplete dsts (src0:maybe_src1)

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
              src0 <- pSrc_I32 op
              pComplete [] [src0]

            -- @!P0  BRA                 `(.L_1) {!5};
            -- @!P1  BRA      !P2,       `(.L_18) {!5};
            --       BRA.DIV       ~URZ, `(.L_989) {!6};
            -- @!P4  BRA.INC             0x210 {!12,Y}
            --
            -- label [81:64]:[63:32]
            -- also
            --   [90:87] = a predicate source
            --   [86:85] = {[default], .INC, .DEC, .INVALID3}
            --
            -- SM90:
            --   [90:87] = a predicate source
            --   [86:85] = {[default], .INC, .DEC, .INVALID3}
            --
            --   @P1 BRA             `(.L_x_16) {!5}; // 000FEA0003800000`0000000000041947 examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:2944
            --       BRA.DIV   UR4, `(.L_x_15)  {!5}; // 000FEA000B800000`0000001604B07947 examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:2892
            --   @P0 BRA       P1,  `(.L_x_44)  {!5}; // 000FEA0000800000`0000000000200947 examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:3388
            --
            OpBRA -> do
              let pSM80 = do
                    maybe_src_pr <- P.option [] $ box <$> (pSrcP <* pSymbol ",")
                    src0 <- pSrcLbl pc op
                    pComplete [] (maybe_src_pr ++ [src0])
              let pSM90 = do
                    maybe_src_pr <- P.option [] $ box <$> (pSrcP <* pSymbol ",")
                    maybe_src_pr <- P.option [] $ box <$> (pSrcUR <* pSymbol ",")
                    src0 <- pSrcLbl pc op
                    pComplete [] (maybe_src_pr ++ [src0])
              pSM90

            -- @!P1  BREAK            !P2, B1 {!1};
            -- src predicate is [90:87] and PT 0b0111 is the default
            OpBREAK -> do
              src_pr <- P.option sRC_PT $ (pSrcP <* pSymbol ",")
              src0 <- pSrcB
              pComplete [] [src_pr, src0]

            -- BRX      R8  -0x1a50 {!5};
            -- BRX !P6, R12 -0xb50 {!5};
            OpBRX -> do
              src_pr <- P.option [] $ ((\c -> [c]) <$> pSrcP) <* pSymbol ","
              src0 <- pSrcR
              -- no comma unless predicate (or our formatting)
              P.option () (pSymbol "," >> return ())
              src1 <- pSrcI49 op
              pComplete [] (src_pr ++ [src0, src1])

            -- BRXU         UR4 -0x1840 {!5,Y};
            -- BRXU     P6, UR4 -0x1840 {!5,Y};
            -- []
            OpBRXU -> do
              src_pr <- P.option [] $ (((\c -> [c]) <$> pSrcP) <* pSymbol ",")
              src0 <- pSrcUR
              -- no comma unless predicate (or our formatting)
              P.option () (pSymbol "," >> return ())
              src1 <- pSrcI49 op
              pComplete [] (src_pr ++ [src0, src1])

            -- BSSY             B0, `(.L_31) {!12,Y};
            OpBSSY -> do
              dst <- pDstB
              pSymbol ","
              src0 <- pSrcLbl pc op
              pComplete [dst] [src0]

            -- BSYNC      B1 {!5}; // PT implied
            -- BSYNC !P5, B2 {!5};
            OpBSYNC -> do
              src0 <- P.option sRC_PT $ P.try $ pSrcP <* pSymbol ","
              src1 <- pSrcB
              pComplete [] [src0,src1]

            -- CALL.ABS.NOINC   `(cudaLaunchDeviceV2) {!5,Y,^1};
            --
            -- [31:24] optional reg (***defaults to 0's*** when absent)
            -- [81:64]:[63:32] label input
            -- [86] .NOINC
            -- [90:87] predicate source
            -- [regfile] enables UR and R0
            --
            -- CALL.ABS is opcode 0x143, CALL.REL uses opcode 0x144
            --
            -- SM90:
            --   CALL.ABS.NOINC  R2 `(__UFT_OFFSET) {!5,^1,^2,^3,^6}; // 027FEA0003C00000`0000000002007343 examples/sm_90/samples/0_Introduction/simpleSeparateCompilation/simpleSeparateCompilation.sass:364
            --   CALL.ABS.NOINC  `(vprintf)  {!5,^1};
            --   CALL.REL.NOINC  `($_Z16shiftPitchLinearPfiiiiiy$__cuda_sm3x_div_rn_noftz_f32_slowpath) {!5,^1};
            OpCALL -> do
              let pSm80 = do
                    src0_p <- P.option sRC_PT $ P.try $ pSrcP <* pSymbol ","
                    src1_ru <- P.option sRC_RZ $ pSrcR <|> pSrcUR
                    src2 <- pSrcLbl pc op
                    pComplete [] [src0_p,src1_ru,src2]
              let pSm90 = do
                    let box c = [c]
                    src_rc <- P.option [] $
                      box <$> (P.try pSrcR <|> P.try (pSrcCCX op))
                    src_lbl <- P.option [] $ do
                      P.option "" (pSymbol ",")
                      box <$> pSrcLbl pc op
                    pComplete [] (src_rc ++ src_lbl)
              pSm90
            --   CCTL.IVALL        {!5,Y};
            --   CCTL.U.IVALL      {!5,Y};
            --   CCTL.RS      [RZ] {!5,Y};
            --   CCTL.IV      [R47+0xffff00] {!5,Y};
            --
            -- [24]     src0
            -- [63:32]  src1-imm
            -- [79:78]  {[default], .U, .C, .I}
            -- [89:87]
            --    0b000   .PF1 (prefetch 1)  [operand]
            --    0b001   .PF2 (prefetch 2)  [operand]
            --    0b010   .WB (write back)   [operand]
            --    0b011   .IV  (invalidate)  [operand]
            --    0b100   .IVALL (invalidate all?)  no operand
            --    0b101   .RS (??)          [operand]
            --    0b110   .IVALLP (invalidate all...) no operand
            --    0b111   .WBALL (write back all)    no operand
            --
            OpCCTL -> do
              srcs <-
                P.option [sRC_RZ,sRC_I32_0] $ do
                  pSymbol "["
                  src0 <- pSrcR <|> pSrcUR
                  src1_imm <- P.option sRC_I32_0 $ SrcI32 . fromIntegral <$> pIntTerm
                  pSymbol "]"
                  return [src0,src1_imm]
              pComplete [] srcs

            -- @P1   CGAERRBAR // 000FEC0000000000`00000000000015AB
            OpCGAERRBAR -> do
              pComplete [] []


            --      CS2R     R6, SRZ {!1};
            --      CS2R.32  R5, SR_CLOCKLO {!7,Y,^2};
            --      CS2R     R4, SR_CLOCKLO {!7,Y};
            -- @P0  CS2R     R30, SRZ {!2};
            OpCS2R -> pSR2RFamily

            --
            --       DADD             R20,  R8,  c[0x3][0x8] {!2,+2.W,+1.R,^1};
            --       DADD             R14, -RZ, -R6 {!6,Y,+1.W,^1};
            -- @P1   DADD             R20, -RZ, -R14 {!10,Y,+1.W,^3};
            OpDADD -> pBinaryOp

            --   DEPBAR.LE  SB0, 0x0 {!4,Y}; (bit [44] is sb, [43:38] is imm val)
            --   DEPBAR.LE  SB0, 0x7, {5,4,3,2,1,0} ...
            --   DEPBAR (unsupported)
            --
            -- [37:32] = activates the bit set
            OpDEPBAR -> do
              src0 <- pSrcSB <* pSymbol ","
              src1 <- pSrcImmNonBranch32 op
              src2s_opt <-
                P.option [] $ do
                  pSymbol ","
                  let pBitIx = pInt <* pWhiteSpace
                  pSymbol "{"
                  b0 <- pBitIx
                  bs <- P.many $ pSymbol "," >> pBitIx
                  pSymbol "}"
                  return [SrcI32 (foldl' setBit 0 (b0:bs))]
              pComplete [] ([src0, src1] ++ src2s_opt)

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

            -- ENDCOLLECTIVE
            OpENDCOLLECTIVE -> pNullaryOp

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
            --
            -- F2I.U64 is opcode 0x111 the rest are 0x105
            OpF2I -> pUnaryOp

            -- @P0   FADD.FTZ         R4,  R0,       1 {!1};
            --       FADD.FTZ         R3, -R5,       -RZ {!4,Y};
            --       FADD             R4,  R0,       -c[0x0][0x17c] {!2,Y,^1};
            --       FADD             R3,  R0.reuse, R9.reuse {!1};
            --       FADD.FTZ         R5, -RZ,       -UR4 {!4,Y};
            OpFADD -> pBinaryOp

            -- FENCE.VIEW.ASYNC.S  {!2,+6.W}; // 000F640000000000`00000000000073C6 examples/sm_90/libs/cublasLt64_12/Elf-1510.sm_90.sass:93510
            --
            -- FENCE.VIEW.ASYNC.S  ?PM1 {!10,+1.W};  000E344000000000`00000000000073C6
            -- [103:102] ... ?PM1, ?PM2, ?PM3
            OpFENCE -> pComplete [] []

            --                 DST  SRC0  SRC1
            -- FCHK             P0, R5,    R0            {!2,+1.W};
            -- FCHK             P0, R5,    c[0x0][0x180] {!2,+1.W};
            -- FCHK             P1, R27,   R28           {!2,+1.W};
            -- dst is [83:81]
            OpFCHK -> do
              dst <- pDstP
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              pComplete [dst] [src0,src1]

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
            --
            -- SM90:
            --   HFMA2.MMA  R25,   -RZ,    RZ,  0,  0  {!1};  /* 000FE200000001FF`00000000FF197435
            --                                  ^^^^^^ fp16x2
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
              src1 <- pSymbol "," >> pSrcXH2 op
              src_p <- pSymbol "," >> pSrcP
              pComplete [dst] [src0,src1,src_p]

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
              src1 <- pSymbol "," >> pSrcXH2 op
              src_p <- pSymbol "," >> pSrcP
              --
              pComplete dsts [src0,src1,src_p]

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
                    SrcCon ms s r o <- pSrcCCX op
                    ms_sfx <- pAddModRegSuffixesFrom byte_selectors
                    return (SrcCon (ms`msUnion`ms_sfx) s r o)
              pSymbol ","
              src <-
                pSrcR_WithModSuffixes byte_selectors <|>
                  pSrcUR_WithModSuffixes byte_selectors <|>
                  pSrcCCX_WithSfx <|>
                  pSrcInt32
              pComplete [dst] [src]

            -- I2FP.F32.U32  R2, R24
            -- I2FP.F32.S32  R0, c0[0x190]
            OpI2FP -> pUnaryOp

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
                ([src0] ++ src12s ++ [src_ci1,src_ci2])

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
              pComplete [dst,dst_co] $ (src0:src12s ++ [src3_ci])

            -- IMMA.16816.U8.U8 R8, R2.reuse.ROW, R26.COL, R8 {!4,+2.W,+1.R,^3};
            OpIMMA -> do
              dst <- pDstR
              src0 <- pSymbol "," >> pSrcR_MMA
              src1 <- pSymbol "," >> pSrcR_MMA
              src2 <- pSymbol "," >> pSrcR
              pComplete [dst] [src0,src1,src2]

            --       IMNMX       R2,  R2, c[0x0][0x174], !PT {!4,Y}; // examples/sm_80/samples/boxFilter_kernel.sass:10553
            -- @P0   IMNMX.U32   R12, R8, R13, PT {!2,^3};
            --       IMNMX       R9,  R9, 0x20, !PT {!5,Y}; // examples/sm_80/samples/cdpQuadtree.sass:7464
            OpIMNMX -> pSelectOp

            -- ISETP.GT.U32.AND P0, PT, R6, 0xfd, PT {!4,Y};
            -- ISETP.GE.AND.EX  P0, PT, R7, c[0x0][0x16c], PT, P0 {!1};
            -- ISETP.LT.AND     P0, PT, R13, UR5, PT {!1}; // examples/sm_80/samples/binomialOptions_kernel.sass:2656
            --                      ^ I've never seen this
            --
            -- [71:68] = second predicate src (only on .EX)
            -- [72] = .EX (extended)
            -- [73] = .U32
            -- [75:74] = {AND,OR,XOR,INVALID3}
            -- [78:76] = {F,LT,EQ,LE,GT,NE,GE,T}
            -- [83:81] = first dst pred
            -- [86:84] = second dst pred
            -- [90:87] = first predicate src
            OpISETP -> do
              dst0 <- pDstP
              dst1 <- pCommaOrColon >> pDstP
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              src2 <- pSymbol "," >> pSrcP
              -- .EX has an extra predicate
              src3 <- P.option sRC_PT $ pSymbol "," >> pSrcP -- 0b0111 (PT) if not set
              pComplete [dst0,dst1] [src0,src1,src2,src3]

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
              src <- pXLdcSrcs (RegR <$> pSyntax) RegRZ
              pComplete [dst] [src]

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
            --   SM90
            --      LDGSTS.E.BYPASS.128          [R112+0x240], desc[UR10][R8.64] {!2,+2.R};
            --      LDGSTS.E.BYPASS.LTC128B.128  [R207+0x8080], [R6.64+UR28+0x80], P6 {!1,+2.R};
            --      LDGSTS.E.BYPASS.LTC128B.128  [R77+0x380], [R14.64+UR14], P1 {!1,+2.R};
            --
            -- [90:87] is the optional src predicate
            OpLDGSTS -> do
              src0_addr <- pLDST_Addrs op
              pSymbol ","
              src1_addr <- pLDST_Addrs op
              src_p <- P.option sRC_PT $ pSymbol "," >> pSrcP
              pComplete [] [src0_addr,src1_addr,src_p]

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
            -- [73]     .SX32 (sign extends the high part)
            -- [74]     .X (activates the carry-in predicate)
            -- [79:75]  the shift amount (0x0 to 0x1F)?
            -- [80]     .HI
            -- [84:81]  carry-out predicate (always enabled; PT defaults)
            -- [90:87]  carry-in predicate expression (defults to !PT)
            OpLEA -> do
              dst <- pDstR
              dst_co <- P.option dST_PT $ P.try $ pCommaOrColon >> pDstP
              --
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcX
              src2 <- P.option sRC_RZ $ P.try (pSymbol "," >> pSrcR)
              src3 <- pSymbol "," >> pSrcI8 -- the shift
              src4_ci <- P.option sRC_NPT $ P.try $ pSymbol "," >> pSrcP
              --
              pComplete
                [dst,dst_co]
                [src0,src1,src2,src3,src4_ci]

            -- LEPC R130 {!2,^4}
            OpLEPC -> do
              dst <- pDstR
              pComplete [dst] []

            --  LOP3.LUT             R15, R16, R15,         RZ, 0xfc, !PT {!2}; // synthetic old form
            --  LOP3.(s0|s1)         R15, R16, R15,         RZ, 0xfc, !PT {!1}; // examples/sm_75/samples\BlackScholes.sass:1220
            --  LOP3.(s0&s1)    P2,  RZ,  R35, 0x7fffffff,  RZ, 0xc0, !PT {!4,Y}; // examples/sm_75/samples\bilateral_kernel.sass:1557
            --  LOP3.LUT.PAND        R15 ...
            OpLOP3 -> do
              dst_p <- P.option dST_PT $ P.try $ (pDstP <* pCommaOrColon)
              dst_r <- pDstR
              --
              src0 <- pSymbol "," >> pSrcR
              src12s <- pSymbol "," >> pSrc_RX_XR
              src3_lut <- pLop3LutOptSrc
              src4 <- pSymbol "," >> pSrcP
              --
              pComplete [dst_p,dst_r] (src0:src12s ++ [src3_lut,src4])

            -- MATCH.ALL         R18, R34
            -- MATCH.ANY         R18, R34
            -- MATCH.ANY.U64     R18, R34
            -- MATCH.ALL     PT, R18, R34
            OpMATCH -> do
              dsts <- pDstsP_R
              src <- pSymbol "," >> pSrcR
              pComplete dsts [src]

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
              src1 <- P.option sRC_I32_0xF $ pSymbol "," >> pSrcI8
              pComplete [dst] $ [src0,src1]

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
              pComplete [dst] [src0,src1]

            -- PLOP3.((s0|s1)&s2) P0, PT, P0, P1, PT, 0xa8, 0x0 {!13,Y};
            -- PLOP3.(s0&s1&s2)   P0, PT, PT, PT, UP0, 0x80, 0x0 {!1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:9440
            OpPLOP3 -> do
              dst0 <- pDstP
              dst1 <- pSymbol "," >> pDstP
              --
              src0 <- pSymbol "," >> pSrcP
              src1 <- pSymbol "," >> (pSrcP <|> pSrcUP)
              src2 <- pSymbol "," >>  (pSrcP <|> pSrcUP)
              src3_lut <- pLop3LutOptSrc -- [??]
              src4_unk <- pSymbol "," >> pSrcI8 -- bits [23:16]
              --
              pComplete [dst0,dst1] [src0,src1,src2,src3_lut,src4_unk]

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
              pComplete [dst] (src0:src12s)

            -- QSPC.E.S   P1, RZ, [R12] {!2,+1.W};
            -- QSPC.E.G   P1, RZ, [R12] {!2,+1.W};
            --
            -- [74] = {.G,.S}
            -- [84:81]  = dst predicate
            OpQSPC -> do
              dsts <- pDstsP_R
              src_addr <- pSymbol "," >> pLDST_Addrs op
              pComplete dsts [src_addr]

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
              pComplete [] [src0, src1]

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
              pComplete [dst_p,dst] [src]

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
            op | op`elem`[OpRED,OpREDG] -> do
              src0_addr <- pLDST_Addrs op
              pSymbol ","
              src1 <- pSrcR
              pComplete [] [src0_addr,src1]

            -- REDUX.SUM.S32     UR6, R4 {!2,+1.W};
            -- REDUX.MAX.S32     UR6, R3 {!1,+1.W};
            --
            -- [73] = {[default], .S32}
            -- [80:78] = {[default AND?], .OR, .XOR, .SUM, .MIN, .MAX, INVALID6, INVALID7}
            OpREDUX -> do
              dst <- pDstUR <* pSymbol ","
              src0 <- pSrcR
              pComplete [dst] [src0]

            -- RET.ABS.NODEC    R20 0x0 {!5,^1};
            -- RET.REL.NODEC    R2 `(_Z27compute_bf16gemm_async_copyPK13__nv_bfloat16S1_PKfPfff) {!5}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:9638
            --             --
            -- [85] = {.REL, .ABS}
            -- [86] = {[default], .NODEC}
            -- [90:87] = optional predicate source
            --
            -- SM90:
            --   RET.REL.NODEC  R14 `(_Z15integrateBodiesIdEvPN4vec4IT_E4TypeES4_S4_jjffi) {!5}
            OpRET -> do
              let pSm80 = do
                    src0 <- P.option sRC_PT $ P.try $ pSrcP <* pSymbol ","
                    src1 <- pSrcR
                    -- no comma
                    src2 <- pSrcLbl pc op
                    pComplete [] [src0,src1,src2]
              let pSm90 = do
                    let box c = [c]
                    src_rc <- P.option [] $
                      box <$> (P.try pSrcR <|> P.try (pSrcCCX op))
                    src_lbl <- P.option [] $ do
                      P.option "" (pSymbol ",")
                      box <$> pSrcLbl pc op
                    pComplete [] (src_rc ++ src_lbl)
              pSm90

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
              pComplete [dst0,dst1] [src0,src1,src2]

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
              src0_addr <- pLDST_Addrs op <* pSymbol ","
              src1 <- pSrcR <* pSymbol ","
              src2 <- pSrcImmNonBranch32 op
              src3s <- P.option [] $ box <$> (pSymbol "," >> pSrcImmNonBranch32 op)
              pComplete [] (src0_addr:src1:src2:src3s)


            -- SM90:
            --     SYNCS.EXCH.64                   URZ, [UR4+0x18], UR8  {!1,+2.W,+2.R}; // 0002620008000100`00001808043F75B2 examples/sm_90/libs/cublasLt64_12/Elf-1510.sm_90.sass:91502
            -- [73:72] {0,.EXCH,1,2}
            --
            --      SYNCS.PHASECHK.TRANS64.TRYWAIT  P0, [UR32+0x28],  RZ   {!2,+2.W}; // 000E640008000160`000028FFFF0075A7 examples/sm_90/libs/cublasLt64_12/Elf-1510.sm_90.sass:91569
            --      SYNCS.PHASECHK.TRANS64.TRYWAIT  P0, [R2+URZ],     R29  {!2,+4.W}; // 000EE4000800017F`0000001D020075A7 examples/sm_90/samples/3_CUDA_Features/globalToShmemAsyncCopy/globalToShmemAsyncCopy.sass:8646
            --
            --      SYNCS.CCTL.IV                       [R8+URZ+0x10]      {!8,+1.W,^1}; // 001E30000800003F`00001000080079B1 examples/sm_90/samples/3_CUDA_Features/globalToShmemAsyncCopy/globalToShmemAsyncCopy.sass:8393
            --
            --      SYNCS.ARRIVE.TRANS64.A1T0       RZ, [UR30],       RZ   {!2,^3};   // 004FE4000810001E`000000FFFFFF79A7 examples/sm_90/libs/cublasLt64_12/Elf-1510.sm_90.sass:91581
            --      SYNCS.ARRIVE.TRANS64.ART0       RZ, [R16+URZ],    R3   {!1,+1.R}; // 0001E2000850003F`0000000310FF79A7 examples/sm_90/samples/3_CUDA_Features/globalToShmemAsyncCopy/globalToShmemAsyncCopy.sass:8284
            -- @P6  SYNCS.ARRIVE.TRANS64.RED.A1T0   RZ, [R0+URZ],     RZ   {!3,+2.R}; // 0003E6000810043F`000000FF00FF69A7 examples/sm_90/libs/cublasLt64_12/Elf-1510.sm_90.sass:105826
            -- @!P0 SYNCS.ARRIVE.TRANS64.RED.A0T1   RZ, [UR4], RZ          {!1};      // 000FE20008200404`000000FFFFFF89A7 examples/sm_90/samples/3_CUDA_Features/globalToShmemAsyncCopy/globalToShmemAsyncCopy.sass:8268
            OpSYNCS -> do
              let pImmTerm :: PI Int
                  pImmTerm = do
                      sgn <-
                        (pSymbol "+" >> return id) <|>
                          (pSymbol "-" >> return negate)
                      sgn . fromIntegral <$> pIntImm32

              let pExch64 = do
                    dst <- pDstUR <* pSymbol ","
                    --
                    pSymbol "["
                    base <- pLabel "ureg" pSyntax :: PI UR
                    immoff <- P.option 0 pImmTerm
                    let src_addr = SrcAddr (RZ,msEmpty) base immoff
                    pSymbol "]" >> pSymbol ","
                    --
                    src_data <- pSrcUR
                    --
                    pComplete [dst] [src_addr,src_data]

              let pArrv = do
                    -- SYNCS.ARRIVE.TRANS64.A1T0  RZ, [UR30], RZ  {!2,^3};
                    dst <- pDstR <* pSymbol ","
                    src_addr <- pLDST_Addrs op <* pSymbol ","
                    src_data <- pSrcR
                    pComplete [dst] [src_addr,src_data]

              let pPhaseChk = do
                    -- SYNCS.PHASECHK.TRANS64.TRYWAIT  P0, [UR32+0x28], RZ   {!2,+2.W};
                    dst <- pDstP <* pSymbol ","
                    src_addr <- pLDST_Addrs op <* pSymbol ","
                    src_data <- pSrcR
                    pComplete [dst] [src_addr,src_data]

              let pCctl = do
                    -- SYNCS.CCTL.IV [R8+URZ+0x10] {!8,+1.W,^1};
                    src_addr <- pLDST_Addrs op
                    pComplete [] [src_addr]

              if InstOptEXCH`iosElem`ios then pExch64
                else if InstOptARRIVE`iosElem`ios then pArrv
                else if InstOptPHASECHK`iosElem`ios then pPhaseChk
                else if InstOptCCTL`iosElem`ios then pCctl
                else pSemanticError loc "unsupported SYNCS... op"



            -- TEX.SCR.LL.NODEP R14, R16, R14, R20, 0x0, 0x5a, 2D      {!1,Y}
            -- TEX.SCR.LL       RZ,  R7,  R6,  R18, 0x0, 0x58, 2D, 0x1 {!3,Y,+6.W};
            -- TEX.SCR.B.LL     R4,  R6,  R12, R4,             2D      {!1,Y,+6.W,^6};
            --
            --   src3a imm is [58:54]
            --   src3b imm is 14b [53:40]
            --   src4 surface shape selector is [63:60]
            --   src5 0x1
            OpTEX -> do
              dst0 <- pDstR
              dst1 <- pSymbol "," >> pDstR
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcR
              pTextureOp [dst0,dst1] [src0,src1]

            -- TLD.SCR.LZ       RZ, R6, R14, 0x0, 0x66, 1D, 0x3 {!1,Y,+6.W};
            OpTLD -> do
              dst0 <- pDstR
              dst1 <- pDstR
              src0 <- pSymbol "," >> pSrcR
              src1 <- pSymbol "," >> pSrcR
              pTextureOp [dst0,dst1] [src0,src1]

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
              pComplete (dst:dst_cos) (src0:src1:src2:src_cis)

            --                  DST  CO0    SRC0  SRC1          SRC2   CI0
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
              src2 <- pSymbol "," >> pSrcURI -- can be UIU or UUI
              src_ci <-
                P.option sRC_UPF $ do
                  pSymbol "," >> pSrcUP
              --
              pComplete
                [dst,dst_co]
                [src0,src1,src2,src_ci]

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
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUP
              -- .EX has an extra predicate
              src3 <- P.option sRC_UPT $ pSymbol "," >> pSrcUP -- 0b0111 (PT) if not set
              pComplete [dst0,dst1] [src0,src1,src2,src3]

            -- ULDC       UR5, c[0x0][0x160] {!1}; // examples/sm_80/libs/cublas64_11/Program.224.sm_80.sass:54823
            -- ULDC.64    UR4, c[0x0][0x118] {!2}; // examples/sm_80/libs/cublas64_11/Program.224.sm_80.sass:56237
            -- ULDC.64    UR4, c0[0x118]
            OpULDC -> do
              dst <- pDstUR
              src <- pSymbol "," >> pXLdcSrcs (RegUR <$> pSyntax) RegURZ
              pComplete [dst] [src]

            --                  DST  CO   SRC0  SRC1       SRC2  shift CI
            -- ULEA             UR15,     UR16, UR10,            0x3       {!4,Y,^1};
            -- ULEA             UR7, UP0, UR6,  UR17,            0x3       {!2};
            -- ULEA.HI.X.SX32   UR4,      UR4,  UR12,            0x1,  UP0 {!1};
            -- ULEA.HI          UR4,      UR4,  0x7fffffff, URZ, 0x1f      {!2};
            -- ULEA             UR5,      UR8,  0xfffffffc,      0x2       {!1};
            -- ULEA.HI.SX32     UR6,      UR6,  0xfffffffc,      0x1b      {!1};
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
              pComplete [dst,dst_p] [src0,src1,src2,src3_sh,src4_ci]

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
            -- ULOP3.(s0&s1)         UR5,  UR5, 0xffffff,   URZ, 0xc0, !UPT  {!6,Y,^1};  /* 001FCC000F8EC03F`00FFFFFF05057892 */
            OpULOP3 -> do
              dst_p <- P.option dST_UPT $ pDstUP <* pSymbol ","
              dst <- pDstUR
              --
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              src3_lut <- pLop3LutOptSrc
              src4_pr <- pSymbol "," >> pSrcUP
              --
              pComplete [dst_p,dst] [src0,src1,src2,src3_lut,src4_pr]

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
              src3_lut <- pSymbol "," >> pLop3LutOptSrc -- [??]
              src4_unk <- pSymbol "," >> pSrcI8 -- bits [23:16]
              --
              pComplete [dst0,dst1] [src0,src1,src2,src3_lut,src4_unk]

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
              pComplete [dst] [src0,src1,src2]

            -- USEL   UR4,  UR4,  UR6, !UP0 {!2}; // examples/sm_80/samples/cdpAdvancedQuicksort.sass:4908
            -- USEL   UR4,  UR4,  0x6, !UP0 {!2};
            OpUSEL -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2_p <- pSymbol "," >> pSrcUP
              pComplete [dst] [src0,src1,src2_p]

            -- USGXT       UR4, UR4, 0x18 {!6,Y,^1}; // examples/sm_80/samples/convolutionTexture.sass:984
            -- USGXT       UR4, UR5, UR24 {!3,Y,^2};
            -- USGXT.W     UR4, UR5, 0x18 {!3,Y,^2};
            OpUSGXT -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              pComplete [dst] [src0,src1]

            -- USHF.L.U32     UR6, UR5,  0x3, URZ {!1}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:7765
            -- USHF.R.S32.HI  UR8, URZ, 0x1f, UR7 {!2,Y}; // examples/sm_80/samples/bf16TensorCoreGemm.sass:8324
            OpUSHF -> do
              dst <- pDstUR
              src0 <- pSymbol "," >> pSrcUR
              src1 <- pSymbol "," >> pSrcURI
              src2 <- pSymbol "," >> pSrcUR
              pComplete [dst] [src0,src1,src2]

            -- VIMNMX.U32  R12,  R4,     UR14,   PT               {!3,Y};
            OpVIMNMX -> pSelectOp

            --                DST   DST?
            --    VOTE.ANY    R5,   PT,  P1 {!4,Y};
            --    VOTE.ALL    R6,   PT,  PT {!1,^1};
            OpVOTE -> do
              dst <- pDstR
              dst_p <- pSymbol "," >> pDstP -- dst I think [83:81]
              src0 <- pSymbol "," >> pSrcP -- [90:87]
              pComplete [dst,dst_p] [src0]

            --                DST   ??    ..
            --    VOTEU.ANY   UR36, UPT,  PT {!1};   // examples/sm_80/samples/cdpAdvancedQuicksort.sass:3566
            --    VOTEU.ALL   UR4,  UPT,  PT {!1};    // examples/sm_80/samples/globalToShmemAsyncCopy.sass:4529
            --    VOTEU.ANY   UR4,  UPT, !P1 {!12,Y}; // examples/sm_80/samples/reduction_kernel.sass:118610
            OpVOTEU -> do
              dst <- pDstUR
              dst_p <- pSymbol "," >> pDstUP -- dst I think [83:81]
              src0 <- pSymbol "," >> pSrcP -- [90:87]
              pComplete [dst,dst_p] [src0]

            --       WARPSYNC                0xffffffff {!4};
            -- @!P2  WARPSYNC                0xffffffff {!4};
            --       WARPSYNC.EXCLUSIVE      R2 {!4};
            --       WARPSYNC                R19 {!4};
            -- (synthetic)
            --       WARPSYNC           !P4, 0xffffffff
            --
            -- SM90
            --       WARPSYNC.ALL  {!1}; // 000FE20003800000`0000000000007948 examples/sm_90/samples/0_Introduction/mergeSort/bitonic.sass:3562
            --       WARPSYNC.COLLECTIVE  R13, `(.L_x_158) {!1};
            --       WARPSYNC.COLLECTIVE.ALL  `(.L_x_59)                {!1}; // examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:3545 000FE20003C00000`0000000000147948
            --
            -- [86] .EXCLUSIVE
            -- [90:87] predicate source (default to PT)
            OpWARPSYNC -> do
              let pSm80 = do
                    src0 <- P.option sRC_PT $ pSrcP <* pSymbol ","
                    src1s <- P.option [] $ box <$> pSrcX
                    src2s <- P.option [] $ box <$> pSrcX
                    pComplete [] (src0:(src1s++src2s))
              let pSm90 = do
                    src_reg <-
                      P.option [] $ do
                        r <- pSrcR
                        return [r]
                    P.option () $ pSymbol "," >> return ()
                    src_lbl <-
                      P.option [] $ do
                        lbl <- pSrcLbl pc op
                        return [lbl]
                    pComplete [] (src_reg ++ src_lbl)
              pSm90

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
              pComplete [] [src0]

            -- TODO: disable and force everything through a pre-chosen path
            _ -> pSemanticError loc $ "unmatched op (InstParser.hs case)"

        -- up to two predicate sources
        -- pOptPreds = do
        --   P.option [] $ do
        --     p_src0 <- pSymbol "," >> pPredSrcP
        --     P.option [p_src0] $ do
        --       p_src1 <- pSymbol "," >> pPredSrcP
        --       return [p_src0,p_src1]

pCommaOrColon :: PI ()
pCommaOrColon = (pSymbol "," <|> pSymbol ":") >> return ()

pPred :: PI Pred
pPred = pLabel "predication" $ do
    pSymbol "@"
    sign <- P.option PredPOS $ pSymbol "!" >> return PredNEG
    tryP sign <|> tryUP sign
  where tryP sign = PredP sign <$> pSyntax
        tryUP sign = PredUP sign <$> pSyntax


pOp :: PI Op
pOp = pWithLoc $ \loc -> do
  op_str <- pIdentifier
  case reads ("Op"++op_str) of
    [(op,"")] -> return op
    _ -> pSemanticError loc $ op_str ++ ": unrecognized mnemonic"


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
          io <- (\x -> [x]) <$> pSyntax
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

