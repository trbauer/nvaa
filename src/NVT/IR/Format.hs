module NVT.IR.Format where

import NVT.IR.Op
import NVT.IR.Syntax
import NVT.IR.Types
import NVT.Floats
import NVT.Lop3

import Data.Bits
import Data.Int
import Data.List
import Text.Printf


instance Syntax Inst where
  format i = fmtInstWithImmFormatter (defaultImmFormatter (iOp i)) i


type ImmFormatter = Imm -> String
--
defaultImmFormatter :: Op -> ImmFormatter
defaultImmFormatter op imm =
  case imm of
    Imm8 u8 -> printf "0x%02X" u8
    Imm32 u32
      -- e.g. FADD
      | oIsFP op -> printf "%f" (bitsToFloat u32)
      -- e.g. IMAD
      | oIsI op && s32 < 0 -> printf "-0x%08X" (negate s32)
      -- everything else
      | otherwise -> printf "0x%08X" u32
      where s32 = fromIntegral u32 :: Int32
    Imm49 i64
      | i64 < 0 -> "-" ++ printf "0x%013X" (-i64)
      | otherwise -> printf "0x%013X" i64

fmtInstWithImmFormatter :: ImmFormatter -> Inst -> String
fmtInstWithImmFormatter fmt_imm i =
    pred ++ op_str ++ " " ++ opnds_str ++ depinfo_str ++ ";"
  where pred :: String
        pred = padR 5 pred_str
          where pred_str
                  | iPredication i`elem`[pred_pt,pred_upt] = ""
                  | otherwise = format (iPredication i)
        op_str :: String
        op_str = padR 12 (format (iOp i) ++ inst_opts)
          where inst_opts =
                  synthetic_tokens ++
                  concatMap (\io -> "." ++ format io) (iosToList (iOptions i))

        -- This is annoying, but for exact matching of nvdisasm, we have to
        -- print helpful hints on IMADs that are used for identity operations.
        -- Personally, I think the SHL is particularly distracting since the
        -- shift value isn't represented explicitly.
        --  * .MOV  .. X            <-  RZ*Y + X
        --                          or   X*1 + RZ
        --                          or   1*X + RZ
        --  * .IADD .. X, Y         <-  X*1 + Y
        --  * .SHL  K, X            <-  X*(2^K) + RZ
        synthetic_tokens :: String
        synthetic_tokens
          | iOp i == OpIMAD =
            case iSrcs i of
              (src0:src1:src2:_)
                | isRZ src0 || isRZ src1 -> ".MOV"   -- RZ*X + Y = MOV Y
                | isEqI 1 src1 && isRZ src2 -> ".MOV" -- X*1 + 0 = MOV X
                | isR src0 && isEqI 1 src1 -> ".IADD"
                | isPow2Gt1 src1 && isRZ src2 -> ".SHL"
                where isRZ SrcRZ = True
                      isRZ _ = False
                      isR (Src_R _ _) = True
                      isR _ = False
                      isEqI k (SrcI32 x) = x == k
                      isEqI _ _ = False
                      isPow2Gt1 (SrcI32 imm) =
                        imm > 1 && (imm .&. (imm - 1)) == 0
                      isPow2Gt1 _ = False
              _ -> ""
          -- unless I choose to use the LUT as syntax
          | iOp i == OpLOP3 =
            case iSrcs i of
              [s0,s1,s2,SrcI32 imm,s4] ->
                ".(" ++ fmtLop3 (fromIntegral imm) ++ ")"
              _ -> ".LUT"
          | iOp i == OpPLOP3 = ".LUT"
          | otherwise = ""

        opnds_str :: String
        opnds_str
          | oIsLD (iOp i) = intercalate ":" dsts ++ ", " ++ fmtAddrs (iSrcs i)
          | oIsST (iOp i) = st_src_addr ++ ", " ++ st_src_data
          | otherwise = intercalate ", " (dsts ++ srcs)
          where (st_src_addr,st_src_data) =
                  case splitAt 3 (iSrcs i) of
                    (src_addrs,[src_dat]) ->
                      (fmtAddrs src_addrs,formatSrcWithOpts fmt_imm src_dat)
                    _ -> ("?","?")

                -- we attempt to copy nvdisasm here (at least for LDG/LDS);
                -- specifically, we omit default values except when all are default;
                -- then we emit RZ only
                fmtAddrs :: [Src] -> String
                fmtAddrs srcs = "[" ++ intercalate "+" (concatMap fmtSrc srcs) ++ "]"
                  where opIsDefault :: Src -> Bool
                        opIsDefault SrcRZ = True
                        opIsDefault SrcURZ = True
                        opIsDefault (SrcI32 0) = True
                        opIsDefault _ = False

                        all_default = all opIsDefault srcs

                        fmtSrc src =
                          case src of
                            SrcRZ
                              | not (opIsDefault src) || all_default -> [formatSrcWithOpts fmt_imm src]
                            _ -> [formatSrcWithOpts fmt_imm src]

                dsts = map format visible_dsts

                visible_dsts
                  | iOp i == OpLOP3 =
                    case iDsts i of
                  --    [DstP PT, dst_r] -> [dst_r]
                      _ -> iDsts i
                  | otherwise = iDsts i

                srcs = map (uncurry fmtSrc) (zip [1..] visible_srcs)
                  where fmtSrc :: Int -> Src -> String
                        fmtSrc src_ix src =
                          case (src_ix, iOp i,src) of
                            _ -> formatSrcWithOpts fmt_imm src

                visible_srcs
                  | iOp i == OpIADD3 =
                    case iSrcs i of
                      -- (SrcP False PT:SrcP False PT:sfx) -> sfx
                      -- (SrcP False PT:sfx) -> sfx
                      SrcPT:SrcPT:sfx -> sfx
                      SrcPT:sfx -> sfx
                      sfx -> sfx
                  | iOp i == OpLOP3 && not (".LUT"`isInfixOf`synthetic_tokens) =
                    -- for LOP3 if we used an expression then we can drop src3
                    -- e.g. LOP3.(...)
                    case iSrcs i of
                      [s0,s1,s2,SrcI32 imm,s4] -> [s0,s1,s2,s4]
                      _ -> iSrcs i
                  | iOp i == OpMOV =
                    case iSrcs i of
                      [src0, SrcI32 0xF] -> [src0] -- hidden parameter for MOV
                      _ -> iSrcs i
                  | otherwise = iSrcs i

        depinfo_str = if null d then "" else (" " ++ d)
          where d = format (iDepInfo i)

-------------------------------------------------------------------------------

instance Syntax Src where
  format = formatSrcWithOpts (defaultImmFormatter OpNOP)

formatSrcWithOpts :: ImmFormatter -> Src -> String
formatSrcWithOpts fmt_imm src =
  case src of
    SrcReg ms r -> msDecorate ms (format r)
    SrcCon ms six soff -> msDecorate ms (format six ++ printf "[0x%X]" soff)
    SrcImm i -> fmt_imm i
    SrcTex to -> format to

