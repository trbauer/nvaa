module NVT.Analysis.Deps where

import NVT.Bits
import NVT.Encoders.Codable
import NVT.IR

import Data.Bits
import Data.List
import Data.Word
import Debug.Trace


data Deps =
  Deps {
    dR :: Word256 -- R0, R1 ... R254
  , dP :: !Word32 -- P0, P1 .. P6
  , dUR :: !Word64 -- UR0, UR1 .. UR62
  , dUP :: !Word32 -- UP0, UP1 .. UP6
  , dB :: !Word32 -- B0 .. B16
  } deriving (Show,Eq)

dEmpty :: Deps
dEmpty = Deps w256_zero 0 0 0 0


dUnion :: Deps -> Deps -> Deps
dUnion d1 d2 =
  Deps {
    dR = dR d1 .|. dR d2
  , dP = dP d1 .|. dP d2
  , dUR = dUR d1 .|. dUR d2
  , dUP = dUP d1 .|. dUP d2
  , dB = dB d1 .|. dB d2
  }



dShow :: Deps -> String
dShow d = "{" ++ deps ++  "}"
  where deps :: String
        deps = intercalate ";" $
          filter (not . null) [
              dShowRegs (dR d) RZ
            , dShowRegs (dP d) PT
            , dShowRegs (dUR d) URZ
            , dShowRegs (dUP d) UPT
            , dShowRegs (dB d) B0
            ]


dShowRegs :: (FiniteBits b,Codeable c,Syntax c) => b -> c -> String
dShowRegs bs dummy = intercalate "," (map toC (bitElems bs))
  where toC i =
          case decode (fromIntegral i) of
            Left err -> show i ++ "?"
            Right c -> format (c`asTypeOf`dummy)

bitElems :: FiniteBits b => b -> [Int]
bitElems fbs = concatMap checkBit [0 .. finiteBitSize fbs - 1]
  where checkBit ix
          | fbs`testBit`ix = [ix]
          | otherwise = []

dReg :: Reg -> Deps
dReg = dRegs 1

dRegs :: Int -> Reg -> Deps
dRegs n r =
    case r of
      RegR RZ -> dEmpty
      RegR r -> addRegsR n r
      --
      RegP PT -> dEmpty
      RegP pr -> dEmpty{dP = bit (fromEnum pr)}
      --
      RegUR URZ -> dEmpty
      RegUR ur -> addRegsUR n ur
      --
      RegUP UPT -> dEmpty
      RegUP pr -> dEmpty{dUP = bit (fromEnum pr)}
      --
      RegB br -> dEmpty{dB = bit (fromEnum br)}
      --
      _ -> dEmpty
  where addRegsR :: Int -> R -> Deps
        addRegsR i RZ = dEmpty -- technically an error
        addRegsR 1 r = dEmpty{dR = bit (fromEnum r)}
        addRegsR i r = dReg (RegR r)`dUnion`(addRegsR (i-1) (succ r))

        addRegsUR :: Int -> UR -> Deps
        addRegsUR _ URZ = dEmpty -- technically an error
        addRegsUR 1 ur = dEmpty{dUR = bit (fromEnum ur)}
        addRegsUR i ur = dReg (RegUR ur)`dUnion`(addRegsUR (i-1) (succ ur))



----------------------------------------------------------
dDsts :: Inst -> Deps
dDsts = fst . dInstDeps
dSrcs :: Inst -> Deps
dSrcs = snd . dInstDeps

dInstDeps :: Inst -> (Deps,Deps)
dInstDeps i = (dsts,srcs)
  where dsts = foldl' (\a d -> a`dUnion`dstDeps d) dEmpty (iDsts i)
        srcs = foldl' (\a d -> a`dUnion`d) pred_deps (pred_deps:map srcDeps (zip [0..] (iSrcs i)))
        pred_deps =
          case iPredication i of
            PredP _ pr -> dRegs 1 (RegP pr)
            PredUP _ upr -> dRegs 1 (RegUP upr)

        dstDeps :: Dst -> Deps
        dstDeps dst =
          case dst of
            Dst _ reg@(RegR _)
              | is_dsts_64 -> dRegs 2 reg
              | oIsLD (iOp i) || oIsAT (iOp i) -> handleLdStAtData reg
            Dst _ reg@(RegUR _)
              | oIsLD (iOp i) || oIsAT (iOp i) -> handleLdStAtData reg
            Dst _ reg
              | is_dsts_64 -> dRegs 2 reg
            Dst _ reg -> dReg reg

        handleLdStAtData reg
            -- LD.64  DST, [..]
            -- ST.64 [..], DATA
            | iHasAnyOpt [InstOpt64] = dRegs 2 reg
            -- LD.128 DST, [..]
            -- ST.128 [..], DATA
            | iHasAnyOpt [InstOpt128] = dRegs 4 reg
            -- LD     DST, [..] or LD.S8 DST, [..]
            -- ST     [..], DATA
            | otherwise = dRegs 1 reg

        srcDeps :: (Int,Src) -> Deps
        srcDeps (src_ix,src) =
          case src of
            SrcReg _ reg@(RegR _)
              | oIsST (iOp i) && src_ix == length (iSrcs i) - 1 -> handleLdStAtData reg
              | is_srcs_64b -> dRegs 2 reg
              | is_ld_st_at && iHasInstOpt InstOptE i -> dRegs 2 reg
              where is_ld_st_at = oIsLD (iOp i) || oIsST (iOp i) || oIsAT (iOp i)
            SrcReg _ reg@(RegUR _)
              | oIsST (iOp i) && src_ix == length (iSrcs i) - 1 -> handleLdStAtData reg
              | is_srcs_64b -> dRegs 2 reg
            SrcReg _ r -> dReg r
            _ -> dEmpty

        iHasAnyOpt :: [InstOpt] -> Bool
        iHasAnyOpt = any (flip iHasInstOpt i)

          -- IMAD.WIDE or UIMAD.WIDE
        is_srcs_64b :: Bool
        is_srcs_64b =
          oIsD (iOp i) && iOp i /= OpDSETP || is_cvt_from_64

        is_dsts_64 :: Bool
        is_dsts_64 =
          oIsD (iOp i) && iOp i /= OpDSETP ||
          iHasInstOpt InstOptWIDE i || is_cvt_to_64

        is_cvt_to_64 :: Bool
        is_cvt_to_64 =
          case iOp i of
            -- OpI2I -> ... uses funnel shift or zext
            OpI2F -> iHasAnyOpt
                      [
                        InstOptF64, InstOptF64_U16, InstOptF64_S16
                      , InstOptS64, InstOptU64
                      ]
            OpF2I -> iHasAnyOpt
                      [
                        InstOptS64, InstOptS64_F64
                      , InstOptU64, InstOptU64_F64
                      ]
            OpF2F -> iHasAnyOpt [InstOptF64_F32]
            _ -> False

        is_cvt_from_64 :: Bool
        is_cvt_from_64 =
          case iOp i of
            OpF2F -> iHasAnyOpt [InstOptF32_F64]
            OpF2I -> iHasAnyOpt [InstOptS64_F64,InstOptU64_F64]
            OpI2F -> iHasAnyOpt [InstOptS64,InstOptU64]
            _ -> False



-- cases
--  IMAD.WIDE
--  D*** uses 2
--  I2F.64.*
--  I2I.64.*
--  F2F.*
--  F2I.*


