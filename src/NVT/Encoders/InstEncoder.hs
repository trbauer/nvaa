module NVT.Encoders.InstEncoder where

import NVT.Bits
import NVT.Loc
import NVT.Diagnostic
import NVT.IR
import NVT.Encoders.Codable

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Debug.Trace
import Text.Printf
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Trans.Except as CME
import Data.Functor.Identity

type Field = (String,Int,Int)

fName :: Field -> String
fName (s,_,_) = s

fOffset :: Field -> Int
fOffset (_,off,_) = off

fLength :: Field -> Int
fLength (_,_,len) = len


data ESt =
  ESt {
    esLoc :: !Loc
  , esInst :: !Word128
  , esMask :: !Word128
  , esFields :: ![(Field,Word64)]
  , esWarnings :: ![Diagnostic]
  } deriving Show


mkESt :: Loc -> ESt
mkESt loc = ESt loc (Word128 0 0) (Word128 0 0) [] []

type E = CMS.StateT ESt (CME.Except Diagnostic)

runE :: Loc -> E () -> Either Diagnostic (Word128,[Diagnostic])
runE loc encode =  (\(x,y,_) -> (x,y)) <$> runDbgE loc encode
runDbgE :: Loc -> E () -> Either Diagnostic (Word128,[Diagnostic],[(Field,Word64)])
runDbgE loc encode =
  case CME.runExcept (CMS.runStateT encode (mkESt loc)) of
    Left d_err -> Left d_err{dLoc = loc} -- exception
    Right (_,es) ->
      Right (
          esInst es
        , reverse (esWarnings es)
        , sortOn (fOffset . fst) (esFields es)
        )

-- eFatal :: (MonadTrans t, Monad m) => Diagnostic -> t (CME.ExceptT Diagnostic m) a
eFatal :: String -> E ()
eFatal msg = do
  loc <- CMS.gets esLoc
  lift (CME.throwE (Diagnostic loc msg)) -- we patch in the location later
eFatalF :: Field -> String -> E ()
eFatalF f msg = eFatal (fName f ++ ": " ++ msg)

eWarning :: String -> E ()
eWarning msg = CMS.modify $
  \es -> es{esWarnings = Diagnostic (esLoc es) msg:esWarnings es}

eField :: Field -> Word64 -> E ()
eField f val = do
  es <- CMS.get
  let mask
        | fLength f == 64 = 0xFFFFFFFFFFFFFFFF
        | otherwise = (1`shiftL`fLength f) - 1
  unless (val <= mask) $
    eFatal $ "field value overflows " ++ fName f
  let mask_val = getField128 (fOffset f) (fLength f) (esMask es)
  when ((mask_val .&. mask) /= 0) $
    eFatal $ "field overlaps another field already set" -- TODO: find it
  let new_mask = putField128 (fOffset f) (fLength f) mask (esMask es)
  let new_inst = putField128 (fOffset f) (fLength f) val  (esInst es)
  CMS.modify $ \es -> es{esInst = new_inst, esMask = new_mask, esFields = (f,val):esFields es}


eTrace :: String -> E ()
eTrace s = trace s $ return ()

eFieldE :: Field -> Either String Word64 -> E ()
eFieldE f e =
  case e of
    Left err -> eFatal (fName f ++ ": " ++ err)
    Right val -> eField f val

eEncode :: Codeable e => Field -> e -> E ()
eEncode f = eFieldE f . encode

eFieldS32 :: Field -> Int64 -> E ()
eFieldS32 = eFieldSignedImm 32
eFieldSignedImm :: Int -> Field -> Int64 -> E ()
eFieldSignedImm bits f val
  | val >= -2^(bits-1) && val < 2^(bits-1) = eField f (field_mask .&. w_val)
--  | high_bits /= 0 && high_bits /= 0xFFFFFFFF00000000 = eField f (field_mask .&. w_val)
  | otherwise = eFatalF f "value is out of bounds"
  where w_val = fromIntegral val :: Word64
        field_mask
          | fLength f == 64 = 0xFFFFFFFFFFFFFFFF
          | otherwise = (1`shiftL`fLength f) - 1

        high_mask = complement field_mask
        high_bits = high_mask .&. w_val

-------------------------------------------------------------------------------

runInstEncoder :: Inst -> Either Diagnostic (Word128,[Diagnostic])
runInstEncoder i = runE (iLoc i) (eInst i)
runInstDbgEncoder :: Inst -> Either Diagnostic (Word128,[Diagnostic],[(Field,Word64)])
runInstDbgEncoder i = runDbgE (iLoc i) (eInst i)

eInst :: Inst -> E ()
eInst i = enc
  where op = iOp i

        enc = do
          case oMnemonic op of
            "MOV" -> eMov
            s -> eFatal $ "unsupported operation"
          eDepInfo (iDepInfo i)

        ePredication :: E ()
        ePredication =
          case iPredication i of
            PredNONE -> ePredicationAt fPREDICATION False PT
            PredP sign pr -> ePredicationAt fPREDICATION sign pr
            PredUP sign pr -> ePredicationAt fPREDICATION sign pr

        ePredicationAt :: Codeable p => Field -> Bool -> p -> E ()
        ePredicationAt f sign pr =
          case encode pr of
            Left e -> eFatalF f "INTERNAL ERROR: invalid predicate register"
            Right reg_bits -> eField f (sign_bit .|. reg_bits)
              where sign_bit = if sign then 0x8 else 0x0

        eDepInfo :: DepInfo -> E ()
        eDepInfo di = do
          eEncode fDEPINFO_STALLS (diStalls di)
          eEncode fDEPINFO_YIELD (not (diYield di)) -- yield is inverted
          let eBarAlloc f mb =
                case mb of
                  Nothing -> eField f 7 -- 0x7 means alloc none
                  Just bi
                    | bi <= 0 || bi > 7 -> eFatalF f "invalid barrier index"
                    | otherwise -> eField f (fromIntegral (bi - 1))
          eBarAlloc fDEPINFO_WRBAR (diAllocWr di)
          eBarAlloc fDEPINFO_RDBAR (diAllocRd di)
          eEncode fDEPINFO_WMASK (diWaitSet di)

        eDstRegR :: E ()
        eDstRegR =
          case iDsts i of
            [DstR r] -> eEncode fDSTREG r
            [_] -> eFatal "wrong kind of destination operand"
            _ -> eFatal "wrong number of destination operands"

        ensureNoNegAbs :: Bool -> Bool -> E ()
        ensureNoNegAbs neg abs = do
          when neg $
            eFatal "unary instructions don't support src negation modifier"
          when abs $
            eFatal "unary instructions don't support src aboslute-value modifier"

        -- 1 = R, 4 = IMM, 5 = const
        eUnrSrcs :: [Src] -> E ()
        eUnrSrcs srcs =
          case srcs of
            [SrcR neg abs reu reg] -> do
              eField fREGFILE 1
              eEncode fSRCREG reg
              ensureNoNegAbs neg abs
              eEncode fSRC0_REUSE reu
            [SrcC neg abs six soff] -> do
              eField fREGFILE 5
              eField fSRCCIX  (fromIntegral six)
              eField fSRCCOFF (fromIntegral soff `div` 4)
              ensureNoNegAbs neg abs
            [SrcI imm] -> do
              eField fREGFILE 4
              eFieldS32 fSRCIMM imm
            [_] -> eFatal "wrong kind of destination operand"
            _ -> eFatal "wrong number of destination operands"

        ----------------------------------------------
        eMov :: E ()
        eMov = do
          eField fOPCODE 0x002
          ePredication
          eDstRegR
          case iSrcs i of
            [src,SrcI imm] -> do
              eUnrSrcs [src]
              eField fMOV_SRC1_IMM4 (fromIntegral imm)
            srcs -> do
              -- MOV without the imm encodes as 0xF
              eUnrSrcs srcs
              eField fMOV_SRC1_IMM4 0xF
          return ()



-------------------------------------------------------------------------------
fReserved :: Int -> Int -> Field
fReserved off len = (rESERVED_NAME,off,len)

rESERVED_NAME :: String
rESERVED_NAME = "*Reserved*"

fIsReserved :: Field -> Bool
fIsReserved (nm,_,_) = nm == rESERVED_NAME

after :: Field -> String -> Int -> Field
after f nm len = (nm,fOffset f + fLength f,len)

fOPCODE :: Field
fOPCODE = ("Opcode",0,9)

fREGFILE :: Field
fREGFILE = ("RegFile",9,3)

fPREDICATION :: Field
fPREDICATION = ("Predication",12,4)

fDSTREG :: Field
fDSTREG = ("Dst.Reg",16,8)
fSRC0REG :: Field
fSRC0REG = ("Src0.Reg",24,8)
fSRC0_ABS :: Field
fSRC0_ABS = ("Src0.AbsVal",72,1)
fSRC0_NEG :: Field
fSRC0_NEG = ("Src0.Negated",73,1)

fSRCREG :: Field
fSRCREG = ("Src.Reg",32,8)
fSRC1_ABS :: Field
fSRC1_ABS = ("Src1.AbsVal",62,1)
fSRC1_NEG :: Field
fSRC1_NEG = ("Src1.Negated",63,1)

fSRCIMM :: Field
fSRCIMM = ("Src.Imm",32,32)
fSRCCOFF :: Field
fSRCCOFF = ("Src.ConstOffset",40,14)
fSRCCIX :: Field
fSRCCIX = ("Src.ConstIndex",54,5)

fDEPINFO_STALLS :: Field
fDEPINFO_STALLS = ("DepInfo.Stalls",105,4) -- [108:105]
fDEPINFO_YIELD :: Field
fDEPINFO_YIELD = after fDEPINFO_STALLS "DepInfo.NoYield" 1 -- [109]
fDEPINFO_WRBAR :: Field
fDEPINFO_WRBAR = after fDEPINFO_YIELD "DepInfo.WrBar" 3 -- [112:110]
fDEPINFO_RDBAR :: Field
fDEPINFO_RDBAR = after fDEPINFO_WRBAR "DepInfo.RdBar" 3 -- [115:113]
fDEPINFO_WMASK :: Field
fDEPINFO_WMASK = after fDEPINFO_RDBAR "DepInfo.WtMask" 6 -- [121:116]
fUNK_REUSE :: Field
fUNK_REUSE = after fUNK_REUSE "???.Reuse" 1 -- [122]
fSRC0_REUSE :: Field
fSRC0_REUSE = after fDEPINFO_WMASK "Src0.Reuse" 1 -- [123]
fSRC1_REUSE :: Field
fSRC1_REUSE = after fSRC0_REUSE "Src1.Reuse" 1 -- [124]
fSRC2_REUSE :: Field
fSRC2_REUSE = after fSRC1_REUSE "Src2.Reuse" 1 -- [125]
-- zeros [127:126]

-------------------------------------------------------------------------------
-- special per-instruction fields with unknown behavior
--
fMOV_SRC1_IMM4 :: Field
fMOV_SRC1_IMM4 = ("Mov.Src1.Imm4",72,4)
