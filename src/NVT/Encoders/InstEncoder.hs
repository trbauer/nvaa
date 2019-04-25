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

runInstEncoders :: [Inst] -> Either Diagnostic ([Word128],[Diagnostic])
runInstEncoders is = do
  (ws,dss) <- unzip <$> sequence (map runInstEncoder is)
  return (ws,concat dss)

runInstEncoder :: Inst -> Either Diagnostic (Word128,[Diagnostic])
runInstEncoder i = runE (iLoc i) (eInst i)
runInstDbgEncoder :: Inst -> Either Diagnostic (Word128,[Diagnostic],[(Field,Word64)])
runInstDbgEncoder i = runDbgE (iLoc i) (eInst i)

-------------------------------------------------------------------------------

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
eFatal :: String -> E a
eFatal msg = do
  loc <- CMS.gets esLoc
  lift (CME.throwE (Diagnostic loc msg)) -- we patch in the location later
eFatalF :: Field -> String -> E a
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
    eFatal $ fName f ++ ": value overflows field"
  let mask_val = getField128 (fOffset f) (fLength f) (esMask es)
  when ((mask_val .&. mask) /= 0) $ do
    let overlaps =
          case filter (fOverlaps f . fst) (esFields es) of
            ((f1,_):_) -> fFormatName f1
            _ -> "?"
    eFatal $ fFormatName f ++ ": field overlaps another field already set (" ++ overlaps ++ ")"
  --
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

eFieldU32 :: Field -> Word64 -> E ()
eFieldU32 = eFieldUnsignedImm 32
eFieldUnsignedImm :: Int -> Field -> Word64 -> E ()
eFieldUnsignedImm bits f val
  | val <= 2^bits - 1 = eField f val
  | otherwise = eFatalF f "value out of bounds"
eFieldS32 :: Field -> Int64 -> E ()
eFieldS32 = eFieldSignedImm 32
eFieldSignedImm :: Int -> Field -> Int64 -> E ()
eFieldSignedImm bits f val
  | val >= -2^(bits-1) && val < 2^(bits-1) = eField f (field_mask .&. w_val)
--  | high_bits /= 0 && high_bits /= 0xFFFFFFFF00000000 = eField f (field_mask .&. w_val)
  | otherwise = eFatalF f "value out of bounds"
  where w_val = fromIntegral val :: Word64
        field_mask
          | fLength f == 64 = 0xFFFFFFFFFFFFFFFF
          | otherwise = (1`shiftL`fLength f) - 1

        high_mask = complement field_mask
        high_bits = high_mask .&. w_val

-------------------------------------------------------------------------------


eInst :: Inst -> E ()
eInst i = enc
  where op = iOp i

        iOptsSubset :: [InstOpt] -> [InstOpt]
        iOptsSubset xs = sort (filter (`elem`xs) (iOptions i))

        iHasOpt :: InstOpt -> Bool
        iHasOpt = (`elem`iOptions i)

        eInstOpt :: Field -> InstOpt -> E ()
        eInstOpt fF io = eEncode fF (iHasInstOpt io i)
        eInvertedInstOpt :: Field -> InstOpt -> E ()
        eInvertedInstOpt fF io = eEncode fF (not (iHasInstOpt io i))

        enc = do
          case oMnemonic op of
            "MOV" -> eMOV
            "S2R" -> eS2R
            "IADD3" -> eIADD3
            "IMAD" -> eIMAD
            "ISETP" -> eISETP
            "STG" -> eSTG
            "LDG" -> eLDG
            "NOP" -> do
              eField fOPCODE 0x118
              eRegFile False 0x4
              ePredication
              return ()
            s -> eFatal $ "unsupported operation"
          eDepInfo (iDepInfo i)

        eRegFile :: Bool -> Word64 -> E ()
        eRegFile unif val = do
          eEncode fUNIFORMREG unif
          eField fREGFILE val

        ePredication :: E ()
        ePredication = ePredicationSrc fPREDICATION (iPredication i)
        ePredicationSrc :: Field -> Pred -> E ()
        ePredicationSrc fPSRC pred =
          case pred of
            PredNONE -> ePredicationAt fPSRC False PT
            PredP sign pr -> ePredicationAt fPSRC sign pr
            PredUP sign pr -> ePredicationAt fPSRC sign pr
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
            [DstR r] -> eEncode fDST_REG r
            [_] -> eFatal "wrong kind of destination operand"
            _ -> eFatal "wrong number of destination operands"

        ensureNoNegAbs :: Bool -> Bool -> E ()
        ensureNoNegAbs neg abs = do
          when neg $
            eFatal "unary instructions don't support src negation modifier"
          when abs $
            eFatal "unary instructions don't support src aboslute-value modifier"

        eConstOffDiv4 :: Field -> Int -> E ()
        eConstOffDiv4 f soff = do
          when ((soff`mod`4)/=0) $
            eFatalF f "constant offset must be a multiple of 4"
          eField fSRCCOFF (fromIntegral soff `div` 4)

        -- 1 = R, 4 = IMM, 5 = const
        eUnrSrcs :: [Src] -> E ()
        eUnrSrcs srcs =
          case srcs of
            [SrcR neg abs reu reg] -> do
              eField fREGFILE 1
              eEncode fSRC1_REG reg -- src goes in [39:32]
              ensureNoNegAbs neg abs
              eEncode fSRC1_REUSE reu -- use Src1.Reuse
            [SrcC neg abs six soff] -> do
              eField fREGFILE 5
              eField fSRCCIX  (fromIntegral six)
              eConstOffDiv4 fSRCCOFF soff
              ensureNoNegAbs neg abs
            [SrcCX neg abs ur_six soff] -> do
              eFatal "eUnrSrcs: TODO: cx[UR#][...] src"
            [SrcI imm] -> do
              eField fREGFILE 4
              eFieldU32 fSRCIMM imm
            [SrcUR neg abs ur] -> do
              eEncode fSRC1_UREG ur
              eEncode fSRC1_NEG neg
              eEncode fSRC1_ABS abs
              eField fREGFILE 6
              eEncode fUNIFORMREG True
            [_] -> eFatal "wrong kind of source operand"
            _ -> eFatal "wrong number of source operands"

        eSrc1C :: Src -> E ()
        eSrc1C = eSrcXC (fSRC1_NEG,fSRC1_ABS,fSRC1_CIX,fSRC1_UREG,fSRC1_COFF)
        eSrc2C :: Src -> E ()
        eSrc2C = eSrcXC (fSRC2_NEG,fSRC2_ABS,fSRC2_CIX,fSRC2_UREG,fSRC2_COFF)
        -- eSrc2C_IMAD :: Src -> E ()
        -- eSrc2C_IMAD = eSrcXC (Nothing,fSRC2_ABS,fSRC2_CIX,fSRC2_UREG,fSRC2_COFF)
        eSrcXC :: (Field,Field,Field,Field,Field) -> Src -> E ()
        eSrcXC  (fSRC_NEG,fSRC_ABS,fSRC_CIX,fSRC_UREG,fSRC_COFF) src = do
          case src of
            SrcC neg abs six soff -> do
              eEncode fSRC_NEG neg
              eEncode fSRC_ABS abs
              eEncode fSRC_CIX six
              eConstOffDiv4 fSRC_COFF soff
            SrcCX neg abs sur soff -> do
              eEncode fSRC_NEG neg
              eEncode fSRC_ABS abs
              eEncode fSRC_UREG sur
              eConstOffDiv4 fSRC_COFF soff
            _ -> eFatal "INTERNAL ERROR: eSrcXC: wrong source type"

        eSrcR :: Int -> (Maybe Field,Maybe Field,Field,Field) -> Src -> E ()
        eSrcR src_ix (mfNEG,mfABS,fREU,fREG) (SrcR neg abs reu r) = do
          eMaybeNegAbs src_ix (mfNEG,mfABS) neg abs
          eEncode fREU reu
          eEncode fREG r

        eMaybeNegAbs :: Int -> (Maybe Field,Maybe Field) -> Bool -> Bool -> E ()
        eMaybeNegAbs src_ix (mfNEG,mfABS) neg abs = do
          let eMaybe what mf z =
                case mf of
                  Nothing
                    | z -> eFatal $ what ++ " not supported on Src"++show src_ix++" for this instruction type"
                    | otherwise -> return ()
                  Just fF -> eEncode fF z
          eMaybe "absolute-value source modifier " mfABS abs
          eMaybe "negation source modifier "       mfNEG neg

        eSrc0R :: Src -> E ()
        eSrc0R = eSrcR 0 (Just fSRC0_NEG,Just fSRC0_ABS,fSRC0_REUSE,fSRC0_REG)
        -- e.g. .U32 of IMAD overlaps Src0.Negated
        -- "NN" means no negation
        eSrc0R_NN :: Src -> E ()
        eSrc0R_NN = eSrcR 0 (Nothing,Just fSRC0_ABS,fSRC0_REUSE,fSRC0_REG)
        eSrc0R_NNA :: Src -> E ()
        eSrc0R_NNA = eSrcR 0 (Nothing,Nothing,fSRC0_REUSE,fSRC0_REG)
        eSrc1R :: Src -> E ()
        eSrc1R = eSrcR 1 (Just fSRC1_NEG,Just fSRC1_ABS,fSRC1_REUSE,fSRC1_REG)
        eSrc2R :: Src -> E ()
        eSrc2R = eSrcR 2 (Just fSRC2_NEG,Nothing,fSRC2_REUSE,fSRC2_REG)

        eSrcUR :: Int -> (Field,Maybe Field,Maybe Field) -> Src -> E ()
        eSrcUR src_ix (fSRC_UREG,mfSRC_NEG,mfSRC_ABS) (SrcUR neg abs ur) = do
          eMaybeNegAbs src_ix (Nothing,Nothing) neg abs
          eEncode fSRC_UREG ur
        eSrc1UR_NN :: Src -> E ()
        eSrc1UR_NN = eSrcUR 1 (fSRC1_UREG,Nothing,Nothing)
        eSrc2UR_NN :: Src -> E ()
        eSrc2UR_NN = eSrcUR 2 (fSRC2_UREG,Nothing,Nothing) -- still targets [37:32] (just changes fName)

        -----------------------------------------------------------------------
        -----------------------------------------------------------------------
        eMOV :: E ()
        eMOV = do
          eField fOPCODE 0x002
          ePredication
          eDstRegR
          case iSrcs i of
            [src,SrcI imm] -> do
              eUnrSrcs [src]
              eField fMOV_SRC_CHEN4 (fromIntegral imm)
            srcs -> do
              -- MOV without the imm encodes as 0xF
              eUnrSrcs srcs
              eField fMOV_SRC_CHEN4 0xF
          return ()

        -----------------------------------------------------------------------
        eS2R :: E ()
        eS2R = do
          eField fOPCODE 0x119
          ePredication
          eDstRegR
          eField fREGFILE 0x4
          case iSrcs i of
            [SrcSR sr] ->
              eEncode fS2R_SRC0 sr
            _ -> eFatal "S2R requires SR* register source"

        -----------------------------------------------------------------------
        eIMAD :: E ()
        eIMAD = do
          eField fOPCODE (if iHasInstOpt InstOptWIDE i then 0x025 else 0x024)
          ePredication
          eEncode fINSTOPT_X (iHasInstOpt InstOptX i)
          eDstRegR
          eEncode fIMAD_SIGNED (not (iHasInstOpt InstOptU32 i))
          case iSrcs i of
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              -- imad__RRR_RRR
              eRegFile False 0x1
              --
              eSrc0R_NN s0
              --
              eSrc1R s1
              --
              eSrc2R s2
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),SrcI imm] -> do
              -- imad__RRsI_RRI
              eRegFile False 0x2
              --
              eSrc0R_NN s0
              --
              eField fSRC1_IMM imm
              --
              eSrc2R s1
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),s2@(SrcC _ _ _ _)] -> do
              -- imad__RRC_RRC
              eRegFile False 0x3
              --
              eSrc0R_NN s0
              --
              eSrc1C s2
              --
              eSrc2R s1
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),s2@(SrcCX _ _ _ _)] -> do
              -- imad__RRCx_RRCx
              eRegFile True 0x3
              --
              eSrc0R_NN s0
              --
              eSrc1C s2
              --
              eSrc2R s1
            [s0@(SrcR _ _ _ _),SrcI imm,s2@(SrcR _ _ _ _)] -> do
              -- imad__RsIR_RIR
              eRegFile False 0x4
              --
              eSrc0R_NN s0
              --
              eField fSRC1_IMM imm
              --
              eSrc2R s2
            [s0@(SrcR _ _ _ _),s1@(SrcC _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              -- imad__RCR_RCR
              eRegFile False 0x5
              --
              eSrc0R_NN s0
              --
              eSrc1C s1
              --
              eSrc2R s2
            [s0@(SrcR _ _ _ _),s1@(SrcCX _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              -- imad__RCxR_RCxR
              eRegFile True 0x5
              --
              eSrc0R_NN s0
              --
              eSrc1C s1
              --
              eSrc2R s2
            [s0@(SrcR _ _ _ _),s1@(SrcUR _ _ _),s2@(SrcR _ _ _ _)] -> do
              -- imad__RUR_RUR
              eRegFile True 0x6
              --
              eSrc0R_NN s0
              --
              eSrc1UR_NN s1
              --
              eSrc2R s2
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),s2@(SrcUR _ _ _)] -> do
              -- imad__RRU_RRU
              eRegFile True 0x7
              --
              eSrc0R_NN s0
              --
              eSrc2R s1
              --
              eSrc2UR_NN s2
            [_,_,_] -> eFatal "wrong type of arguments to instruction"
            _ -> eFatal "wrong number of arguments to instruction"
          eEncode fIADD3_SRCPRED0 PT
          case iSrcPreds i of
            [] -> ePredicationSrc fIADD3_X_SRCPRED1 (PredP True PT)
            [pt]
              | iHasInstOpt InstOptX i -> ePredicationSrc fIADD3_X_SRCPRED1 pt
            _ -> eFatal "wrong number of predicate arguments"

        -----------------------------------------------------------------------
        eIADD3 :: E ()
        eIADD3 = do
          eField fOPCODE 0x010
          ePredication
          eDstRegR
          eInstOpt fINSTOPT_X InstOptX
          -- the first two operands may be predicate sources,
          -- if they are omitted, we encode PT (0x7); neither has a sign
          (p0,p1,srcs) <-
            case iSrcs i of
              (SrcP p0neg p0:SrcP p1neg p1:sfx)
                | p0neg -> eFatal "predicate negation not support on extra predicate source (0)"
                | p1neg -> eFatal "predicate negation not support on extra predicate source (1)"
                | otherwise -> return (p0,p1,sfx)
              (SrcP p0neg p0:sfx)
                | p0neg -> eFatal "predicate negation not support on extra predicate source (0)"
                | otherwise -> return (p0,PT,sfx)
              sfx -> return (PT,PT,sfx)
          eEncode fIADD3_SRCPRED0 p0
          eEncode fIADD3_SRCPRED1 p1
          case srcs of
              -- iadd3_noimm__RRR_RRR
            [s0@(SrcR _ _ _ _),s1@(SrcR _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              eField fREGFILE 0x1
              eEncode fUNIFORMREG False
              --
              eSrc0R s0
              eSrc1R s1
              eSrc2R s2
              -- iadd3_noimm__RCR_RCR
            [s0@(SrcR _ _ _ _),s1@(SrcC _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              eField fREGFILE 0x5
              eEncode fUNIFORMREG False
              --
              eSrc0R s0
              --
              eSrc1C s1
              --
              eSrc2R s2
            -- iadd3_noimm__RCxR_RCxR
            [s0@(SrcR _ _ _ _),s1@(SrcCX _ _ _ _),s2@(SrcR _ _ _ _)] -> do
              eField fREGFILE 0x5
              eEncode fUNIFORMREG True
              --
              eSrc0R s0
              --
              eSrc1C s1
              --
              eSrc2R s2
            -- iadd3_imm__RsIR_RIR
            [s0@(SrcR _ _ _ _),SrcI imm,s2@(SrcR _ _ _ _)] -> do
              eField fREGFILE 0x4
              eEncode fUNIFORMREG False
              --
              eSrc0R s0
              --
              eField fSRC1_IMM imm
              --
              eSrc2R s2
            -- iadd3_noimm__RUR_RUR
            [s0@(SrcR _ _ _ _),s1@(SrcUR _ _ _),s2@(SrcR _ _ _ _)] -> do
              eField fREGFILE 0x6
              eEncode fUNIFORMREG True
              --
              eSrc0R s0
              --
              eSrc1UR_NN s1
              --
              eSrc2R s2
            [_,_,_] -> eFatal "unsupported operand kinds to IADD3"
            _ -> eFatal "wrong number of operands to IADD3"
          -- these are the extra source predicate expressions
          -- for the .X case
          (ext_pred0,ext_pred1) <- do
            if iHasOpt InstOptX then
              case iSrcPreds i of
                [sp0,sp1] -> return (sp0,sp1)
                [] -> return (PredNONE,PredNONE)
                _ -> eFatal "IADD3.X expects two extra predicate expression parameters"
            else
              -- explicit IR version (uses defaults)
              case iSrcPreds i of
                [sp0,sp1] -> return (sp0,sp1)
                -- _ -> eFatal "IADD3 takes exactly two extra predicates"
                [] -> return (PredP True PT,PredP True PT)
                -- _ -> eFatal "IADD3 without .X forbids extra predicates"
          ePredicationSrc fIADD3_X_SRCPRED0 ext_pred0
          ePredicationSrc fIADD3_X_SRCPRED1 ext_pred1
            -- eField (fi "IADD.UNKNOWN76_80" 76 5) 0x1E -- probably [80:77] = 0xF !PT
            -- eField (fi "IADD.UNKNOWN90_87" 87 4) 0xF
          return ()

        -----------------------------------------------------------------------
        eISETP :: E ()
        eISETP = do
          eField fOPCODE 0x00C
          --
          ePredication
          -- .EX
          eInstOpt fISETP_EX InstOptEX
          -- .U32
          eInvertedInstOpt fISETP_U32 InstOptU32
          -- .{AND,OR,XOR}
          case iOptsSubset [InstOptAND,InstOptOR,InstOptXOR] of
            [InstOptAND] -> eField fISETP_COMBINING_FUNCTION 0x0
            [InstOptOR]  -> eField fISETP_COMBINING_FUNCTION 0x1
            [InstOptXOR] -> eField fISETP_COMBINING_FUNCTION 0x2
            _ -> eFatal "exactly one combining function required (.AND,.OR,.XOR)"
          -- .{F,LT,EQ,...}
          case iOptsSubset inst_opt_isetp_functions of
            [InstOptF]  -> eField fISETP_FUNCTION 0x0
            [InstOptLT] -> eField fISETP_FUNCTION 0x1
            [InstOptEQ] -> eField fISETP_FUNCTION 0x2
            [InstOptLE] -> eField fISETP_FUNCTION 0x3
            [InstOptGT] -> eField fISETP_FUNCTION 0x4
            [InstOptNE] -> eField fISETP_FUNCTION 0x5
            [InstOptGE] -> eField fISETP_FUNCTION 0x6
            [InstOptT]  -> eField fISETP_FUNCTION 0x7
            _ -> eFatal "exactly one comparison function required"
          case iDsts i of
            [DstP p] -> eEncode fISETP_DST_PRED p
            _ ->  eFatal "malformed destination"
          case iSrcs i of
            [SrcP c_neg c_pr, src0@(SrcR _ _ _ _), src1] -> do
              when c_neg $
                eFatalF fISETP_SRC0_PRED "src0 predicate cannot be negated"
              ePredicationSrc fISETP_SRC0_PRED (PredP c_neg c_pr)
              eSrc0R_NNA src0
              case src1 of
                SrcR _ _ _ _ -> eRegFile False 0x1 >> eSrc1R src1
                SrcC _ _ _ _ -> eRegFile False 0x5 >> eSrc1C src1
                SrcCX _ _ _ _ -> eRegFile True 0x5 >> eSrc1C src1
                SrcI imm -> eRegFile False 0x4 >> eField fSRC1_IMM imm
                SrcUR _ _ _ -> eRegFile True 0x6 >> eSrc1UR_NN src1
                _ -> eFatal "source 1 must be R, I, or C"
            _ -> eFatal "wrong number of sources (expects P, R, R|I|C, P(, P?))"
          case iSrcPreds i of
            [] -> eFatal "extra source predicate expected (src3)"
            -- not sure how to get ISETP to use a different symbol for src3
            -- or where the bits actually go?
            (src3:sfx) -> do
              ePredicationSrc fISETP_SRC3_PRED src3
              case sfx of
                [] -> ePredicationSrc fISETP_SRC4_PRED (PredP False PT)
                [psrc] -> ePredicationSrc fISETP_SRC4_PRED psrc
                _ -> eFatal "wrong number of extra source predicates (src4)"

        -----------------------------------------------------------------------
        -- LD/ST helpers
        --
        eLdStDataType :: E ()
        eLdStDataType =
          case iOptsSubset inst_opt_ldst_types of
            [] -> eField fLDST_DATA_TYPE 0x4
            [InstOptU,InstOpt128] -> eField fLDST_DATA_TYPE 0x7
            (InstOptU:_) -> eFatalF fLDST_DATA_TYPE ".U only allowed on .128"
            [InstOptU8] -> eField fLDST_DATA_TYPE 0x0
            [InstOptS8] -> eField fLDST_DATA_TYPE 0x1
            [InstOptU16] -> eField fLDST_DATA_TYPE 0x2
            [InstOptS16] -> eField fLDST_DATA_TYPE 0x3
            --
            [InstOpt64] -> eField fLDST_DATA_TYPE 0x5
            [InstOpt128] -> eField fLDST_DATA_TYPE 0x6
            xs -> eFatalF fLDST_DATA_TYPE $
                    "invalid data combination: {" ++
                      intercalate ", " (map format xs) ++ "}"


        eLdStCommon :: E ()
        eLdStCommon = do
          eLdStDataType
          --
          eInstOpt fLDST_ADDR_E InstOptE
          --
          eInstOpt fLDST_PRIVATE InstOptPRIVATE
          --
          case iOptsSubset inst_opt_ldst_scope of
            [InstOptCTA] -> eField fLDST_SCOPE 0x0
            [InstOptSM]  -> eField fLDST_SCOPE 0x1
            [InstOptGPU] -> eField fLDST_SCOPE 0x2
            [InstOptSYS] -> eField fLDST_SCOPE 0x3
            _ -> eFatal "exactly one scope option required"
          --
          case iOptsSubset inst_opt_ldst_ordering of
            [] -> eField fLDST_ORDERING 0x1 -- default
            [InstOptSTRONG] -> eField fLDST_ORDERING 0x2
            [InstOptMMIO]   -> eField fLDST_ORDERING 0x3
            _ -> eFatal "only one memory ordering option permitted"
          --
          case iOptsSubset inst_opt_ldst_caching of
            [] -> eField fLDST_CACHING 0x1 -- default
            [InstOptEF] -> eField fLDST_CACHING 0x0
            [InstOptEL] -> eField fLDST_CACHING 0x2
            [InstOptLU] -> eField fLDST_CACHING 0x3
            [InstOptEU] -> eField fLDST_CACHING 0x4
            [InstOptNA] -> eField fLDST_CACHING 0x5
            _ -> eFatal "only one caching option permitted"

        -- [11:9]  [90] [91]  [31:24]
        --    1     *    0     /=RZ       LDG.E.64.SYS R16, [R24] {!1,+4.W} ;
        --    4     0    1     /=RZ       LDG.E.64.SYS R16, [R254.U32+UR0]
        --    4     0    1     ==RZ       LDG.E.64.SYS R16, [UR0]  // probably [RZ.U32+UR0]
        --    4     1    1     /=RZ       LDG.E.64.SYS R16, [R254.64+UR0]
        --    4     1    1     ==RZ       LDG.E.64.SYS R16, [UR0]  // probably [RZ.U32+UR0]
        eLDST_Addrs :: [Src] -> E ()
        eLDST_Addrs srcs =
          case srcs of
            [SrcR _ _ _ r_addr, SrcUR _ _ ur, SrcI imm] -> do
              eEncode fSRC0_REG r_addr
              if ur == URZ
                then do
                  eField fREGFILE 0x1
                else do
                -- oddly their encoder leaves this 0's if the uniform offset
                -- isn't used (bit 91) so we only set this field for that case
                  eField fREGFILE 0x4
                  eEncode fLDST_ADDR_UROFF ur
                  -- eEncode fLDST_UNIFORMOPTS True
                  eFatal $ "op doesn't support URZ yet (need to sort out bits[91:90])"
              eField fLDST_ADDR_IMMOFF imm
            _ -> eFatal $ "malformed source parameters for " ++ show (iOp i)

        -----------------------------------------------------------------------
        eLDG :: E ()
        eLDG = do
          eField fOPCODE 0x181
          ePredication
          eLdStCommon
          case iDsts i of
            [DstP r_prd,DstR r_data] -> do
              unless (iHasOpt InstOptZD || r_prd == PT) $
                eFatalF fLD_ZD_PREG "dst predicate requires .ZD be set"
              eEncode fLD_ZD_PREG r_prd
              eEncode fDST_REG r_data
            [DstR r_data] -> do
              when (iHasOpt InstOptZD) $
                eFatalF fLD_ZD "lack of dst predicate forbids .ZD be set"
              eEncode fDST_REG r_data
              eEncode fLD_ZD_PREG PT
            _ -> eFatal "expected one data register"
          eLDST_Addrs (iSrcs i)
          eInstOpt fLD_ZD InstOptZD

        -----------------------------------------------------------------------
        eSTG :: E ()
        eSTG = do
          eField fOPCODE 0x186
          ePredication
          eLdStCommon
          eLDST_Addrs (take 3 (iSrcs i))
          case drop 3 (iSrcs i) of
            [SrcR False False False r_data] -> do
              eEncode fSRC1_REG r_data
            [SrcR _ _ _ r_data] -> eFatal "source modifiers and reuse flags not permitted"
            _ -> eFatal "expected one data register"
          --
          return ()


        -----------------------------------------------------------------------
        -- eNEXTOP :: E ()

-------------------------------------------------------------------------------
data Field =
  Field {
    fName :: !String
  , fOffset :: !Int
  , fLength :: !Int
  , fFormat :: !(Word128 -> Word64 -> String)
  }
instance Show Field where
  show f =
    "Field {\n" ++
    "  fName = " ++ show (fName f) ++ "\n" ++
    "  fOffset = " ++ show (fOffset f) ++ "\n" ++
    "  fLength = " ++ show (fLength f) ++ "\n" ++
    "  fFormat = ...function...\n" ++
    "}"
fFormatName :: Field -> String
fFormatName f  = fName f ++ "[" ++ body ++ "]"
  where body
          | fLength f == 1 = show (fOffset f)
          | otherwise = show (fOffset f + fLength f - 1) ++ ":" ++ show (fOffset f)
fOverlaps :: Field -> Field -> Bool
fOverlaps f0 f1 =
  -- [.....] f0
  --      [.....] f1
  fOffset f1 >= fOffset f0 && fOffset f1 < fOffset f0 + fLength f0 ||
  --     [.....] f0
  --  [.....] f1
  fOffset f0 >= fOffset f1 && fOffset f0 < fOffset f1 + fLength f1


f :: String -> Int -> Int -> (Word128 -> Word64 -> String) -> Field
f = Field
fi :: String -> Int -> Int -> Field
fi nm off len = f nm off len $ const show
fb :: String -> Int -> String -> Field
fb nm off true = fl nm off 1 ["",true]
fl :: String -> Int -> Int -> [String] -> Field
fl nm off len vs = f nm off len fmt
  where fmt _ v
          | fromIntegral v >= length vs = show v ++ "?"
          | otherwise = vs !! fromIntegral v
fC :: (Codeable c,Syntax c) => c -> String -> Int -> Int -> Field
fC c_dummy nm off len = f nm off len fmt
  where fmt _ val =
          case decode val of
            Left err -> err
            Right c -> areSame c c_dummy $ format c

        areSame :: a -> a -> b -> b
        areSame _ _ b = b

after :: Field -> String -> Int -> (Word64 -> String) -> Field
after fBEFORE nm len fmt = afterG fBEFORE nm len $ const fmt
afterI :: Field -> String -> Int -> Field
afterI fBEFORE nm len = after fBEFORE nm len show
afterB :: Field -> String -> String -> Field
afterB fBEFORE nm = fb nm (fOffset fBEFORE + fLength fBEFORE)
afterG :: Field -> String -> Int -> (Word128 -> Word64 -> String) -> Field
afterG fBEFORE nm len fmt = f nm (fOffset fBEFORE + fLength fBEFORE) len fmt

fReserved :: Int -> Int -> Field
fReserved off len = f rESERVED_NAME off len $ \_ v ->
  if v /= 0 then "should be zeros" else ""

rESERVED_NAME :: String
rESERVED_NAME = "*Reserved*"

fIsReserved :: Field -> Bool
fIsReserved = (==rESERVED_NAME) . fName
-------------------------------------------------------------------------------
-- field constructors for our syntax
fR :: String -> Int -> Field
fR nm off = fC (undefined :: R) nm off 8
fUR :: String -> Int -> Field
fUR nm off = fC (undefined :: UR) nm off 6
fPREDSRC :: String -> Int -> Field
fPREDSRC nm off = f nm off 4 fmt
  where fmt _ val =
            case decode (val.&.0x7) :: Either String PR of
              Left err -> neg ++ err
              Right a -> neg ++ format a
          where neg = if testBit val 3 then "!" else ""
fPREG :: String -> Int -> Field
fPREG nm off = f nm off 3 fmt
  where fmt _ val =
          case decode val :: Either String PR of
            Left err -> err
            Right a -> format a

-------------------------------------------------------------------------------
fOPCODE :: Field
fOPCODE = fi "Opcode" 0 9

fREGFILE :: Field
fREGFILE = fi "RegFile" 9 3

fPREDICATION :: Field
fPREDICATION = f{fFormat = fmt}
  where f = fPREDSRC "Predication" 12
        fmt w128 v64 = "@" ++ fFormat f w128 v64

fABS :: Int -> Int -> Field
fABS src_ix off = fb ("Src" ++ show src_ix ++ ".AbsVal") off "|..|"
fNEG :: Int -> Int -> Field
fNEG src_ix off = fb ("Src" ++ show src_ix ++ ".Negated") off "-(..)"

fDST_REG :: Field
fDST_REG = fR "Dst.Reg" 16


fSRC0_REG :: Field
fSRC0_REG = fR "Src0.Reg" 24
fSRC0_ABS :: Field
fSRC0_ABS = fABS 0 72
fSRC0_NEG :: Field
fSRC0_NEG = fNEG 0 73


fSRC1_REG :: Field
fSRC1_REG = fR "Src1.Reg" 32
fSRC1_UREG :: Field
fSRC1_UREG = fUR "Src1.UReg" 32
fSRC1_CIX :: Field
fSRC1_CIX = fSRCCIX{fName = "Src1.ConstIndex"}
fSRC1_COFF :: Field
fSRC1_COFF = fSRCCOFF{fName = "Src1.ConstOffset"}
fSRC1_CUIX :: Field
fSRC1_CUIX = fSRCCOFF{fName = "Src1.ConstIndex"}
--
fSRC1_IMM :: Field
fSRC1_IMM = fSRCIMM{fName = "Src1.Imm"}
--
fSRC1_ABS :: Field
fSRC1_ABS = fABS 1 62
fSRC1_NEG :: Field
fSRC1_NEG = fNEG 1 63


fSRC2_REG :: Field
fSRC2_REG = fR "Src0.Reg" 64
fSRC2_UREG :: Field
fSRC2_UREG = fSRC1_UREG{fName = "Src2.UReg"}
--
fSRC2_CIX :: Field
fSRC2_CIX = fSRCCIX{fName = "Src2.ConstIndex"}
fSRC2_COFF :: Field
fSRC2_COFF = fSRCCOFF{fName = "Src2.ConstOffset"}
fSRC2_CUIX :: Field
fSRC2_CUIX = fSRCCOFF{fName = "Src2.ConstIndex"}
--
fSRC2_IMM :: Field
fSRC2_IMM = fSRCIMM{fName = "Src2.Imm"}
--
fSRC2_ABS :: Field -- (not supported for IADD), [74] is .X
fSRC2_ABS = fABS 2 74
fSRC2_NEG :: Field
fSRC2_NEG = fNEG 2 75


fSRCIMM :: Field
fSRCIMM = fi "Src.Imm" 32 32
fSRCCOFF :: Field
fSRCCOFF = f "Src.ConstOffset" 40 14 fmt
  where fmt w128 off =
            c_or_cx ++ "[..][" ++ printf "0x%X" (4*off) ++ "]"
          where c_or_cx = if testBit w128 91 then "cx" else "c"
fSRCCIX :: Field
fSRCCIX = f "Src.ConstIndex" 54 5 fmt
  where fmt w128 val = "c[" ++ show val ++ "][..]"
fSRCCUIX :: Field
fSRCCUIX = f "Src.ConstIndex" 32 6 $ \_ val ->
  let ureg = if val == 63 then "URZ" else ("UR" ++show val)
    in "cx[" ++ ureg ++ "][..]"

fUNIFORMREG :: Field
fUNIFORMREG = fb "EnableUniformReg" 91 "uniform registers enabled"

fDEPINFO_STALLS :: Field
fDEPINFO_STALLS = f "DepInfo.Stalls" 105 4 $ \_ val -> -- [108:105]
  if val == 0 then "" else ("!" ++ show val)

fDEPINFO_YIELD :: Field
fDEPINFO_YIELD = after fDEPINFO_STALLS "DepInfo.NoYield" 1 $ -- [109]
  \val ->
    if val == 1
      then "no yield: prefer same warp"
      else "yield: prefer another warp"
fDEPINFO_WRBAR :: Field
fDEPINFO_WRBAR = fRWBAR fDEPINFO_YIELD "DepInfo.WrBar" -- [112:110]
fDEPINFO_RDBAR :: Field
fDEPINFO_RDBAR = fRWBAR fDEPINFO_WRBAR "DepInfo.RdBar" -- [115:113]
fRWBAR :: Field -> String -> Field
fRWBAR fAFT name = after fAFT name 3 fmt
  where fmt 7 = "no barrier"
        fmt val = "+" ++ show (val+1) ++ "." ++ which
        which = take 1 $ drop 1 (dropWhile (/='.') name)

fDEPINFO_WMASK :: Field
fDEPINFO_WMASK = afterI fDEPINFO_RDBAR "DepInfo.WtMask" 6 -- [121:116]
fUNK_REUSE :: Field
fUNK_REUSE = afterB fUNK_REUSE "???.Reuse" ".reuse" -- [122]
fSRC0_REUSE :: Field
fSRC0_REUSE = afterB fDEPINFO_WMASK "Src0.Reuse" ".reuse" -- [123]
fSRC1_REUSE :: Field
fSRC1_REUSE = afterB fSRC0_REUSE "Src1.Reuse" ".reuse" -- [124]
fSRC2_REUSE :: Field
fSRC2_REUSE = afterB fSRC1_REUSE "Src2.Reuse" ".reuse" -- [125]
-- zeros [127:126]


-------------------------------------------------------------------------------
-- special per-instruction fields
--
fMOV_SRC_CHEN4 :: Field
fMOV_SRC_CHEN4 = f "Mov.ChEn4" 72 4 fmt
  where fmt _ v =
          -- bit 0 enables X (0,4,..)
          -- bit 1 enables Y (1,5,..)
          -- bit 2 enables Z (2,6,..)
          -- bit 3 enables W (3,7,..)
          case v of
            0xF -> ".XYZW: all channels enabled"
            0x0 -> "all channels disabled"
            x ->
              case filter (testBit x) [0,1,2,3] of
                chs -> "." ++ map ("XYZW"!!) chs

-- this is a channel

fS2R_SRC0 :: Field
fS2R_SRC0 = fi "S2R.Src0" 72 8

fINSTOPT_X :: Field
fINSTOPT_X = fi "IADD3.X" 74 1

-- IADD3 R4, P0, P5, R24, 0x18, RZ   000FE40007D1E0FF`0000001818047810
--           ^^  ^^ these
-- nvdisasm has a problem here in that if the second is set (P5 above)
-- and the first (P0 above) is not (i.e. it's PT) we cannot discern if
-- the first or second slot should hold the value
-- one solution is to always print the first (as PT)
-- if the second is set (not PT)
--
-- I am starting to wonder if these are srcs or dests;
-- since they have no signs and reject PT from syntax (hide),
-- it's fishy
fIADD3_SRCPRED0 :: Field
fIADD3_SRCPRED0 = fPREG "IADD3.SrcPred0" 81
fIADD3_SRCPRED1 :: Field
fIADD3_SRCPRED1 = fPREG "IADD3.SrcPred1" 84
-- bit 4

fIADD3_X_SRCPRED0 :: Field
fIADD3_X_SRCPRED0 = fPREDSRC "IADD3.X.SrcPred0" 77
fIADD3_X_SRCPRED1 :: Field
fIADD3_X_SRCPRED1 = fPREDSRC "IADD3.X.SrcPred1" 87

fIMAD_SIGNED :: Field
fIMAD_SIGNED = fl "IMAD.Signed" 73 1 [".U32","(.S32)"]

-------------------------------------------------------------------------------
fLDST_ADDR_E :: Field
fLDST_ADDR_E = fb "LDST.Addr.Is64b" 72 ".E"
fLDST_DATA_TYPE :: Field
fLDST_DATA_TYPE = f "LDST.Data.Type" 73 3 $ \_ val ->
  case val of
    0 -> ".U8"
    1 -> ".S8"
    2 -> ".U16"
    3 -> ".S16"
    4 -> "(.32)"
    5 -> ".64"
    6 -> ".128"
    7 -> ".U.128" -- forbidden in LDS
fLDST_ADDR_IMMOFF :: Field
fLDST_ADDR_IMMOFF = fi "LDST.Addr.ImmOff" 40 24
fLDST_ADDR_UROFF :: Field
fLDST_ADDR_UROFF = fUR "LDST.Addr.UROff" 32

-- this is not to be confused with fLDST_DATA_TYPE = 7
fLDS_U :: Field
fLDS_U = fb "LDS.U" 76 ".U"
-- LDS ... [R12.X4]
fLDSTS_SCALE :: Field
fLDSTS_SCALE = f "LDST.Scale" 77 2 $ \_ val ->
  case val of
    0 -> "(no scale)"
    1 -> ".X4"
    2 -> ".X8"
    3 -> ".X16"

fLD_ZD :: Field
fLD_ZD = fb "LD.ZD" 87 ".ZD"
fLD_ZD_PREG :: Field
fLD_ZD_PREG = fPREG "LD.ZD.Pred" 81

fLDST_PRIVATE :: Field
fLDST_PRIVATE = fb "LDST.Private" 76 ".PRIVATE"
fLDST_SCOPE :: Field
fLDST_SCOPE = f "LDST.Scope" 77 2 $ \_ val ->
  case val of
    0 -> ".CTA"
    1 -> ".SM"
    2 -> ".GPU"
    3 -> ".SYS"
fLDST_ORDERING :: Field
fLDST_ORDERING = f "LDST.Ordering" 79 2 $ \_ val ->
  case val of
    0 -> ".INVALID0"
    1 -> "(default)"
    2 -> ".STRONG"
    3 -> ".MMIO"
fLDST_CACHING :: Field
fLDST_CACHING = f "LDST.Caching" 84 3 $ \_ val ->
  case val of
    0 -> ".EF (evict first)"
    1 -> "(default)"
    2 -> ".EL (evict last)"
    3 -> ".LU (last use / invalidate after read)"
    4 -> ".EU (evict ...)"
    5 -> ".NA (no allocate / bypass)"
    6 -> ".INVALID6"
    7 -> ".INVALID7"

fISETP_DST_PRED :: Field
fISETP_DST_PRED = fPREG "ISETP.Dst.Reg" 81

fISETP_SRC0_PRED :: Field
fISETP_SRC0_PRED = fPREG "ISETP.Src0.PredExpr" 84
fISETP_SRC3_PRED :: Field
fISETP_SRC3_PRED = fPREDSRC "ISETP.Src3.PredExpr" 87
fISETP_SRC4_PRED :: Field
fISETP_SRC4_PRED = fPREDSRC "ISETP.Src4.PredExpr" 68

fISETP_EX :: Field
fISETP_EX = fb "ISETP.IsU32" 72 ".EX" -- 64b

fISETP_U32 :: Field
fISETP_U32 = fl "ISETP.IsU32" 73 1 [".U32","(.S32)"]

fISETP_COMBINING_FUNCTION :: Field
fISETP_COMBINING_FUNCTION = fl "ISETP.CombiningFunction" 74 2 [".AND",".OR",".XOR",".INVALID3"]

fISETP_FUNCTION :: Field
fISETP_FUNCTION = f "ISETP.Function" 76 3 $ \_ val ->
  case val of
    0 -> ".F"
    1 -> ".LT"
    2 -> ".EQ"
    3 -> ".LE"
    4 -> ".GT"
    5 -> ".NE"
    6 -> ".GE"
    7 -> ".T"
