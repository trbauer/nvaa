module NVT.Encoders.InstEncoder where

import NVT.Bits
import NVT.Loc
import NVT.Diagnostic

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
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
    Right (_,es) -> Right (esInst es, reverse (esWarnings es), reverse (esFields es))

-- eFatal :: (MonadTrans t, Monad m) => Diagnostic -> t (CME.ExceptT Diagnostic m) a
eFatal :: String -> E ()
eFatal str = 
  lift (CME.throwE (Diagnostic lNONE str)) -- we patch in the location later

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
    eFatal $ "field overlaps another field already set"
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