module NVT.Parsers.ParseCubinListing where

import NVT.Parsers.Parser
import NVT.Bits

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P


parseCubinListing :: FilePath -> String -> Either Diagnostic (Binary,[Diagnostic])
parseCubinListing = runPC pcListing

-----------------------------

type PC a = P Identity PCBSt a
data PCBSt = 
  PCBSt {
  -- global
    pcbDefinitions :: ![(String,Int,Loc)]
  -- current parse
  , pcbOffset :: !Int
  , pcbWhere :: !String
  , pcbBinary :: !Binary
  , pcbReferences :: ![(RefExpr,Int)]
  } deriving Show
initPCBS :: PCBSt
initPCBS = PCBSt [] 0 "" bEmpty []

data RefExpr =
    RefExprID !Loc !String
  | RefExprSub !Loc !RefExpr !RefExpr
  deriving Show

data PartialSection =
  PartialSection {
    psName       :: !String -- e.g. ".debug_line" ....
  , psAttrs      :: !String -- e.g. "" or "ax"
  , psType       :: !String -- e.g. @progbits
  , psReferences :: ![(RefExpr,Int)] -- unresolved labels (and relative offset within the section)
  , psBinary     :: !Binary
  } deriving Show

runPC :: PC a -> FilePath -> String -> Either Diagnostic (a,[Diagnostic])
runPC pca fp inp = 
  runIdentity $ runP pca initPCBS fp inp

-----------------------------

pAdvanceOffset :: Int -> PC ()
pAdvanceOffset k = pModify $ \cbs -> cbs{pcbOffset = pcbOffset cbs + k} 
pResetOffset :: PC ()
pResetOffset = pSetOffset 0
pSetOffset :: Int -> PC ()
pSetOffset off = pModify $ \cbs -> cbs{pcbOffset = off} 
pGetOffset :: PC Int
pGetOffset = pGets pcbOffset

-----------------------------
testFile :: IO ()
testFile = do
  fstr <- readFile "BlackScholes.sass"
  length fstr `seq` return ()
  case parseCubinListing "BlackScholes.sass" fstr of
    Left err -> putStrLn $ fmtDiagnostic err
    Right (bin,_) -> do
      putStrLn "finished"


pcListing :: PC Binary
pcListing = do
  pSectionReset
  pcHeader
  ps <- P.many pcSection
  P.eof
  -- TODO:  resolve references
  return bEmpty


-- data CubinHeader =
--   CubinHeader {
--     cbhHeaderFlags :: !String
--   , cbhElfType
--   }

pcHeader :: PC String
pcHeader = do
  pSymbol_ ".headerflags"
  pSymbol_ "@"
  headerflags <- pStringLiteral
  pSymbol ".elftype" 
  pSymbol_ "@"
  elftype <- pStringLiteral
  pTrace $ (headerflags,elftype)
  return headerflags



pcSection :: PC PartialSection
pcSection = P.label "section" $ do
  pSymbol ".section"
  id <- pcDirective
  pSymbol ","
  attrs <- pStringLiteral
  pSymbol ","
  stype <- pSymbol "@progbits"
  --
  pSectionBody
  --
  refs <- pGets pcbReferences
  bin <- pGets pcbBinary
  pSectionReset  
  --  
  return $ PartialSection id attrs stype bin refs
pSectionReset = 
  pModify $ \pcb -> pcb{pcbWhere = "",pcbOffset = 0, pcbBinary = bEmpty, pcbReferences = []}

pSectionBody :: PC ()
pSectionBody = P.try pLabel <$> pSectionBinaryLine

pSectionBinaryLine :: PC ()
pSectionBinaryLine =
   pSectionBinaryLine ".byte"  pImm8  <|>
   pSectionBinaryLine ".short" pImm16 <|>
   pSectionBinaryLine ".word"  pImm32 <|>
   pSectionBinaryLine ".dword" pImm64

pSectionBinaryLine :: Integral a, Ord a, Num a => String -> Int -> PC a -> PC ()
pSectionBinaryLine sym val pValue = P.label (sym ++ " value") $ do
  pSymbol sym
  P.sepBy (pLabelReference <|> pValue) (pSymbol ",")


--        /*0022*/ 	.short	(.L_40 - .L_39)
pLabelReference :: Int -> PC ()
pLabelReference bsz = 
  where body = do
          o <- pGetOffset
          e <- pExpr
          pModify $ \pcb -> pcb{pcbReferences = (e,o):pcbReferences pcb}
  
        --
        pExpr = pAddExpr
        pAddExpr = P.chainl1 pPrimExpr pAddOp
          where pAddOp = do
                  loc <- pGetLoc 
                  pSymbol "-" 
                  return $ RefExprSub loc
        pPrimExpr = pLabelRef <|> pGroup
          where pLabel = do
                  loc <- pGetLoc 
                  RefExprID loc <$> pcAsmIdentifier
                pGroup = do
                  pSymbol "("
                  e <- pExpr
                  pSymbol ")"
                  return e


          


pLabelDefinition :: PC ()
pLabelDefinition = P.try $ do
  loc <- pGetLoc
  id <- pcAsmIdentifier
  P.char ':'
  pcs <- pGet
  off <- pGetOffset
  refs <- pGet pcbReferences
  case find (\(lbl,_,_) -> lbl == id) refs of
    Just (_,_,prev_loc) -> 
      pSemanticError loc $ 
        "redefinition of label (previous definition was at " ++ fmtLoc prev_loc ++ ")"
    Nothing -> return ()
  pModify $ \pcb -> pcb{pcbDefinitions = (id,off,loc):pcbDefinitions pcb}




--	.section	.debug_line,"",@progbits

-- .text._Z15BlackScholesGPUP6float2S0_S0_S0_S0_ffi
-- e.g.
pcAsmIdentifier :: PC String
pcAsmIdentifier = P.try $ P.many1 (P.char '.' <|> P.alphaNum <|> P.oneOf "_$")

pcAppendBits :: (FiniteBits i,Integral i) => i -> PC ()
pcAppendBits i = do
  pModify $ \pcb -> pcb{pcbBinary pcb `bAppend` toByteStringLE i, pcbOffset = pcbOffset pcb + bSize b}


