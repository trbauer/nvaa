module NVT.RawInst where

import NVT.Bits
import NVT.CUDASDK
import NVT.Floats
import NVT.Fmt
import NVT.Lop3
import NVT.Opts
-- import NVT.Parsers.Parser

-- import Control.Applicative
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.Exit
import System.Directory
import System.IO
import Text.Printf
import qualified Data.ByteString as BS

data RawBlock =
  RawBlock {
    rbLine :: !Int
  , rbOffset :: !Int
  , rbLabel :: !String
  , rbInsts :: ![RawInst]
  }

data RawInst =
  RawInst {
    riLine :: !Int
  , riOffset :: !Int
  , riPredication :: !String
  , riMnemonic :: !String
  , riOptions :: ![String] -- . suffixes of mnemonic
  , riOperands :: ![String]
  , riDepInfo :: ![String]
  } deriving Show


data SampleInst =
  SampleInst {
    siRawInst :: !RawInst
  , siBits :: !Word128
  } deriving Show


fmtRawInst :: RawInst -> String
fmtRawInst = fmtRawInstWith dft_fos
fmtRawInstWith :: FmtOpts -> RawInst -> String
fmtRawInstWith fos = concatMap fsText . fmtSpansRawInstWith fos

data FmtOpts =
  FmtOpts {
    -- alignment for dep info
    foColsPerInstBody :: !Int
    -- width of the mnmeonic (and options/subops)
  , foColsPerMnemonic :: !Int
    -- per each operand
  , foColsPerOperand :: !Int
    -- per predicate
  , foColsPerPredicate :: !Int
    -- do we decode LOP3 expressions
  , foDecodeLop3 :: !Bool
    -- do we print deps
  , foPrintDeps :: !Bool
    -- print the text encoding
  , foPrintEncoding :: !Bool
    -- print PC offsets
  , foPrintOffsets :: !Bool
    -- do we permit various short hand forms?
  , foShortHand :: !Bool
  } deriving Show
dft_fos :: FmtOpts
dft_fos =
  FmtOpts {
    foColsPerInstBody = 56
  , foColsPerMnemonic = 8
  , foColsPerOperand = 8
  , foColsPerPredicate = 5
  , foDecodeLop3 = True
  , foPrintOffsets = True
  , foPrintEncoding = True
  , foPrintDeps = True
  , foShortHand = True
  }

--------------------------------------------------------------------------------
-- comments
fs_comm :: String -> FmtSpan
fs_comm = FmtSpan fmt_sty_comm
fmt_sty_comm :: FmtStyle
fmt_sty_comm = FmtGD

-- registers
fs_reg :: String -> FmtSpan
fs_reg = FmtSpan fmt_sty_reg
fmt_sty_reg :: FmtStyle
fmt_sty_reg = FmtCL

-- mnemonics
fs_kw0 :: String -> FmtSpan
fs_kw0 = FmtSpan fmt_sty_kw0
fmt_sty_kw0 :: FmtStyle
fmt_sty_kw0 = FmtYL

-- controls, subops, etc...
fs_kw1 :: String -> FmtSpan
fs_kw1 = FmtSpan fmt_sty_kw1
fmt_sty_kw1 :: FmtStyle
fmt_sty_kw1 = FmtCD

-- literals, labels, immediates
fs_lit :: String -> FmtSpan
fs_lit = FmtSpan fmt_sty_lit
fmt_sty_lit :: FmtStyle
fmt_sty_lit = FmtBL

fmt_sty_err :: FmtStyle
fmt_sty_err = FmtRL

fs_none :: String -> [FmtSpan]
fs_none = (\c -> [c]) . FmtSpan FmtNONE

--------------------------------------------------------------------------------
fmtSpansRawInstWith :: FmtOpts -> RawInst -> [FmtSpan]
fmtSpansRawInstWith fos ri = fssSimplify $
   op_part_padded ++ maybe_dep_info ++ fs_none ";" ++ maybe_sanity_hint
  where op_part_padded
          | null maybe_dep_info = op_part
          | otherwise = fssPadR (foColsPerInstBody fos) op_part
        op_part = pred ++ fs_none " " ++ fmtd_tokens
          where pred = fssPadR (foColsPerPredicate fos) ps
                  where ps
                          | null (riPredication ri) = []
                          | otherwise = fmtOpnd fos (riPredication ri)

        -- ghetto move instruction on newer processors
        maybe_sanity_hint :: [FmtSpan]
        maybe_sanity_hint =
          case (riMnemonic ri,riOperands ri) of
            ("HFMA2.MMA",[_,"-RZ","RZ",hf_hi16,hf_lo16]) ->
              case (reads hf_hi16,reads hf_lo16) of
                ([(hi_f,"")], [(lo_f,"")]) ->
                    [fs_comm $ printf " // 0x%08X" (to_hf16_w 16 hi_f .|. to_hf16_w 0 lo_f)]
                  where to_hf16_w sh =
                          (`shiftL`sh) . (fromIntegral :: Word16 -> Word32) .
                            floatBitsToHalfBits RoundE . floatToBits
            _ -> []

        fmtd_tokens :: [FmtSpan]
        fmtd_tokens
          | null (riOperands ri) = mne
          | otherwise = mne_padded ++ fs_none "  " ++
            fmtOpnds 0 (fssLength mne_padded - cols_per_mne) (riOperands ri)
          where mne_padded = fssPadR cols_per_mne mne

        fmtOpnds :: Int -> Int -> [String] -> [FmtSpan]
        fmtOpnds ix debt (o:os) = -- trace (show debt) $
            fmtd_o ++ fmtOpnds (ix + 1) new_debt os
          where maybe_comma = if null os then [] else fs_none ", "
                fmtd_o = fssPadR (cols_per_opnd - debt) (o_simplified ++ maybe_comma)
                new_debt = debt - (cols_per_opnd - fssLength fmtd_o)

                o_simplified :: [FmtSpan]
                o_simplified = fmtOpnd fos o

        fmtOpnds _ _ [] = []
        --
        cols_per_mne = foColsPerMnemonic fos
        cols_per_opnd = foColsPerOperand fos
        --
        mne :: [FmtSpan]
        mne
          | foDecodeLop3 fos && is_lop =
              [fs_kw0 lop, fs_kw1 ("." ++ lop_func)]
          | otherwise =
              case span (/='.') (riMnemonic ri) of
                (op,'.':sfx)
                  | foShortHand fos && sfx == "MOV.U32" -> fmt "MOV"
                  | foShortHand fos && sfx == "SHL.U32" -> fmt ""
                  | otherwise -> fmt sfx
                  where fmt "" = [fs_kw0 op]
                        fmt so = [fs_kw0 op,fs_kw1 ('.':so)]
                _ -> [fs_kw0 (riMnemonic ri)]
          where is_lop :: Bool
                is_lop = any (`isPrefixOf`riMnemonic ri)
                  ["LOP3","PLOP3","ULOP3","UPLOP3"]

                lop :: String
                lop = takeWhile (/='.') (riMnemonic ri)
                --
                -- ULOP3.LUT UR12, UR6, 0x5, URZ, 0xfc, !UPT
                -- UPLOP3.LUT UP0, UPT, UPT, UPT, UPT, 0x80, 0x0
                lop_func :: String
                lop_func =
                  case drop 1 (reverse (riOperands ri)) of
                    [] -> "LUT"
                    op_str:_ ->
                      case reads op_str of
                        [(x,"")] | x <= 255 -> "(" ++ fmtLop3 x ++ ")"
                        _ -> "LUT"
        --
        maybe_dep_info :: [FmtSpan]
        maybe_dep_info
          | not (null (riDepInfo ri)) && foPrintDeps fos =
            fs_none (" {" ++ intercalate "," (riDepInfo ri) ++ "}")
          | otherwise = []


fmtOpnd :: FmtOpts -> String -> [FmtSpan]
fmtOpnd fos o
  | not (foShortHand fos) = fs_none o
  | ".reuse"`isSuffixOf`o = fmtOpnd fos $ take (length o - 2) o
  | "c[0x0]["`isPrefixOf`o = highlightConst o
  | "c[0x1]["`isPrefixOf`o = highlightConst o
  | "c[0x2]["`isPrefixOf`o = highlightConst o
  | "-c[0x0]["`isPrefixOf`o = highlightConst o
  | "-c[0x1]["`isPrefixOf`o = highlightConst o
  | "-c[0x2]["`isPrefixOf`o = highlightConst o
  | "`"`isPrefixOf`o = [fs_lit o] -- label
  | "["`isPrefixOf`o || "desc["`isPrefixOf`o = highlightAddr o
  | otherwise =
    case regSpan o of
      Just (reg,sfx) -> fs_reg reg:rest
         -- RET.REL.NODEC  R2 `(run_query_space) ... ;
         -- this is "one" operand (no comma)
        where rest =
                case span (/='`') sfx of
                  (pfx,sfx@('`':_)) -> fs_none pfx ++ fmtOpnd fos sfx
                  _ -> fs_none sfx
      _ -> highlightAllRegs o
  where reg r = [fs_reg r]

        highlightConst :: String -> [FmtSpan]
        highlightConst ('-':cs) = fs_none "-" ++ highlightConst cs
        highlightConst s@('|':cs) =
          case span (/='|') cs of
            (body,"|") -> fs_none "|" ++ highlightConst body ++ fs_none "|"
            _ -> fs_none s
        highlightConst s
          | "c[0x0]["`isPrefixOf`s = [fs_lit $ "c0" ++ drop 6 s]
          | "c[0x1]["`isPrefixOf`s = [fs_lit $ "c1" ++ drop 6 s]
          | "c[0x2]["`isPrefixOf`s = [fs_lit $ "c2" ++ drop 6 s]

     -- e.g. desc[UR6][R2.64]
     -- e.g. [R16.64+0x100] or [R88.X16+UR18+0x400]
        highlightAddr :: String -> [FmtSpan]
        highlightAddr o =
          case span (/='[') o of
            -- pfx = "" or "desc"
            ("",'[':sfx) ->
                fs_none "[" ++ highlightAllRegs sfx
            ("desc",'[':sfx) ->
                [fs_kw1 "desc"] ++ fs_none "[" ++ highlightAllRegs sfx
            _ -> fs_none o

        highlightAllRegs :: String -> [FmtSpan]
        highlightAllRegs = loop ""
          where loop :: String -> String -> [FmtSpan]
                loop rfs_none "" = fs_none (reverse rfs_none)
                loop rfs_none s@(c:cs) =
                    case s of
                      -- e.g. [R16.64...]
                      '.':'6':'4':sfx -> matchToken [fs_kw1 ".64"] sfx
                      -- e.g. SLM scaling suffix
                      '.':'X':'2':sfx -> matchToken [fs_kw1 ".X2"] sfx
                      '.':'X':'4':sfx -> matchToken [fs_kw1 ".X4"] sfx
                      '.':'X':'1':'6':sfx -> matchToken [fs_kw1 ".X16"] sfx
                      _ ->
                        case regSpan s of
                          Just (rn,sfx) -> matchToken (reg rn) sfx
                          Nothing ->
                            case s of
                              '0':'x':d0:sfx | isHexDigit d0 ->
                                case span isHexDigit sfx of
                                  (hex_ds,sfx) ->
                                    matchToken [fs_lit ("0x"++d0:hex_ds)] sfx
                              _ -> loop (c:rfs_none) cs
                  where matchToken ::  [FmtSpan] -> String -> [FmtSpan]
                        matchToken tks sfx = maybe_nfmt ++ tks ++ loop "" sfx
                          where maybe_nfmt = if null rfs_none then [] else fs_none (reverse rfs_none)

-- e.g. RZ, R2, R2+0x100, R2+UR6, R2+UR6+0x100
regSpan :: String -> Maybe (String,String)
regSpan s =
  case s of
    'R':'Z':sfx | notIdentChar sfx -> Just ("RZ",sfx)
    'P':'T':sfx | notIdentChar sfx -> Just ("PT",sfx)
    'U':'P':'T':sfx  | notIdentChar sfx -> Just ("UPT",sfx)
    'U':'R':'Z':sfx  | notIdentChar sfx -> Just ("URZ",sfx)
    --
    'S':'B':sfx -> tryReg "SB" sfx
    'U':'R':sfx -> tryReg "UR" sfx
    'U':'P':sfx -> tryReg "UP" sfx
    'R':sfx -> tryReg "R" sfx
    'P':sfx -> tryReg "P" sfx
    'B':sfx -> tryReg "B" sfx
    -- 'R':'p':'c':sfx -> Just ("Rpc",sfx)
    _ -> tryKeywords keywords
  where tryReg pfx sfx =
          case span isDigit sfx of
            (ds@(_:_),sfx) | notIdentChar sfx -> Just (pfx ++ ds,sfx)
            _ -> Nothing
        notIdentChar "" = True
        notIdentChar (c:_) = not (isAlphaNum c || c == '_')

        tryKeywords [] = Nothing
        tryKeywords (kw:kws)
          | kw`isPrefixOf`s =
            case splitAt (length kw) s of
              (kw1,sfx) | kw1 == kw && notIdentChar sfx -> Just (kw,drop (length kw) s)
              _ -> tryKeywords kws
          | otherwise = tryKeywords kws

        keywords :: [String]
        keywords =
          [
            "SR_TID.X"
          , "SR_TID.Y"
          , "SR_TID.Z"
          --
          , "SR_CgaCtaId"
          , "SR_SWINLO"
          , "SR_SWINHI"
          --
          , "SR_GLOBALTIMERLO"
          ]



fmtSampleInst :: SampleInst -> String
fmtSampleInst = fmtSampleInstWithOpts dft_fos

fmtSampleInstWithOpts :: FmtOpts -> SampleInst -> String
fmtSampleInstWithOpts fos = fssToString . fmtSampleInstToFmtSpans fos

fmtSampleInstToFmtSpans :: FmtOpts -> SampleInst -> [FmtSpan]
fmtSampleInstToFmtSpans fos si =
    ln_pfx ++ fmtSpansRawInstWith fos (siRawInst si) ++ bits
  where ln_pfx
          | foPrintOffsets fos = [fs_comm $ printf "  /*%04X*/ " (riOffset (siRawInst si))]
          | otherwise = []

        bits
          | foPrintEncoding fos = [fs_comm $ printf "  /* %016X`%016X */" (wHi64 (siBits si)) (wLo64 (siBits si))]
          | otherwise = []



-- puts the bits in front of the instruction
-- FFFFFFF000007947`0X000FC0000383FFFF:     BRA `(.L_1);  {Y};
fmtSampleInstPrefixed :: SampleInst -> String
fmtSampleInstPrefixed si =
    bits ++ fmtSampleInstWithOpts fos si
  where bits :: String
        bits = printf "%016X`%016X:  " (wHi64 (siBits si)) (wLo64 (siBits si))
        fos =
          dft_fos {
            foPrintDeps = False
          , foPrintEncoding = False
          }

decodeDepInfo :: Word128 -> [String]
decodeDepInfo w128 = tokens
  where tokens = filter (not . null) [
            stall_tk
          , yield_tk
          , wr_alloc_tk
          , rd_alloc_tk
          , wait_mask_tk
          ]

        control_bits = getField128 (128 - 23) 23 w128
        -- Volta control words are the top 23 bits.  The meaning is pretty much
        -- unchanged since Maxwell (I think).
        --   https://arxiv.org/pdf/1804.06826
        --   https://github.com/NervanaSystems/maxas/wiki/Control-Codes
        --
        --  [22:21] / [127:126] = MBZ
        --  [20:17] / [125:122] = .reuse
        --  [16:11] / [121:116] = wait barrier mask
        --  [10:8]  / [115:113] = read barrier index
        --  [7:5]   / [112:110] = write barrier index
        --  [4]     / [109]     = !yield (yield if zero)
        --  [3:0]   / [108:105] = stall cycles (0 to 15)
        --
        -- After seeing DEPBAR.LE and that those SB operands,
        -- then we have an idea that barrier indices are SB0..SB5
        -- (instead of 1..6).  So the encoded value *is* the barrier id.
        stall_tk
          | stalls == 0 = ""
          | otherwise = "!" ++ show stalls
          where stalls = getField64 0 4 control_bits
        yield_tk
          | getField64 4 1 control_bits == 0 = "Y"
          | otherwise = ""
        wr_alloc_tk = mkAllocTk ".W" 5
        rd_alloc_tk = mkAllocTk ".R" 8
        mkAllocTk sfx off
          | enc == 7 = "" -- I guess 7 is their ignored value
          | otherwise = "+" ++ show enc ++ sfx
          where enc = getField64 off 3 control_bits
        wait_mask_tk
          | null indices = ""
          | otherwise = intercalate "," (map (\bid -> "^" ++ show bid) indices)
          where indices = filter (testBit wait_mask) [0..5]
                 where wait_mask = getField64 11 6 control_bits


-- TODO: use Parsec
--
-- parses a raw instruction
parseRawInst :: String -> Either String RawInst
parseRawInst = fmap fst . parseRawInst'
parseRawInst' :: String -> Either String (RawInst,String)
parseRawInst' = parseSyntax . skipWs
  where parseSyntax :: String -> Either String (RawInst,String)
        parseSyntax =
            parsePredication $
              RawInst {
                riOffset = -1
              , riLine = 0
              , riPredication = ""
              , riMnemonic = ""
              , riOptions = []
              , riOperands = []
              , riDepInfo = []
              }
          where parsePredication :: RawInst -> String -> Either String (RawInst,String)
                parsePredication ri sfx = -- F2FP.BF16.PACK_AB
                  case skipWs sfx of
                    '@':sfx ->
                      case span (\c -> c == '!' || isAlphaNum c) sfx of
                        (pred,sfx) -> parseMnemonic (ri{riPredication = "@" ++ pred}) (skipWs sfx)
                    s -> parseMnemonic ri (skipWs s)

                parseMnemonic :: RawInst -> String -> Either String (RawInst,String)
                parseMnemonic ri sfx =
                  case span (\c -> isAlphaNum c || c`elem`"._(~&^|)") (skipWs sfx) of
                    (mne,sfx) -> parseOperands (ri{riMnemonic = mne}) (skipWs sfx)

                parseOperands :: RawInst -> String -> Either String (RawInst,String)
                parseOperands ri sfx =
                    case trimWs sfx of
                      -- end of instruction
                      "" -> return (ri,"")
                      --
                      -- end of instruction
                      ';':sfx -> return (ri,dropWhile isSpace sfx)
                      --
                      -- start next operand
                      ',':sfx -> parseOperands ri (trimWs sfx)
                      --
                      -- either start of dep info or special case DEPBAR
                      '{':sfx
                        | "DEPBAR"`isInfixOf`riMnemonic ri ->
                          case span (/='}') sfx of
                            (body,'}':sfx1)
                              | all (\c -> isDigit c || c `elem` ",") body ->
                                parseOperands (ri{riOperands = riOperands ri ++ ["{" ++ body ++ "}"]}) (trimWs sfx1)
                              | otherwise -> addDepInfo sfx
                            _ -> addDepInfo sfx
                        | otherwise -> addDepInfo sfx
                      --
                      -- normal operand
                      sfx -> handleNormalOp sfx
                  where handleNormalOp sfx =
                            case span (not . (`elem`",;{")) sfx of
                              (op,sfx) -> addOperand op sfx
                          where addOperand op sfx = parseOperands ri1 (trimWs sfx)
                                  where ri1 = ri{riOperands = riOperands ri ++ [trimWs op]}

                        addDepInfo sfx =
                          case span (/='}') sfx of
                            (opts,'}':sfx) -> parseOperands (ri {riDepInfo = ws}) (skipWs sfx)
                              where ws = words (map (\c -> if c == ',' then ' ' else c) opts)
                            (opts,sfx) -> Left $ "expected } ==" ++ show (opts,sfx)


parseSampleInst :: String -> Either String SampleInst
parseSampleInst s = -- parseLongForm s <|> parseShortForm s
    case tryParsePrefixBits s of
        -- format that accepts
        --   000FE40000000F00`0000001E000A7202: ... syntax
      Just (w128,raw_syn) ->
        case parseRawInst raw_syn of
          Left err -> Left err
          Right ri -> Right $ SampleInst ri w128
      -- the form that has the bits in a suffix comment
      Nothing -> parseLongForm s
  where
        tryParsePrefixBits s =
            case (reads ("0x"++his),reads ("0x"++drop 1 tick_los)) of
              ([(hi64,"")],[(lo64,"")]) ->
                Just (Word128 hi64 lo64,drop 1 raw_syntax)
              _ -> Nothing
          where (bits,raw_syntax) = span (/=':') s
                (his,tick_los) = span (/='`') bits

        parseLongForm = parseOffsetOpt . dropWhile isSpace

        parseOffsetOpt :: String -> Either String SampleInst
        parseOffsetOpt s =
          case s of
            -- /*0020*/ <- no 0x prefix
            '/':'*':sfx ->
              case span isHexDigit sfx of
                (xds,'*':'/':sfx)
                  | null xds -> parseSyntax (-1) (skipWs sfx)
                  | otherwise -> parseSyntax (read ("0x"++xds)) (skipWs sfx)
                _ -> Left "expected */"
            _ -> parseSyntax (-1) s

        parseSyntax :: Int -> String -> Either String SampleInst
        parseSyntax off s =
          case parseRawInst' s of
            Left err -> Left err
            Right (ri,sfx) ->
              parseBitsSuffix (SampleInst ri{riOffset = off} (Word128 0 0)) sfx

        parseBitsSuffix :: SampleInst -> String -> Either String SampleInst
        parseBitsSuffix si sfx0 =
          case parseDualBitsInSingleComment sfx of
              Nothing ->
                case parseBitsInCommentSequence sfx of
                  Nothing -> Left "unable to parse suffix bits"
                  Just (w128,_) -> completeInstruction si{siBits = w128}
              Just (w128,_) -> completeInstruction si{siBits = w128}
            where sfx
                    | "/*" `isPrefixOf` dropWhile isSpace sfx0 =
                      case span (/='*') comment_body of
                        (body,'*':'/':sfx1)
                          | any (`isInfixOf`body) ["s0","s1","s2"] -> sfx1
                        _ -> sfx0
                    | otherwise = sfx0
                    where comment_body = drop 2 (dropWhile isSpace sfx0)

        completeInstruction :: SampleInst -> Either String SampleInst
        completeInstruction si = return $
          si{siRawInst = (siRawInst si){riDepInfo = decodeDepInfo (siBits si)}}


-- /* F123...`AFD0... */
parseDualBitsInSingleComment :: String -> Maybe (Word128,String)
parseDualBitsInSingleComment s =
  case dropWhile isSpace s of
    '/':'*':sfx ->
      case span isHexDigit (dropWhile isSpace sfx) of
        (ds0,sfx)
          | null ds0 -> Nothing
          | otherwise ->
            case dropWhile isSpace sfx of
              '`':sfx ->
                case span isHexDigit sfx of
                  (ds1,sfx) ->
                    case dropWhile isSpace sfx of
                      '*':'/':sfx -> return (w128,dropWhile isSpace sfx)
                        where w128 = Word128 (read ("0x"++ds0)) (read ("0x"++ds1))
                      _ -> Nothing
              _ -> Nothing
    _ -> Nothing

-- reverse order (low bits first)
-- /* 0x1234 */ /* 0x789A */
parseBitsInCommentSequence :: String -> Maybe (Word128,String)
parseBitsInCommentSequence s0 = do
  (w0,s1) <- parseHexInComment s0
  (w1,s2) <- parseHexInComment s1
  return (Word128 w1 w0,s2)

-- /* 0x1234 */
parseHexInComment :: String -> Maybe (Word64,String)
parseHexInComment s =
  case dropWhile isSpace s of
    '/':'*':sfx ->
      case dropWhile isSpace sfx of
        '0':'x':sfx ->
          case span isHexDigit sfx of
            (ds,sfx) ->
              case dropWhile isSpace sfx of
                '*':'/':sfx ->
                  return (read ("0x" ++ ds), dropWhile isSpace sfx)
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing

trimWs :: String -> String
trimWs = reverse .  dropWhile isSpace . reverse .  dropWhile isSpace

padR :: Int -> String -> String
padR k s = s ++ replicate (k - length s) ' '
padL :: Int -> String -> String
padL k s = replicate (k - length s) ' ' ++ s

skipWs :: String -> String
skipWs [] = []
skipWs ('/':'*':sfx) = skipComment sfx
  where skipComment [] = []
        skipComment ('*':'/':sfx) = skipWs sfx
        skipComment (_:cs) = skipComment cs
skipWs (c:cs)
  | isSpace c = skipWs cs
  | otherwise = c:cs




--   /*0c50*/ @P0    FFMA R5, R0, 1.84467440737095516160e+19, RZ  {@6,Y} ;   /* 000fcc00000000ff`5f80000000050823 */
-- tryParseFilteredRawInst :: String -> Maybe RawInst
-- tryParseFilteredRawInst s =
--     case (dropToSyntax s) of
--
--   where dropToSyntax =
--           dropWhile isSpace . drop 1 .
--             dropWhile (/='/') . drop 1 . dropWhile (/='/')

getTempFile :: IO FilePath
getTempFile = do
-- TODO: need a temp file
  temp_dir <- getTemporaryDirectory
  (fp,h) <- openTempFile temp_dir "nvaa.bin"
  hClose h
  return fp

disBits :: Opts -> Word128 -> IO String
disBits os w128 = head <$> disBitsBatch os [w128]

disBitsBatch :: Opts -> [Word128] -> IO [String]
disBitsBatch os w128s = do
  let addDeps :: (Word128,String) -> String
      addDeps (w128,syn) = syn ++ deps
        where deps = " {" ++ intercalate "," (decodeDepInfo w128) ++ "};"
  map addDeps . zip w128s <$> disBitsRawBatch os w128s

disBitsRaw :: Opts -> Word128 -> IO String
disBitsRaw os w128 = head <$> disBitsRawBatch os [w128]

-- need to fallback on failure and binary search
disBitsRawBatch :: Opts -> [Word128] -> IO [String]
disBitsRawBatch os w128s = setup
  where setup = do
          temp_file <- getTempFile
          ss <- decodeChunk temp_file (length w128s) w128s
          removeFile temp_file
          return ss

        decodeChunk :: FilePath -> Int -> [Word128] -> IO [String]
        decodeChunk _         _       [] = return []
        decodeChunk temp_file chunk_len w128s = do
          let (w128s_chunk,w128s_sfx) = splitAt chunk_len w128s
              bs = BS.concat (map toByteStringW128 w128s_chunk)
          BS.writeFile temp_file bs
          (ec,oup,err) <-
            runCudaToolWithExitCode os "nvdisasm" [
                    "--no-vliw"
                 -- , "--print-instruction-encoding"
                  , "--no-dataflow"
                  , "--binary=" ++ filter (/='_') (map toUpper (oArch os))
                  , temp_file
                  ]
          case ec of
            ExitFailure _
              | chunk_len == 1 -> do
                  -- failed at the smallest value, it's a legit error
                  debugLn os $
                    "*** nvdisasm failed chunk (" ++ show chunk_len ++ ") ***"
                  let this_error = (\ls -> if null ls then "???" else head ls) (lines err)
                  (this_error:) <$>
                    decodeChunk temp_file chunk_len w128s_sfx
              | otherwise -> do
              -- TODO:
                -- nvdisasm error   : Unrecognized operation for functional unit 'uC' at address 0x00000000
                -- can use the address to intelligently skip ahead
                --
                -- take everything up to that address and assemble as a chunk, then
                --
                -- split it in half and try again
                debugLn os $
                  "*** nvdisasm failed chunk (" ++ show chunk_len ++ ") ***"
                decodeChunk temp_file (chunk_len`div`2) w128s
            ExitSuccess -> do
              debugLn os $
                "*** nvdisasm succeeded with chunk(" ++ show chunk_len ++ ") ***"
              -- 1. we get a header line with .headerflags first, drop it and emit rest
              -- 2. in some strange cases nvdisasm just fails to produce output
              let filterOutput :: String -> [String]
                  filterOutput =
                      pad . map (removeSemi . trimWs . skipWs) . drop 1 . lines
                    where removeSemi s
                            | null s = s
                            | last s == ';' = trimWs (init s)
                            | otherwise = s
                          pad lns = lns ++ replicate (length w128s_chunk - length lns) pad_ln
                            where pad_ln = "*** nvdisasm failed to produce output ***"

              -- hope we got past the error and reduce size
              (filterOutput oup++) <$>
                decodeChunk
                  temp_file (min (chunk_len*2) (length w128s_sfx)) w128s_sfx
