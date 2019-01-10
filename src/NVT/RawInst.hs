module NVT.RawInst where

import NVT.Lop3
import NVT.Word128

import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import Text.Printf

data RawInst =
  RawInst {
    riOffset :: !Int
--  , riSyntax :: !String
  , riPredication :: !String
  , riMnemonic :: !String -- includes any . suffixes
  , riOperands :: ![String]
  , riBits :: !Word128
  } deriving Show

--
-- data Inst =
--   Inst {
--     iPredication :: !(Maybe PReg)
--   , iMnemonic :: !(...)
--
--   , iOperands :: !Operands
--   }
--
-- data Operands =
--     Operands_RRRR !Reg !Reg !Reg  !Reg
--   | Operands_RRRC !Reg !Reg !Reg  !Const
--   | Operands_RRRI !Reg !Reg !Reg  !Imm
--   | Operands_PRRRP !PReg !Reg !Reg !Reg !Reg !PReg   e.g. ISETP
--   |
--    ..
--   | Operands_Load !Reg !Reg !UReg !Imm -- LD.... [R12 + UR2 + 0x120]    URZ means none



fmtRawInstWithOpts :: Bool -> RawInst -> String
fmtRawInstWithOpts ctl_info ri =
      pred ++ " " ++ riMnemonic ri ++ " " ++ intercalate ", " (riOperands ri) ++ control_info ++ " ;"
  where pred = printf "%-5s" (riPredication ri)
        control_info
          | ctl_info = " {" ++ intercalate "," tokens ++ "}"
          | otherwise = ""
          where control_bits = getField128 (128 - 23) 23 (riBits ri)
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
                tokens = filter (not . null) [
                    stall_tk
                  , yield_tk
                  , wr_alloc_tk
                  , rd_alloc_tk
                  , wait_mask_tk
                  ]


                stall_tk
                  | stalls == 0 = ""
                  | otherwise = "@" ++ show stalls
                  where stalls = getField64 0 4 control_bits
                yield_tk
                  | getField64 4 1 control_bits == 0 = "Y"
                  | otherwise = ""
                wr_alloc_tk = mkAllocTk "W" 5
                rd_alloc_tk = mkAllocTk "R" 8
                mkAllocTk nm off
                  | val == 7 = "" -- I guess 7 is their ignored value
                  | otherwise = "v" ++ nm ++ show (val + 1)
                  where val = getField64 off 3 control_bits
                wait_mask_tk
                  | null indices = ""
                  | otherwise = intercalate "," (map (\i -> "^" ++ show (i + 1)) indices)
                  where indices = filter (testBit wait_mask) [0..5]
                         where wait_mask = getField64 11 6 control_bits


fmtRiWithControlInfo :: RawInst -> String
fmtRiWithControlInfo ri =
    ln_pfx ++ ln_spaces ++ bits ++ "\n"
  where ln_pfx = printf "  /*%04x*/ " (riOffset ri) ++ syntax ++ lop3_lut_suffix
        syntax = fmtRawInstWithOpts True ri
        lop3_lut_suffix
          | "LOP3.LUT" `isInfixOf` riMnemonic ri = lop_func
          | otherwise = ""
          where ix
                  | "P" `isPrefixOf` head (riOperands ri) = 5
                  | otherwise = 4
                lop_func =
                  case reads (riOperands ri !! ix) of
                    [(x,"")] -> "/* " ++ fmtLop3 x ++ " */"
                    _ -> ""
        ln_spaces = replicate (90 - length ln_pfx) ' '
        bits = printf "/* %016x`%016x */" (wHi64 (riBits ri)) (wLo64 (riBits ri))


-- 0x001c7c00e22007f6
debugDepInfoMaxwellTriple :: Word64 -> IO ()
debugDepInfoMaxwellTriple w64 =
  putStrLn $ concatMap (\s -> s ++ "\n") (decodeDepInfoMaxwellTriple w64)
decodeDepInfoMaxwellTriple :: Word64 -> [String]
decodeDepInfoMaxwellTriple w64 =
  map formatDepInfoMaxwell [
        -- maxas uses high bits for the first instructions
        w64 .&. 0x000000000001FFFF
      , (w64`shiftR`21) .&. 0x000000000001FFFF
      , (w64`shiftR`(2*21)) .&. 0x000000000001FFFF
      ]


formatDepInfoMaxwell :: Word64 -> String
formatDepInfoMaxwell control_info =
    "{" ++ intercalate "," tokens ++ "}"
  where getField off len = getField64 off len control_info

        tokens = filter (not . null) $ [
            stall_tk
          , yield_tk
          , wr_alloc_tk
          , rd_alloc_tk
          , wait_mask_tk
          ]
        stall_tk
          | stalls == 0 = ""
          | otherwise = "@" ++ show stalls
          where stalls = getField 0 4
        yield_tk
          | getField 4 1 == 0 = "Y"
          | otherwise = ""
        wr_alloc_tk = mkAllocTk "W" 5
        rd_alloc_tk = mkAllocTk "R" 8
        mkAllocTk nm off
          | val == 7 = "" -- I guess 7 is their ignored value
          | otherwise = "v" ++ nm ++ show (val + 1)
          where val = getField off 3
        wait_mask_tk
          | null indices = ""
          | otherwise = intercalate "," (map (\i -> "^" ++ show (i + 1)) indices)
          where indices = filter (testBit wait_mask) [0..5]
                 where wait_mask = getField 11 6

-- s0 = "        /*0000*/                   MOV R1, c[0x0][0x28] ;                                    /* 0x00000a0000017a02 */"
-- s1 = "                                                                                             /* 0x000fd00000000f00 */"

tryParseInstructionLines :: String -> String -> Maybe RawInst
tryParseInstructionLines ln0 ln1 =
    case span isSpace ln0 of
      (spaces,'/':'*':ds) ->
          case span isHexDigit ds of
            (xds,'*':'/':sfx) -> parseSyntax (read ("0x"++xds)) sfx
            _ -> Nothing
      _ -> Nothing
  where isAddrPfx ('/':'*':ds) =
          case span isDigit (dropWhile isSpace ds) of
            (ds,'*':'/':sfx) -> True
            _ -> False

        parseSyntax :: Int -> String -> Maybe RawInst
        parseSyntax off sfx0 = do
          (ri0,sfx) <- parseRawInstNoSuffix sfx0
          let ri =
                RawInst {
                  riOffset = off
                , riPredication = riPredication ri0
                , riMnemonic = riMnemonic ri0
                , riOperands = riOperands ri0
                , riBits = Word128 0 0
                }
          parseBits ri sfx

        parseBits :: RawInst -> String -> Maybe RawInst
        parseBits ri sfx = do
          w_lo <- parseSlashStarHexWord (dropWhile isSpace sfx)
          w_hi <- parseSlashStarHexWord (dropWhile isSpace ln1)
          return $ ri{riBits = Word128 w_hi w_lo}

--- "/* 0x123 */" -> 0x123
parseSlashStarHexWord :: String -> Maybe Word64
parseSlashStarHexWord ('/':'*':ds) =
 case reads (dropWhile isSpace ds) of
  [(x,sfx)] ->
    case dropWhile isSpace sfx of
      ('*':'/':_) -> Just x
      _ -> Nothing
  _ -> Nothing

skipWs :: String -> String
skipWs [] = []
skipWs ('/':'*':sfx) = skipComment sfx
  where skipComment [] = []
        skipComment ('*':'/':sfx) = skipWs sfx
        skipComment (_:cs) = skipComment cs
skipWs (c:cs)
  | isSpace c = skipWs cs
  | otherwise = c:cs

parseRawInst :: String -> Maybe (RawInst,String)
parseRawInst =  fmap f . parseRawInstNoSuffix
  where f (ri,sfx) = (ri,skipWs sfx)

-- parses a raw instruction, but doesn't skip whitespace at the end
parseRawInstNoSuffix :: String -> Maybe (RawInst,String)
parseRawInstNoSuffix = parseOffsetOpt . dropWhile isSpace
  where parseOffsetOpt :: String -> Maybe (RawInst,String)
        parseOffsetOpt s =
          case s of
            '/':'*':sfx ->
              case span isHexDigit sfx of
                (digs,'*':'/':sfx)
                  | null digs -> parseSyntax 0 (skipWs sfx)
                  | otherwise -> parseSyntax (read ("0x"++digs)) (skipWs sfx)
            _ -> parseSyntax 0 s

        parseSyntax :: Int -> String -> Maybe (RawInst,String)
        parseSyntax off =
            parsePredication $
              RawInst {
                riOffset = off
              , riPredication = ""
              , riMnemonic = ""
              , riOperands = []
              , riBits = Word128 0 0
              }
          where parsePredication :: RawInst -> String -> Maybe (RawInst,String)
                parsePredication ri sfx =
                  case skipWs sfx of
                    '@':sfx ->
                      case span (\c -> c == '!' || isAlphaNum c) sfx of
                        (pred,sfx) -> parseMnemonic (ri{riPredication = "@" ++ pred}) (skipWs sfx)
                    s -> parseMnemonic ri (skipWs s)

                parseMnemonic :: RawInst -> String -> Maybe (RawInst,String)
                parseMnemonic ri sfx =
                  case span (\c -> isAlphaNum c || c == '.') (skipWs sfx) of
                    (mne,sfx) -> parseOperands (ri{riMnemonic = mne}) (skipWs sfx)

                parseOperands :: RawInst -> String -> Maybe (RawInst,String)
                parseOperands ri sfx =
                  case sfx of
                    -- nullary instruction
                    ';':sfx -> return (ri,skipWs sfx)
                    s ->
                      case span (not . (`elem`",;{")) s of
                        (op,sfx) ->
                            case skipWs sfx of
                              ',':sfx -> parseOperands ri1 sfx
                              ';':sfx -> return (ri1,sfx)
                              '{':sfx ->
                                case dropWhile (/=';') (drop 1 sfx) of
                                  "" -> return (ri1,"")
                                  ';':sfx -> return (ri1,sfx)
                              "" -> Nothing
                          where ri1 = ri{riOperands = riOperands ri ++ [trimWs op]}

trimWs :: String -> String
trimWs = reverse .  dropWhile isSpace . reverse .  dropWhile isSpace

--   /*0c50*/ @P0    FFMA R5, R0, 1.84467440737095516160e+19, RZ  {@6,Y} ;   /* 000fcc00000000ff`5f80000000050823 */
-- tryParseFilteredRawInst :: String -> Maybe RawInst
-- tryParseFilteredRawInst s =
--     case (dropToSyntax s) of
--
--   where dropToSyntax =
--           dropWhile isSpace . drop 1 .
--             dropWhile (/='/') . drop 1 . dropWhile (/='/')



