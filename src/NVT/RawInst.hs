module NVT.RawInst where

import NVT.Word128

import Data.Bits
import Data.Char
import Data.List
import Data.Word
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

fmtRiWithControlInfo :: RawInst -> String
fmtRiWithControlInfo ri =
    ln_pfx ++ ln_spaces ++ bits ++ "\n"
  where ln_pfx = printf "  /*%04x*/ " (riOffset ri) ++ syntax
        syntax = pred ++ " " ++ mne ++ " " ++ intercalate ", " (riOperands ri) ++ " " ++ control_info ++ " ;"
          where pred = printf "%-5s" (riPredication ri)
                mne = riMnemonic ri -- we could pad here
                control_info = "{" ++ intercalate "," tokens ++ "}"
                  where control_bits = getField128 (128 - 23) 23 (riBits ri)
                        -- Volta control words are the top 23 bits
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
                        -- https://arxiv.org/pdf/1804.06826
                        -- https://github.com/NervanaSystems/maxas/wiki/Control-Codes

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
            (xds,'*':'/':sfx) -> parseSyntax ri0 sfx
              where ri0 =
                      RawInst {
                        riOffset = read ("0x"++xds)
                      , riPredication = ""
                      , riMnemonic = ""
                      , riOperands = []
                      , riBits = Word128 0 0
                      }
            _ -> Nothing
      _ -> Nothing
  where isAddrPfx ('/':'*':ds) =
          case span isDigit (dropWhile isSpace ds) of
            (ds,'*':'/':sfx) -> True
            _ -> False

        parseSyntax :: RawInst -> String -> Maybe RawInst
        parseSyntax = parsePredication
          where parsePredication :: RawInst -> String -> Maybe RawInst
                parsePredication ri sfx =
                  case dropWhile isSpace sfx of
                    '@':sfx ->
                      case span (\c -> c == '!' || isAlphaNum c) sfx of
                        (pred,sfx) -> parseMnemonic (ri{riPredication = pred}) sfx
                    s -> parseMnemonic ri s
                parseMnemonic :: RawInst -> String -> Maybe RawInst
                parseMnemonic ri sfx =
                  case span (\c -> isAlphaNum c || c == '.') (dropWhile isSpace sfx) of
                    (mne,sfx) -> parseOperands (ri{riMnemonic = mne}) sfx

                parseOperands :: RawInst -> String -> Maybe RawInst
                parseOperands ri sfx =
                  case dropWhile isSpace sfx of
                    (';':sfx) -> parseBits ri sfx
                    s ->
                      case span (not . (`elem`",;")) s of
                        (op,sfx) ->
                            case dropWhile isSpace sfx of
                              ',':sfx -> parseOperands ri1 sfx
                              ';':sfx -> parseBits ri1 sfx
                              "" -> Nothing
                          where ri1 = ri{riOperands = riOperands ri ++ [op]}

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