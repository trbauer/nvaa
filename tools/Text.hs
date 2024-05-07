module Text where

import Data.Bits
import Data.Char
import Data.List
import Data.Word(Word8,Word32,Word64)
import Debug.Trace
import Text.Printf
import qualified Data.ByteString       as S

trimWs :: String -> String
trimWs = reverse . dropWhile isSpace . reverse . dropWhile isSpace

rspan :: (Char -> Bool) -> String -> (String,String)
rspan pr s =
  case span pr (reverse s) of
    (xs,ys) -> (reverse ys,reverse xs)

-- The venerable chunking function
chunk :: Int -> [a] -> [[a]]
chunk k as = go as
  where go [] = []
        go as = let (c,cs) = splitAt k as in c : go cs

data TextAlign =
    TextAlignLeft
  | TextAlignRight
  deriving (Show,Eq)

fmtAlignedTable :: [TextAlign] -> [[String]] -> String
fmtAlignedTable aligns rows0 = table
  where rows = padRows rows0
          where padRows :: [[String]] -> [[String]]
                padRows rs = map padRow rs
                  where longest_row = maximum $ 0 : map length rs
                        padRow cs = cs ++ replicate (longest_row - length cs) ""

        table :: String
        table = unlines (map joinRow rows)
          where joinRow :: [String] -> String
                joinRow cs =
                    concatMap (\(pad,cell_str) -> pad cell_str) (zip column_padders cs)

                column_padders :: [String -> String]
                column_padders =
                    zipWith (\f col -> f (maxCellWidth col)) pad_funcs (transpose rows)
                  where maxCellWidth :: [String] -> Int
                        maxCellWidth = maximum . map length

                        pad_funcs :: [Int -> String -> String]
                        pad_funcs = map toPadder aligns ++ repeat padL
                          where toPadder TextAlignRight = padR
                                toPadder TextAlignLeft = padL



-- Prefix a bunch of lines with some given prefix
prefixLines :: String -> String -> String
prefixLines pfx = unlines . map (pfx++) . lines

-- indents a bunch of lines
indent :: String -> String -> String
indent = prefixLines

padR :: Int -> String -> String
padR n s = s ++ replicate (n - length s) ' '

padL :: Int -> String -> String
padL n s = replicate (n - length s) ' ' ++ s

bsFrom :: [Word8] -> S.ByteString
bsFrom = S.pack

ppBs :: S.ByteString -> String
ppBs = ppHex "" 0 . S.unpack

-- Formats a bitfield from a word32 into a bit string
fmtBinaryW32 :: Int -> Word32 -> String
fmtBinaryW32 = fmtBinary
fmtBinaryW64 :: Int -> Word64 -> String
fmtBinaryW64 = fmtBinary
fmtBinary :: Bits b => Int -> b -> String
fmtBinary n bits = go (fromIntegral n - 1)
  where go :: Int -> String
        go bi
          | bi < 0 = "b"
          | testBit bits bi = '1' : go (bi-1)
          | otherwise       = '0' : go (bi-1)

-- replacing printf "%xX" since I cannot flippin track down the nondeterministic
-- printf formatting bug (probably a %x on a negative Integer value, but something
-- speculatively reduced...)
fmtHex :: (Integral f, FiniteBits f) => f -> String
fmtHex x = fmtHexWidth 1 x
-- just the digits part, without the 0x prefix
fmtHexDigits :: (Integral f, FiniteBits f) => f -> String
fmtHexDigits x = fmtHexDigitsWidth 1 x
--
-- Zero pads to the natural size
fmtHexNatural :: (Integral f, FiniteBits f) => f -> String
fmtHexNatural = ("0x"++) . fmtHexDigitsNatural
-- just the digits part, without the 0x prefix
fmtHexDigitsNatural :: (Integral f, FiniteBits f) => f -> String
fmtHexDigitsNatural x = fmtHexDigitsWidth n_nibs x
  where n_nibs = (finiteBitSize x + (4 - 1)) `div` 4
--
-- Specifies zero-padding
-- (fmtHexWidth 5) = printf %05X
fmtHexWidth :: (Integral f, FiniteBits f) => Int -> f -> String
fmtHexWidth w = ("0x"++) . fmtHexDigitsWidth w
fmtHexDigitsWidth :: (Integral f, FiniteBits f) => Int -> f -> String
fmtHexDigitsWidth n_nibs x = reverse $ fmt 0 x
  where t_nibs = (finiteBitSize x + (4 - 1)) `div` 4
        --
        -- fmt :: Int -> f -> Char
        fmt n 0 = replicate (n_nibs - n) '0' -- pad tail with 0's
        fmt n x
          | n == t_nibs = ""
          | otherwise = x_char:fmt (n+1) (x`shiftR`4)
          where x_digit = fromIntegral (x .&. 0xF) :: Int
                x_char
                  | x_digit < 0xA = chr (x_digit + ord '0')
                  | otherwise = chr (x_digit - 0xA + ord 'A')

ppHex :: String -> Int -> [Word8] -> String
-- ppHex ind stoff = unlines . map (ind++) . lines . hexString 16 ppRowStart stoff
ppHex ind stoff = hexString ppopts
  where soff = (fromIntegral stoff) :: Int
        ppRowStart off = ind ++ fmtHexWidth 8 (soff + fromIntegral off) ++ ": "
        ppopts = PPHexOpts stoff ppRowStart True 16

-- emits clusters of hex bytes
--  00 00 00 00  00 00 00 00
ppHexGroups :: Int -> Int -> [Word8] -> String
ppHexGroups bytes_per_cluster clusters_per_line = drop 1 . go 0
  where go _   [] = []
        go clu ws
          | clu == clusters_per_line = "\n" ++ drop 1 (go 0 ws)
        go clu (w0:w1:w2:w3:ws) = concatMap ppWord8 [w0,w1,w2,w3] ++ go (clu+1) ws
        go _   ws = concatMap ppWord8 ws

        ppWord8 x = " " ++ fmtHexWidth 2 x


data PPHexOpts a =
  PPHexOpts {
    pphoStartOff :: !a              -- the given start offset to report
  , pphoPpRowStart :: (a -> String) -- formats the starting column label
  , pphoAlignPadding :: !Bool       -- puts padding on the top row to maintain a column alignment
  , pphoWidth :: !a                 -- the number of bytes
  }

--
-- hexString :: Integral a => a -> (a -> String) -> a -> [Word8] -> String
-- hexString widthA ppRowStart soffA bs = concatMap (\(i,ln) -> ppRowStart (widthA * i - soffA `mod` widthA) ++ ln) ixdFmtdLns
hexString :: Integral a => PPHexOpts a -> [Word8] -> String
hexString opts bs = concatMap (\(i,ln) -> ppRowStart (widthA * i - soffA `mod` widthA) ++ ln) ixdFmtdLns
  where al = soff `mod` width -- slack space at beginning
        widthA = pphoWidth opts
        width = fromIntegral widthA
        soffA = pphoStartOff opts
        soff = fromIntegral soffA
        ppRowStart = pphoPpRowStart opts
        asoff = soff - al -- aligned start offset
        bs' = replicate al 0 ++ bs -- pad first line with slack space
        lns = chunk width (map (\b -> (ppByte b, ppChar b)) bs') -- [[(String,Char)]]

        ppLine :: [(String,Char)] -> String
        ppLine ln = intercalate " " (map fst ln) ++ " " ++ snd (unzip ln) ++ "\n"

        -- Fixup the lines padding the beginning and end lines.  This allows
        -- correct aligned and padded rendering of.
        -- 16: .. .. .. .. 12 a0 b0 00 00 00 00 00 00 10 15 00 ................
        lnsPadded
          | pphoAlignPadding opts =
              case lns of
                [] -> []
                [ln] -> [padLineSfx (padLinePfx ln)]
                (ln:lns) -> padLinePfx ln : init lns ++ [padLineSfx (last lns)]
          | otherwise = lns

        padLinePfx :: [(String,Char)] -> [(String,Char)]
        padLinePfx = replaceFirstN al ("..",' ')

        padLineSfx :: [(String,Char)] -> [(String,Char)]
        padLineSfx ln = ln ++ replicate (width - length ln) ("  ",' ')

        replaceFirstN :: Int -> b -> [b] -> [b] -- replace the first N with a constant
        replaceFirstN n b bs = replicate n b ++ drop n bs

        ixdFmtdLns = zip [0 ..] (map ppLine lnsPadded) -- [(Int,String)]

        ppByte :: Word8 -> String
        ppByte = fmtHexDigitsWidth 2

-- Converts a byte to a character if it's a printable character.
-- On Windows hPutChar (hence hPutStr) will puke if it gets certain
-- characters on which isPrint returns True.
--
ppChar :: Word8 -> Char
ppChar 0 = '.'
ppChar w
      | isPrintChar w = chr (fromIntegral w)
      | otherwise  = '.'
-- HACK: hPutChar will puke on some isPrint chars.
-- This is essentially a Windows issue I think.
-- Cases that isPrint is True, but 0xC0 explodes
-- isPrintChar w c = isPrint c ||
-- isPrintChar 0xC0 _ =  False
-- isPrintChar 0xD4 _ =  False
-- isPrintChar 0xCF _ =  False
-- ... others
isPrintChar :: Word8 -> Bool
isPrintChar w =
  w >= 0x1 && w <= 0x6 ||
    w >= 0xB && w < 0xC ||
    w >= 0xE && w <= 0x7F ||
    w >= 0xC4 && w <= 0xC7 ||
    w >= 0xA1 && w <= 0xA3 ||
    w == 0xA5 ||
    w >= 0xAA && w <= 0xAC ||
    w >= 0xB0 && w <= 0xB2 ||
    w == 0xB5 ||
    w == 0xB7 ||
    w >= 0xBA && w <= 0xBD ||
    w == 0xBF ||
    w >= 0xC4 && w <= 0xC7 ||
    w == 0xC9 ||
    w == 0xD1 ||
    w == 0xD6 ||
    w == 0xDC ||
    w >= 0xDF && w <= 0xE2 ||
    w >= 0xE4 && w <= 0xEF ||
    w >= 0xF1 && w <= 0xF4 ||
    w >= 0xF6 && w <= 0xF7 ||
    w >= 0xF9 && w <= 0xFC ||
    w == 0xFF
