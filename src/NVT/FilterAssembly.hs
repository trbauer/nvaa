module NVT.FilterAssembly where

import NVT.RawInst

import Control.Applicative
import Data.Bits
import Data.Array
import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Printf

type SrcDict = [(FilePath,Array Int String)]


-- "//## File "D:\\dev\\nvaa\\cuda-tests/cf.cu", line 62"
tryParseLineMapping :: String -> Maybe (FilePath,Int)
tryParseLineMapping ln
  | lno_pfx `isPrefixOf` (dropWhile isSpace ln) =
    case reads sfx_file :: [(String,String)] of
      [(file,sfx)] ->
        case reads (dropWhile (not . isDigit) sfx) :: [(Int,String)] of
          [(lno,"")] -> Just (file,lno)
          _ -> Nothing
      _ -> Nothing
  | otherwise = Nothing
  where lno_pfx = "//## File "
        sfx_file = dropWhile (/='"') ln

lm_test = "//## File \"D:\\\\dev\\\\nvaa\\\\cuda-tests/cf.cu\", line 62"


-- maxwell =
--   "JUNK\n" ++
--   "                                                                                         /* 0x001c7c00e22007f6 */\n" ++
--   "        /*0008*/                   MOV R1, c[0x0][0x20] ;                                /* 0x4c98078000870001 */\n" ++
--   "        /*0010*/                   S2R R0, SR_CTAID.X ;                                  /* 0xf0c8000002570000 */\n" ++
--   "        /*0018*/                   S2R R2, SR_TID.X ;                                    /* 0xf0c8000002170002 */"
--   "JUNK\n" ++
type FilterProcessorIO = SrcDict -> [(Int,String)] -> IO ()
type Arch = String

filterAssemblyWithInterleavedSrcIO :: Bool -> Handle -> Arch -> String -> IO ()
filterAssemblyWithInterleavedSrcIO no_bits h_out arch = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> IO ()
        processLns
          -- | arch >= "sm_80" = hPutStr h_out . unlines . map snd
          | arch >= "sm_70" = processLns128B    empty_dict
          | arch >= "sm_50" = processLnsMaxwell empty_dict
          | otherwise = hPutStr h_out . unlines . map snd

        empty_dict :: SrcDict
        empty_dict = []

        emitLn :: String -> IO ()
        emitLn = hPutStrLn h_out

        -- Volta, Turing, and Ampere
        processLns128B :: FilterProcessorIO
        processLns128B _ [] = do
          return ()
        processLns128B dict lns@((_,ln0str):(_,ln1str):lns_sfx) =
          case parseSampleInst (ln0str ++ ln1str) of
            Right si -> do
              emitLn $ fmtSi si
              processLns128B dict lns_sfx
            Left _ -> tryProcessLineMapping dict lns processLns128B
        processLns128B dict [(_,lnstr)] = emitLn lnstr >> processLns128B dict []

        fmtSi :: SampleInst -> String
        fmtSi si
          | no_bits = fmtRawInst (siRawInst si)
          | otherwise = fmtSampleInst si

        tryProcessLineMapping :: SrcDict -> [(Int,String)] -> FilterProcessorIO -> IO ()
        tryProcessLineMapping dict0 ((_,lnstr):lns) cont =
          case tryParseLineMapping lnstr of
            Nothing -> emitLn lnstr >> cont dict0 lns
            Just (file,lno) -> lookupMapping dict0
              where lookupMapping :: SrcDict -> IO ()
                    lookupMapping dict =
                      case file `lookup` dict of
                        Nothing -> do
                          src_lns <- lines <$> readFile file
                          let arr = listArray (1,length src_lns) src_lns :: Array Int String
                          lookupMapping ((file,arr):dict)
                        Just arr -> do
                          let src_line = "  //" ++ (arr ! lno)
                          emitLn $ lnstr ++ "\n" ++ src_line
                          cont dict lns

        -- Maxwell and Pascal
        processLnsMaxwell :: FilterProcessorIO
        processLnsMaxwell dict [] = return ()
        processLnsMaxwell dict ((_,lnstr):lns)
          | "/* 0x" `isPrefixOf` s && mw64 /= Nothing =
            case mw64 of
              Just (w64,_) -> parseInstGroup deps lns
                where deps = [
                           w64                .&. 0x000000000001FFFF
                        , (w64`shiftR`21)     .&. 0x000000000001FFFF
                        , (w64`shiftR`(2*21)) .&. 0x000000000001FFFF
                        ]
          where s = dropWhile isSpace lnstr
                mw64 = parseHexInComment s

                parseInstGroup [] lns = processLnsMaxwell dict lns -- done with group of 3
                parseInstGroup _ [] = processLnsMaxwell dict lns
                parseInstGroup (dep:deps) ((lno,lnstr):lns) =
                    -- since the Maxwell and Volta control fields have the same encoding
                    -- we can cheat and just synthesize the "high 64b" of the Volta word
                    -- and using that in tryParseInstructionLines.
                    case parseSampleInst (lnstr ++ fake_second_line) of
                      Left _ ->
                        -- e.g. parse failure just means the end of the function
                        -- NOTE: we might drop unused control codes here; that's okay
                        processLnsMaxwell dict ((lno,lnstr):lns)
                      Right si -> do
                        emitLn $ fmtSampleInst si
                        parseInstGroup deps lns

                  where fake_second_line = printf "  /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell dict lns = tryProcessLineMapping dict lns processLnsMaxwell



filterAssembly :: Bool -> String -> String -> String
filterAssembly no_bits arch = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> String
        processLns
          | arch >= "sm_70" = processLns128B
          | arch >= "sm_50" = processLnsMaxwell
          | otherwise = unlines . map snd

        fmtSi :: SampleInst -> String
        fmtSi si
          | no_bits = fmtRawInst (siRawInst si)
          | otherwise = fmtSampleInst si

        -- Volta and Turing
        processLns128B [] = ""
        processLns128B ((lno0,ln0str):(lno1,ln1str):lns) =
          case parseSampleInst (ln0str ++ ln1str) of
            Right si ->
              fmtSampleInst si ++ "\n" ++
              processLns lns
            Left _ ->
              ln0str ++ "\n" ++
              processLns ((lno1,ln1str):lns)
--          case tryParseInstructionLines ln0str ln1str of
--            Nothing ->
--              ln0str ++ "\n" ++
--              processLns ((lno1,ln1str):lns)
        processLns128B ((_,lnstr):lns) = lnstr ++ processLns lns

        -- Maxwell and Pascal
        processLnsMaxwell [] = ""
        processLnsMaxwell ((_,lnstr):lns)
          | "/* 0x" `isPrefixOf` s && mw64 /= Nothing =
            case mw64 of
              Just (w64,_) -> parseInstGroup deps lns
                where deps = [
                           w64                .&. 0x000000000001FFFF
                        , (w64`shiftR`21)     .&. 0x000000000001FFFF
                        , (w64`shiftR`(2*21)) .&. 0x000000000001FFFF
                        ]
          where s = dropWhile isSpace lnstr
                mw64 = parseHexInComment s

                parseInstGroup [] lns = processLnsMaxwell lns -- done with group of 3
                parseInstGroup _ [] = ""
                parseInstGroup (dep:deps) ((lno,lnstr):lns) =
                    -- since the Maxwell and Volta control fields have the same encoding
                    -- we can cheat and just synthesize the "high 64b" of the Volta word
                    -- and using that in tryParseInstructionLines.
                    case parseSampleInst (lnstr ++ fake_second_line) of
                      Left _ ->
                        -- e.g. parse failure just means the end of the function
                        -- NOTE: we might drop unused control codes here
                        processLnsMaxwell ((lno,lnstr):lns)
                      Right si ->
                        fmtSampleInst si ++ "\n" ++
                        parseInstGroup deps lns
                  where fake_second_line = printf "  /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell ((_,lnstr):lns) =
          lnstr ++ "\n" ++
          processLnsMaxwell lns

