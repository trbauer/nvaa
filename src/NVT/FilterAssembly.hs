module NVT.FilterAssembly where

import NVT.RawInst

import Control.Applicative
import Data.Bits
import Data.Array
import Data.Char
import Data.List
import Debug.Trace
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
type FilterProcessorIO = SrcDict -> [String] -> [(Int,String)] -> IO String

filterAssemblyWithInterleavedSrcIO :: Bool -> String -> String -> IO String
filterAssemblyWithInterleavedSrcIO no_bits arch = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> IO String
        processLns
          | arch >= "sm_70" = processLns128B [] []
          | arch >= "sm_50" = processLnsMaxwell [] []
          | otherwise = return . unlines . map snd

        -- Volta and Turing
        processLns128B :: FilterProcessorIO
        processLns128B dict rlns [] = return $ unlines $ reverse rlns
        processLns128B dict rlns  lns@((_,ln0str):(_,ln1str):lns_sfx) =
          case parseRawInstWithBits (ln0str ++ ln1str) of
            Just ri -> processLns128B dict (fmtRi ri:rlns) lns_sfx
            Nothing -> tryProcessLineMapping dict rlns lns processLns128B
        processLns128B dict rlns [(_,lnstr)] = processLns128B dict (lnstr:rlns) []

        fmtRi :: RawInst -> String
        fmtRi ri
          | no_bits = fmtRiWithControlInfo ri
          | otherwise = fmtRiWithControlInfoBits ri

        tryProcessLineMapping :: SrcDict -> [String] -> [(Int,String)] -> FilterProcessorIO -> IO String
        tryProcessLineMapping dict0 rlns ((_,lnstr):lns) cont =
          case tryParseLineMapping lnstr of
            Nothing -> cont dict0 (lnstr:rlns) lns
            Just (file,lno) -> lookupMapping dict0
              where lookupMapping :: SrcDict -> IO String
                    lookupMapping dict =
                      case file `lookup` dict of
                        Nothing -> do
                          src_lns <- lines <$> readFile file
                          let arr = listArray (1,length src_lns) src_lns :: Array Int String
                          lookupMapping ((file,arr):dict)
                        Just arr -> cont dict (src_line:lnstr:rlns) lns
                          where src_line = "  //" ++ (arr!lno)

        -- Maxwell and Pascal
        processLnsMaxwell dict rlns [] = return $ unlines $ reverse rlns
        processLnsMaxwell dict rlns ((_,lnstr):lns)
          | "/* 0x" `isPrefixOf` s && mw64 /= Nothing =
            case mw64 of
              Just (w64,_) -> parseInstGroup rlns deps lns
                where deps = [
                           w64                .&. 0x000000000001FFFF
                        , (w64`shiftR`21)     .&. 0x000000000001FFFF
                        , (w64`shiftR`(2*21)) .&. 0x000000000001FFFF
                        ]
          where s = dropWhile isSpace lnstr
                mw64 = parseHexInComment s

                parseInstGroup rlns [] lns = processLnsMaxwell dict rlns lns -- done with group of 3
                parseInstGroup rlns _ [] = processLnsMaxwell dict rlns lns
                parseInstGroup rlns (dep:deps) ((lno,lnstr):lns) =
                    -- since the Maxwell and Volta control fields have the same encoding
                    -- we can cheat and just synthesize the "high 64b" of the Volta word
                    -- and using that in tryParseInstructionLines.
                    case parseRawInstWithBits (lnstr ++ fake_second_line) of
                      Nothing ->
                        -- e.g. parse failure just means the end of the function
                        -- NOTE: we might drop unused control codes here; that's okay
                        processLnsMaxwell dict rlns ((lno,lnstr):lns)
                      Just ri ->
                        parseInstGroup (fmtRi ri:rlns) deps lns

                  where fake_second_line = printf "  /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell dict rlns lns =
          tryProcessLineMapping dict rlns lns processLnsMaxwell



filterAssembly :: Bool -> String -> String -> String
filterAssembly no_bits arch = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> String
        processLns
          | arch >= "sm_70" = processLns128B
          | arch >= "sm_50" = processLnsMaxwell
          | otherwise = unlines . map snd

        fmtRi :: RawInst -> String
        fmtRi ri
          | no_bits = fmtRiWithControlInfo ri
          | otherwise = fmtRiWithControlInfoBits ri

        -- Volta and Turing
        processLns128B [] = ""
        processLns128B ((lno0,ln0str):(lno1,ln1str):lns) =
          case parseRawInstWithBits (ln0str ++ ln1str) of
            Just ri ->
              fmtRi ri ++ "\n" ++
              processLns lns
            Nothing ->
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
                    case parseRawInstWithBits (lnstr ++ fake_second_line) of
                      Nothing ->
                        -- e.g. parse failure just means the end of the function
                        -- NOTE: we might drop unused control codes here
                        processLnsMaxwell ((lno,lnstr):lns)
                      Just ri ->
                        fmtRi ri ++ "\n" ++
                        parseInstGroup deps lns
                  where fake_second_line = printf "  /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell ((_,lnstr):lns) =
          lnstr ++ "\n" ++
          processLnsMaxwell lns

