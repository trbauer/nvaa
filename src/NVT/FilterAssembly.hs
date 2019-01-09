module NVT.FilterAssembly where

import NVT.RawInst

import Control.Applicative
import Data.Bits
import Data.Char
import Data.List
import Text.Printf


-- maxwell =
--   "JUNK\n" ++
--   "                                                                                         /* 0x001c7c00e22007f6 */\n" ++
--   "        /*0008*/                   MOV R1, c[0x0][0x20] ;                                /* 0x4c98078000870001 */\n" ++
--   "        /*0010*/                   S2R R0, SR_CTAID.X ;                                  /* 0xf0c8000002570000 */\n" ++
--   "        /*0018*/                   S2R R2, SR_TID.X ;                                    /* 0xf0c8000002170002 */"
--   "JUNK\n" ++


filterAssembly :: String -> String -> String
filterAssembly arch = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> String
        processLns
          | arch >= "sm_70" = processLns128B
          | arch >= "sm_50" = processLnsMaxwell
          | otherwise = unlines . map snd

        -- Volta and Turing
        processLns128B [] = ""
        processLns128B ((lno0,ln0str):(lno1,ln1str):lns) =
          case tryParseInstructionLines ln0str ln1str of
            Nothing ->
              ln0str ++ "\n" ++
              processLns ((lno1,ln1str):lns)
            Just ri ->
              fmtRiWithControlInfo ri ++
              processLns lns
        processLns128B ((_,lnstr):lns) = lnstr ++ processLns lns

        -- Maxwell and Pascal
        processLnsMaxwell [] = ""
        processLnsMaxwell ((_,lnstr):lns)
          | "/* 0x" `isPrefixOf` s && mw64 /= Nothing =
            case mw64 of
              Just w64 -> parseInstGroup deps lns
                where deps = [
                           w64                .&. 0x000000000001FFFF
                        , (w64`shiftR`21)     .&. 0x000000000001FFFF
                        , (w64`shiftR`(2*21)) .&. 0x000000000001FFFF
                        ]
          where s = dropWhile isSpace lnstr
                mw64 = parseSlashStarHexWord s

                parseInstGroup [] lns = processLnsMaxwell lns -- done with group of 3
                parseInstGroup _ [] = ""
                parseInstGroup (dep:deps) ((lno,lnstr):lns) =
                    -- since the Maxwell and Volta control fields have the same encoding
                    -- we can cheat and just synthesize the "high 64b" of the Volta word
                    -- and using that in tryParseInstructionLines.
                    case tryParseInstructionLines lnstr fake_second_line of
                      Nothing ->
                        -- e.g. parse failure just means the end of the function
                        -- NOTE: we might drop unused control codes here
                        processLnsMaxwell ((lno,lnstr):lns)
                      Just ri ->
                        fmtRiWithControlInfo ri ++
                        parseInstGroup deps lns
                  where fake_second_line = printf "           /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell ((_,lnstr):lns) =
          lnstr ++ "\n" ++
          processLnsMaxwell lns

