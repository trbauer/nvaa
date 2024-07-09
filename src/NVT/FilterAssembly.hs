module NVT.FilterAssembly where

import NVT.Fmt
-- import NVT.Demangle
import NVT.RawInst

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Array
import Data.Char
import Data.List
import Debug.Trace
import System.IO
import System.FilePath
import System.Directory
import Text.Printf
import qualified Data.Map.Strict as DM
import qualified Data.IntMap.Strict as DIM



-- "//## File "D:\\dev\\nvaa\\cuda-tests/cf.cu", line 62"
-- rejects inline
-- "//## File "D:\\dev\\nvaa\\cuda-tests/cf.cu", line 62 inlined at ..." << rejects
-- Typically we want the top-level only location only; that comes last without an "inlined at" part
tryParseLineMapping :: String -> Maybe (FilePath,Int)
tryParseLineMapping ln =
  case tryParseLineMappingWithSfx ln of
    Just (fp,lno,"") -> return (fp,lno)
    _ -> Nothing


-- Will allow for the above
tryParseLineMappingWithSfx :: String -> Maybe (FilePath,Int,String)
tryParseLineMappingWithSfx ln
  | lno_pfx `isPrefixOf` dropWhile isSpace ln =
    case reads sfx_file :: [(String,String)] of
      [(file,sfx)] ->
        case reads (dropWhile (not . isDigit) sfx) :: [(Int,String)] of
          [(lno,sfx)] -> Just (file,lno,sfx)
          _ -> Nothing
      _ -> Nothing
  | otherwise = Nothing
  where lno_pfx = "//## File "
        sfx_file = dropWhile (/='"') ln


lm_test = "//## File \"D:\\\\dev\\\\nvaa\\\\cuda-tests/cf.cu\", line 62"
inl_test = "        //## File \"C:\\include\\include\\__cuda/barrier.h\", line 204 inlined at \"E:\\dev\\mbarrier-example.cu\", line 149"
-- //## File "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 458 inlined at "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 471
-- //## File "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 471 inlined at "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 396
-- //## File "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 396 inlined at "E:\\dev\\nvaa\\experiments\\asynccopy/micro.cu", line 57
-- //## File "E:\\dev\\nvaa\\experiments\\asynccopy/micro.cu", line 57


-- maxwell =
--   "JUNK\n" ++
--   "                                                                                         /* 0x001c7c00e22007f6 */\n" ++
--   "        /*0008*/                   MOV R1, c[0x0][0x20] ;                                /* 0x4c98078000870001 */\n" ++
--   "        /*0010*/                   S2R R0, SR_CTAID.X ;                                  /* 0xf0c8000002570000 */\n" ++
--   "        /*0018*/                   S2R R2, SR_TID.X ;                                    /* 0xf0c8000002170002 */"
--   "JUNK\n" ++
type SrcDict = [(FilePath,Array Int String)]
type FilterProcessorIO = SrcDict -> [(Int,String)] -> IO ()
type Arch = String

data InlinedSource =
    InlinedSourceDROP
  | InlinedSourcePASSTHROUGH
  deriving (Show,Eq)

data FilterOpts =
  FilterOpts {
    foArch :: !String
  , foColor :: !Bool
  , foFmtOpts :: !FmtOpts
  , foInlinedSource :: !InlinedSource
  , foVerbosity :: !Int
  } deriving Show
fos_dft :: FilterOpts
fos_dft = FilterOpts "" False dft_fos InlinedSourceDROP 0

isCommentLine :: String -> Bool
isCommentLine = (=="//") . take 2 . dropWhile isSpace

isLabelLine :: String -> Bool
isLabelLine "" = False
isLabelLine (c:cs)
  | isLabelStart c =
    case span isLabelChar cs of
      (lbl,':':_) -> True
      _ -> False
isLabelLine _ = False

isLabelStart :: Char -> Bool
isLabelStart c = isAlpha c || c`elem`"._$"

isLabelChar :: Char -> Bool
isLabelChar c = isAlphaNum c || c`elem`"._$"

-- .byte 0x...""
decodeAsciiSection :: [(Int,String)] -> Either String (String,[(Int,String)])
decodeAsciiSection = loopLns ""
  where loopLns :: String -> [(Int,String)] -> Either String (String,[(Int,String)])
        loopLns rstr [] = return (reverse rstr,[])
    -- /*0040*/ .byte	0x6c, 0x69, 0x67, 0x6e, 0x20, 0x34, 0x20, 0x2e, 0x62, 0x38, 0x20, 0x5f, 0x5a, 0x5a, 0x4e, 0x34
        loopLns rstr ((lno,ln):lns)
          | null (dropWhile isSpace ln) = return (reverse rstr,((lno,ln):lns))
          | otherwise =
            case tks of
              ('/':'*':sfx):".byte":bs -> loopBs rstr bs
              ".byte":bs -> loopBs rstr bs
              _ -> Left $ show lno ++ ". expected line of bytes " ++ show tks
          where tks = words (map (\c -> if c`elem`"," then ' ' else c) ln)
                loopBs :: String -> [String] -> Either String (String,[(Int,String)])
                loopBs rstr [] = loopLns rstr lns
                loopBs rstr (tk:tks) =
                  case reads tk of
                    [(w,"")] | w <= 255 -> loopBs (chr w:rstr) tks
                    _ -> Left $ show lno ++ ". invalid .byte (" ++ tk ++ ")"


-- ptx ="\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL.version 8.3\NUL.target sm_90\NUL.address_size 64\NUL\NUL\NUL.global .align 4 .b8 _ZZN4cuda3std3__48__detail21__stronger_order_cudaEiiE7__xform[16] = {3, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 3};\NUL.global .align 1 .b8 _ZN56_INTERNAL_6d04214c_25_mbarrier_embarrassment_cu_954f22e24cuda3std3__48in_placeE[1];\NUL.global .align 1 .b8 _ZN56_INTERNAL_6d04214c_25_mbarrier_embarrassment_cu_954f22e24cuda3std6ranges3__45__cpo4swapE[1];\NUL\NUL.visible .entry _Z18mbarrier_test_waitPi(\NUL.param .u64 _Z18mbarrier_test_waitPi_param_0\NUL)\NUL{\NUL.reg .b32 \t%r<3>;\NUL.reg .b64 \t%rd<5>;\NUL\NUL\NUL\NULld.param.u64 \t%rd1, [_Z18mbarrier_test_waitPi_param_0];\NUL\NULcvta.to.global.u64 \t%rd2, %rd1;\NUL\NUL\NUL{\NUL\NUL\NUL.reg .b64 r1;\NUL.reg .pred complete;\NUL.shared .b64 shMem;\NUL\NULmbarrier.init.shared.b64 [shMem], 32;  // N threads participating in the mbarrier.\NUL\NULmbarrier.arrive.shared.b64  r1, [shMem]; // N threads executing mbarrier.arrive\NUL\NUL\NUL\NULwaitLoop:\NULmbarrier.test_wait.shared.b64    complete, [shMem], r1;\NUL@!complete nanosleep.u32 20;\NUL@!complete bra waitLoop;\NUL}\NUL\NUL\NUL\NULmov.u32 \t%r1, %tid.x;\NULmul.wide.u32 \t%rd3, %r1, 4;\NULadd.s64 \t%rd4, %rd2, %rd3;\NUL\NUL\NULred.global.add.u32 \t[%rd4], 1;\NUL\NULret;\NUL\NUL}\NUL\NUL.visible .entry _Z17mbarrier_try_waitPi(\NUL.param .u64 _Z17mbarrier_try_waitPi_param_0\NUL)\NUL{\NUL.reg .b32 \t%r<3>;\NUL.reg .b64 \t%rd<5>;\NUL\NUL\NUL\NULld.param.u64 \t%rd1, [_Z17mbarrier_try_waitPi_param_0];\NUL\NULcvta.to.global.u64 \t%rd2, %rd1;\NUL\NUL\NUL{\NUL\NUL\NUL.reg .b64 r1;\NUL.reg .pred complete;\NUL.shared .b64 shMem;\NUL\NULmbarrier.init.shared.b64 [shMem], 32;  // N threads participating in the mbarrier.\NUL\NULmbarrier.arrive.shared.b64  r1, [shMem]; // N threads executing mbarrier.arrive\NUL\NUL\NUL\NULwaitLoop:\NULmbarrier.try_wait.shared.b64    complete, [shMem], r1;\NUL@!complete bra waitLoop;\NUL}\NUL\NUL\NUL\NULmov.u32 \t%r1, %tid.x;\NULmul.wide.u32 \t%rd3, %r1, 4;\NULadd.s64 \t%rd4, %rd2, %rd3;\NUL\NUL\NULred.global.add.u32 \t[%rd4], 1;\NUL\NULret;\NUL\NUL}\NUL\NUL\NUL.section\t.debug_str\NUL{\NUL$L__info_string0:\NUL\NUL\NUL\NUL}\NUL"

decodeNvDebugPtxSection :: SrcDict -> [(Int,String)] -> Either String (SrcDict,[(Int,String)])
decodeNvDebugPtxSection dict lns =
    case decodeAsciiSection lns of
      Left err -> Left err
      Right (str,lns_sfx) ->
          return ((".nv_debug_ptx_txt",array):dict,lns_sfx)
        where debugSectNulToLine :: String -> [String]
              debugSectNulToLine = lines . map (\c -> if c == '\NUL' then '\n' else c)
              dbg_lns = debugSectNulToLine str
              array = listArray (1,length dbg_lns) dbg_lns

-- .section ID,STRLIT,@ID
{-
tryParseSectionHeader :: [(Int,String)] -> Maybe (String,String,String)
tryParseSectionHeader [] = Nothing
tryParseSectionHeader ((lno,ln):lns) =
    case splitAt (length ".section") (dropWhile isSpace ln) of
      (".section",sfx) -> --ID,STRLIT,@ID
        case span (/=',') (dropWhile isSpace sfx) of
          (id,',':sfx) ->
            case reads (dropWhile isSpace sfx) of
              [(slit,sfx)] ->
                case dropWhile isSpace sfx of
                  ',':sfx -> return (id,slit,dropWhile isSpace sfx)
                  _ -> Nothing
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
-}

matchesSectionStart :: String -> String -> [(Int,String)] -> Bool
matchesSectionStart sect_name sect_lbl ((_,ln0str):(_,ln1str):lns_sfx) =
  case (words ln0str,words ln1str) of
    (".section":sfx:_,[lbl]) -> sect_name`isPrefixOf`sfx && lbl == sect_lbl
    _ -> False
matchesSectionStart _ _ _ = False

filterAssemblyWithInterleavedSrcIO :: FilterOpts -> Handle -> String -> IO ()
filterAssemblyWithInterleavedSrcIO fos h_out = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> IO ()
        processLns
          | foArch fos >= "sm_70" = processLns128B    empty_dict
          | foArch fos >= "sm_50" = processLnsMaxwell empty_dict
          | otherwise = hPutStr h_out . unlines . map snd

        empty_dict :: SrcDict
        empty_dict = []

        emitLn :: String -> IO ()
        emitLn = hPutStrLn h_out
        emit :: String -> IO ()
        emit = hPutStr h_out

        -- Volta, Turing, and Ampere
        processLns128B :: FilterProcessorIO
        processLns128B _ [] = return ()
        processLns128B dict lns
          | matchesSectionStart ".nv_debug_ptx_txt" ".nv_debug_ptx_txt:" lns =
              case decodeNvDebugPtxSection dict (drop 2 lns) of
                Left err -> do
                  putStrLn $ "filterAssemblyWithInterleavedSrcIO: ERROR " ++ err
                  tryProcessLineMapping dict lns processLns128B
                Right (dict,lns_sfx) -> do
                  when (foVerbosity fos >= 2) $ do
                    emitStyle fos h_out fmt_sty_comm  " /* filterAssemblyWithInterleavedSrcIO: successfully parsed .nv_debug_ptx_txt */\n"
                  -- render the .nv_debug_ptx_txt as raw bytes anyway
                  -- (we could use lns_sfx to skip it)
                  tryProcessLineMapping dict lns processLns128B
          | matchesSectionStart ".debug_str" ".debug_str:" lns =
            case decodeAsciiSection (drop 2 lns) of
              Left err -> do
                putStrLn $ "filterAssemblyWithInterleavedSrcIO: ERROR " ++ err
                tryProcessLineMapping dict lns processLns128B
              Right (str,lns_sfx) -> do
                emitStyle fos h_out fmt_sty_comm $  "/* " ++ show str ++ " */"
                emitLn ""
                tryProcessLineMapping dict lns processLns128B

        processLns128B dict lns@((_,ln0str):(_,ln1str):lns_sfx) =
          case parseSampleInst (ln0str ++ ln1str) of
            Right si -> do
              emitSpans fos h_out (fmtSi fos si) >> emitLn ""
              processLns128B dict lns_sfx
            Left _ -> tryProcessLineMapping dict lns processLns128B
        processLns128B dict (ln@(_,lnstr):lns) = do
          emitGenericLine lnstr
          tryProcessLineMapping dict lns processLns128B

-- handle comments
--        processBinarySection :: SrcDict -> [(Int,String)] -> FilterProcessorIO -> IO ()
--        processBinarySection =
-- debug_str:
--        /*0000*/  .byte 0x5f, 0x5a, 0x4e, 0x35, 0x36, 0x5f, 0x49, 0x4e, 0x54, 0x45, 0x52, 0x4e, 0x41, 0x4c, 0x5f, 0x36
--        /*004c*/ 	.dword	_Z18mbarrier_test_waitPi

        tryProcessLineMapping :: SrcDict -> [(Int,String)] -> FilterProcessorIO -> IO ()
        tryProcessLineMapping dict0 [] cont = return ()
        tryProcessLineMapping dict0 ((_,lnstr):lns) cont
          | is_global_directive = do
              hPutStr h_out lnstr
              emitStyle fos h_out fmt_sty_comm (" // " ++ global_demangled)
              hPutStrLn h_out ""
              cont dict0 lns
          | isLabelLine lnstr && foColor fos = do
              emitStyle fos h_out fmt_sty_lit lnstr >> emitLn "" >> cont dict0 lns
          | otherwise =
            case tryParseLineMappingWithSfx lnstr of
              Nothing
                | isCommentLine lnstr && foColor fos ->
                    emitStyle fos h_out fmt_sty_comm lnstr >> emitLn "" >> cont dict0 lns
                | otherwise -> emitGenericLine lnstr >> cont dict0 lns
              -- //## File "E:\\dev\\nvaa\\experiments\\asynccopy/micro.cu", line 57
              Just (file,lno,"") -> lookupMapping dict0
                where lookupMapping :: SrcDict -> IO ()
                      lookupMapping dict = do
                        case file `lookup` dict of
                          Nothing -> do
                            z <- doesFileExist file
                            if not z then do
                                putStrLn $ file ++ ": FILE NOT FOUND (line mappings unavailable)"
                                let arr = listArray (0,0) [] :: Array Int String
                                lookupMapping ((file,arr):dict)
                              else do
                                src_lns <- lines <$> readFile file
                                let arr = listArray (1,length src_lns) src_lns :: Array Int String
                                lookupMapping ((file,arr):dict)
                          Just arr -> do
                            let src_loc = takeFileName file ++ ":" ++ show lno
                            let src_line = "  // " ++ padR 32 (src_loc ++ ": ") ++ ln_str
                                  where ln_str
                                          | lno <= snd (bounds arr) = arr ! lno
                                          | otherwise = ": ???"
                            emitStyle fos h_out fmt_sty_comm src_line
                            emitLn ""
                            -- emitLn $ lnstr ++ "\n" ++ src_line
                            cont dict lns
              -- inlined at line source line
              -- -- //## File "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 458 inlined at "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v12.0\\bin/../include\\cuda/pipeline", line 471
              Just (_,_,sfx) | "inlined at"`isPrefixOf`dropWhile isSpace sfx -> do
                when (foInlinedSource fos == InlinedSourcePASSTHROUGH) $ do
                  emitStyle fos h_out fmt_sty_comm lnstr
                  emitLn ""
                cont dict0 lns
              Just x@(_,_,_) -> do
                -- print x
                emitStyle fos h_out fmt_sty_err ("MALFORMED LINE: " ++ show lnstr ++ "\n") -- ERROR unexpected
                cont dict0 lns
          where {-
                global_demangled =
                  -- .global _Z5add64PyPKyy
                  case  words lnstr of
                    [".global", func] ->
                      case demangle func of
                        Left e_msg -> "(demangle error: " ++ e_msg ++ ")"
                        Right func_dem
                          | func_dem /= func -> func_dem
                          | otherwise -> "" -- e.g. extern C
                    _ -> ""
                is_global_directive = not (null global_demangled)
                -}
                -- see: libcufilt: how to bind to this in Haskell? (safe foreign call to a DLL)
                -- https://docs.nvidia.com/cuda/cuda-binary-utilities/index.html#library-availability
                is_global_directive = False
                global_demangled = ""


        -- emit something else
        --   * bytes in a raw section
        --   * section header
        emitGenericLine :: String -> IO ()
        emitGenericLine ln = do
            sfx <- emitTokens ln
            emitLn sfx -- emit the leftovers
          where emitSpaces sfx =
                  case span isSpace sfx of
                    (spcs,sfx) -> emit spcs >> return sfx
                emitTokens :: String -> IO String
                emitTokens sfx = do
                  sfx <- emitSpaces sfx
                  case sfx of
                    '/':'*':sfx -> emitComm ("*/") sfx >>= emitTokens
                    _ ->
                      case find (`isPrefixOf`sfx) keywords of
                        Just kw -> emitStyle fos h_out FmtCL kw >> return (drop (length kw) sfx)
                        Nothing ->
                          case find (`isPrefixOf`sfx) data_types of
                            Just dt -> emitStyle fos h_out FmtYL dt >> return (drop (length dt) sfx)
                            _ -> return sfx

                keywords = [
                      ".align"
                    , ".elftype"
                    , ".global"
                    , ".headerflags"
                    , ".other"
                    , ".sectionentsize"
                    , ".sectionflags"
                    , ".sectioninfo"
                    , ".section"
                    , ".size"
                    , ".text"
                    , ".type"
                    , ".zero"
                    ]
                data_types = [".byte",".short",".word",".dword"]

                emitComm rstr [] =
                  emitStyle fos h_out fmt_sty_err (reverse rstr) >> return ""
                emitComm rstr ('*':'/':sfx) = do
                  emitStyle fos h_out fmt_sty_comm (reverse rstr ++ "*/")
                  return sfx
                emitComm rstr (c:cs) = emitComm (c:rstr) cs

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

emitStyle :: FilterOpts -> Handle -> FmtStyle -> String -> IO ()
emitStyle fos h sty str
  | foColor fos = fsEmitStyle h sty str
  | otherwise = hPutStr h str

emitSpans :: FilterOpts -> Handle -> [FmtSpan] -> IO ()
emitSpans fos h_out
  | foColor fos = fsEmit h_out
  | otherwise = hPutStr h_out . fssToString

fmtSi :: FilterOpts -> SampleInst -> [FmtSpan]
fmtSi fos = fmtSampleInstToFmtSpans (foFmtOpts fos)

filterAssembly :: FilterOpts -> String -> String
filterAssembly fos = processLns . zip [1..] . lines
  where processLns :: [(Int,String)] -> String
        processLns
          | foArch fos >= "sm_70" = processLns128B
          | foArch fos >= "sm_50" = processLnsMaxwell
          | otherwise = unlines . map snd

        -- Volta and Turing
        processLns128B [] = ""
        processLns128B ((lno0,ln0str):(lno1,ln1str):lns) =
          case parseSampleInst (ln0str ++ ln1str) of
            Right si ->
              fssToString (fmtSi fos si) ++ "\n" ++
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
                        fssToString (fmtSi fos si) ++ "\n" ++
                        parseInstGroup deps lns
                  where fake_second_line = printf "  /* 0x%016X */" shifted_word
                        -- in Volta it is stored at [125:105]
                        shifted_word = dep `shiftL` (64 - 23)
        processLnsMaxwell ((_,lnstr):lns) =
          lnstr ++ "\n" ++
          processLnsMaxwell lns


--------------------------------------------------------------------------------
-- assembly collating
type SrcLoc = (FilePath,Int)
type SassListingMap = ListingMap SassListingLine
type SassListingLine = (Int,SassListingLineContents) -- (lno_in_asm,line|inst)
type PtxInstMap = ListingMap PtxInstLine
type PtxInstLine = (Int,String) -- (lno_in_asm, inst_text)

-- TODO: AsmLine a = AsmLine {alAsmLine :: !Int, alContent :: !a}

data SassListingLineContents =
    SassListingLineContentsLabel !Int !String -- PC and label
  | SassListingLineContentsInst !SampleInst -- PC is in the inst

-- map source locations to all listing lines/instructions that map there
type ListingMap a = DM.Map SrcLoc [a]

-- line and string pair for PTX instruction
no_sloc :: SrcLoc
no_sloc = ("",0)

-- maps locations to listing line numbers
collateSassAssembly :: String -> SassListingMap
collateSassAssembly = loop DM.empty no_sloc 0 . zip [1..] . lines
  where loop :: SassListingMap -> SrcLoc -> Int -> [(Int,String)] -> SassListingMap
        loop fm _ _ [] = fm
        loop fm sl lpc ((lno0,ln0):lns)
          | isLabelLine ln0 =  -- labels don't break location continuity
            loop (fmInsert sl (lno0,SassListingLineContentsLabel lpc ln0) fm) sl lpc lns
          | otherwise =
            case lns of
              [] -> handleSrcMappingComment ln0
              (_,ln1):lns1 ->
                case parseSampleInst (ln0 ++ ln1) of
                  Right si -> loop fm1 sl (riOffset (siRawInst si) + inst_len) lns1
                    where fm1 = fmInsert sl (lno0,SassListingLineContentsInst si) fm
                          inst_len = 0x10 -- assume 16B instructions
                  Left _ -> handleSrcMappingComment ln0
          where handleSrcMappingComment ln =
                  case tryParseLineMapping ln of
                    Nothing -> loop fm no_sloc lpc lns -- reset loc on inst parse failure
                    Just sl -> loop fm sl      lpc lns

fmInsert :: SrcLoc -> SassListingLine -> SassListingMap -> SassListingMap
fmInsert sl ll = DM.insertWith (++) sl [ll]

--
-- data Collated a =
--   Collated {
--     cListingMap :: ListingMap a
--   , cLinesToInsts :: DIM.IntMap a
--   }
--

emitCollatedListing :: FilterOpts -> Handle -> String -> String -> IO ()
emitCollatedListing fos h_out ptx sass = do
  let sass_lms0 = collateSassAssembly sass
      ptx_lms0 = collatePtxAssembly ptx
  ---------------------------
  -- canonicalize all the paths so that they match the source files we walk
  -- (e.g. PTX or SASS might normalize the path or produce an absolute path, etc...)
  -- this requires remapping all of the
      filesFromMap = map (fst . fst) . DM.toList

  -- all source files mentioned in all listings
      all_src_fs0 = sort . nub $ filesFromMap sass_lms0 ++ filesFromMap ptx_lms0

  -- canonically and associate (old-name,canon-name)
  src_f_canons <-
    forM (filter (not . null) all_src_fs0) $ \src_f -> do
      src_f_canon <- canonicalizePath src_f
      return (src_f,src_f_canon)

  -- remap the listings so all file names are correct
  -- this assumes each listing only uses one mapping for each file
  let remapEntry :: SrcLoc -> SrcLoc
      remapEntry e@(src_f,lno) =
        case src_f `lookup` src_f_canons of
          Nothing -> e
          Just src_f_canon -> (src_f_canon,lno)
  let remapFileNamesToCanonical = DM.mapKeysWith (++) remapEntry
  let all_src_fs = nub (map snd src_f_canons)
      ptx_lms = remapFileNamesToCanonical ptx_lms0
      sass_lms = remapFileNamesToCanonical sass_lms0
  emitCollatedListingWith fos h_out all_src_fs ptx_lms sass_lms


emitCollatedListingWith ::
  FilterOpts -> Handle ->
  [FilePath] -> PtxInstMap -> SassListingMap -> IO ()
emitCollatedListingWith fos h_out all_src_fs ptx_lms sass_lms = do
    mapM_ emitSrcFile all_src_fs
  where emitFileHeader :: FilePath -> IO ()
        emitFileHeader src_f = do
          emitStyle fos h_out FmtLW  $
            "// " ++ replicate 72 '=' ++ "\n" ++
            "// " ++ src_f ++ "\n"

        emitPtxInstLineLn :: Int -> String -> Bool -> IO ()
        emitPtxInstLineLn llno ptx_ins ooo =
            emitSpans fos h_out (lno_span:spans ptx_ins) >> hPutChar h_out '\n'
          where lno_span = text $ ooo_str ++ printf "%4d" llno ++ "> "
                  where ooo_str = if ooo then "!" else " "

                spans :: String -> [FmtSpan]
                spans s
                  -- filter out non instruction lines
                  | not (isPtxInstLine s) = [text s]
                  | otherwise = parseBegin s
                  where parseBegin "" = []
                        parseBegin sfx@(c:cs)
                          | isSpace c =
                            case span isSpace sfx of
                              (pfx,sfx) -> text pfx : parsePredOp sfx
                          | otherwise = parsePredOp sfx

                        parsePredOp ('@':cs) =
                          case span (not . isSpace) cs of
                            (pfx,sfx) -> text pfx : parseBegin sfx
                        parsePredOp str = -- looking at the op
                          case span (\c -> isAlphaNum c || c == '.') str of
                            (op,sfx) -> [opText op, text sfx]

                text = FmtSpan FmtMD
                opText = FmtSpan FmtML
        --
        -- set of mapped ptx listing lines
        toListingMap :: DM.Map SrcLoc [(Int,a)] -> DIM.IntMap ()
        toListingMap = DIM.fromList . concatMap (map ((\x -> (x,())) . fst) . snd) . DM.toList

        all_ptx_listing_lines :: DIM.IntMap ()
        all_ptx_listing_lines = toListingMap ptx_lms
        all_sass_listing_lines :: DIM.IntMap ()
        all_sass_listing_lines = toListingMap sass_lms

        -- true if there's another instruction between these two listing lines
        areListingLinesDiscontinuous :: DIM.IntMap () -> Int -> Int -> Bool
        areListingLinesDiscontinuous lls ll_stt ll_end =
            ll_stt >= 1 &&
              any (`DIM.member`lls) [ll_stt + 1 .. ll_end - 1]
        --
        -- all_sass_listing_lines :: DIM.IntMap [SampleInst]
        -- all_sass_listing_lines = DIM.fromList (concatMap snd (DM.toList sass_lms))
        -- hasSassListingLine :: Int -> Bool
        -- hasSassListingLine = (`DIM.member`all_sass_listing_lines)

        emitSassInstLn :: Int -> SassListingLineContents -> Bool -> IO ()
        emitSassInstLn llno sllc ooo =
            emitSpans fos h_out $ ooo_span : lloc_span ++ body ++ fs_none "\n"
          where ooo_span = if ooo then FmtSpan FmtRD "!" else FmtSpan FmtNONE " "
                lloc_span
                  -- emit listing line number (no PCs)
                  | off < 0 = fs_none (printf "[line %4d]" llno)
                  -- use the PC
                  | otherwise = fs_none (printf "[%05X] " off)
                  where off =
                          case sllc of
                            SassListingLineContentsInst si -> riOffset (siRawInst si)
                            SassListingLineContentsLabel pc _ -> pc
                body :: [FmtSpan]
                body =
                  case sllc of
                    SassListingLineContentsLabel _ lbl -> [fs_lit lbl]
                    SassListingLineContentsInst si -> fmtSi fos si

        emitSrcLn :: String -> IO ()
        emitSrcLn s = emitStyle fos h_out fmt_sty_comm s >> hPutStrLn h_out ""

        -- true if there's an instruction between these two listing lines
        -- areSassDiscontinuous :: Int -> Int -> Bool
        -- areSassDiscontinuous ll_stt ll_end =
        --    ll_stt >= 1 && any hasSassListingLine [ll_stt + 1 .. ll_end - 1]

        -- iterates source file line by line keeping track of the
        -- last listing line we've emitted to avoid out of order (!) warnings
        -- e.g.
        -- //  w = x*y + z
        -- > IMAD ...
        -- //  w *= 2
        -- > IADD ... << consider in order with IMAD even though new src line so
        --     long as there is no instruction between the IMAD and IADD
        --
        -- also we are keep track of listing location for PTX too
        emitSrcFile :: FilePath -> IO ()
        emitSrcFile src_f = do
          emitFileHeader src_f
          --
          src_lns <- zip [1..] . lines <$> readFile src_f
          length src_lns `seq` return ()
          emitSrcFileLines src_f (-1,-1) src_lns
          --
          -- don't care about PTX orphans
          let emit_ptx_orphans = False
          --
          -- emit PTX orphans
          when emit_ptx_orphans $
            case no_sloc `DM.lookup` ptx_lms of
              Nothing -> return ()
              Just oph_ptx_llns -> do
                emitFileHeader "(PTX orphans)"
                forM_ (sortOn fst oph_ptx_llns) $ \(ptx_llno,si) ->
                  emitPtxInstLineLn ptx_llno si False
          -- emit SASS orphans
          case no_sloc `DM.lookup` sass_lms of
            Nothing -> return ()
            Just oph_llns -> do
              emitFileHeader "(orphans)"
              forM_ (sortOn fst oph_llns) $ \(llno,sllc) -> emitSassInstLn 0 sllc False

        emitSrcFileLines :: FilePath -> (Int,Int) -> [(Int,String)] -> IO ()
        emitSrcFileLines _ _ [] = return ()
        emitSrcFileLines src_f (pv_ptx_llno,pv_sass_llno)  ((src_lno,src_ln):src_lns) = do
          emitSrcLn $ printf "%3d>" src_lno ++ src_ln
          let src_loc = (src_f,src_lno)
              -- emitLingMappingsForLanguage ::
              --   ListingMap a -> Int ->
              --   (Int -> Int -> Bool) -> (Int -> a -> Bool -> IO ()) -> IO Int
              emitLingMappingsForLanguage lm pv_llno are_discontinuous emit_inst =
                case src_loc`DM.lookup`lm of
                  --
                  -- no mappings to this source line
                  Nothing -> return pv_llno
                  --
                  -- 'lls' is all the ptx/sass listing lines that map to this source line
                  Just lls -> emitListingLines pv_llno (sortOn fst lls)
                    where emitListingLines pv_llno [] = return pv_llno
                          emitListingLines pv_llno ((llno,si):llns) = do
                            let discnt = are_discontinuous pv_llno llno
                            emit_inst llno si discnt
                            emitListingLines llno llns
          nxt_ptx_llno <-
            emitLingMappingsForLanguage
              ptx_lms pv_ptx_llno
              (areListingLinesDiscontinuous all_ptx_listing_lines)
              emitPtxInstLineLn
          nxt_sass_llno <-
            emitLingMappingsForLanguage
              sass_lms pv_sass_llno
              (areListingLinesDiscontinuous all_sass_listing_lines)
              emitSassInstLn
          emitSrcFileLines src_f (nxt_ptx_llno,nxt_sass_llno) src_lns -- next src line


--------------------------------------------------------------------------------
-- PTX collating

-- just internal to collatePtxAssembly
-- we post process it to a PtxInstMap
type PtxMapInternal = DM.Map (Int,Int) [PtxInstLine]

no_ptx_loc :: (Int,Int)
no_ptx_loc = (-1,-1)

collatePtxAssembly :: String -> PtxInstMap
collatePtxAssembly = loop DM.empty [] no_ptx_loc . zip [1..] . lines
  where loop :: PtxMapInternal -> [(Int,FilePath)] -> (Int,Int) -> [(Int,String)] -> PtxInstMap
        loop pmi fs curr_ptx_loc [] = foldl' acc DM.empty (DM.toList pmi)
          where acc :: PtxInstMap -> ((Int,Int),[PtxInstLine]) -> PtxInstMap
                acc pm ((f_ix,lno),ptx_lns)
                  | no_ptx_loc == (f_ix,lno) =
                     DM.insertWith (++) no_sloc ptx_lns pm
                  | otherwise =
                  case f_ix `lookup` fs of
                    Nothing -> trace "collatePtxAssembly: cannot find .function mapping for .loc"
                      pm
                    Just fp -> DM.insertWith (++) (fp,lno) ptx_lns pm
          -- ... to do convert PtxMapInternal to PtxInstLineMap
        loop pmi fs curr_ptx_loc ((lno,ln):lns) =
            case ws of
              (".loc":f_ix:f_lno:_) ->
                case (readMaybe f_ix,readMaybe f_lno) of
                  (Just f_ix,Just f_lno) -> loop pmi fs (f_ix,f_lno) lns -- parsed .loc
                  _ -> badLine "malformed .loc"
              (".file":f_ix:f_name:_) ->
                case readMaybe f_ix of
                  Just f_ix ->
                    case decodeFileName f_name of
                      Just f_name_dec -> loop pmi ((f_ix,f_name_dec):fs) no_ptx_loc lns -- parsed .file
                      Nothing -> badLine ".file string literal"
                  _ -> badLine ".file index"
              [] -> ignoreLine
              _ -> handleNonEmptyLine
          where ws = words (map (\c -> if c == ',' then ' ' else c) ln)
                ws_0 = ws !! 0

                handleNonEmptyLine
                  -- begin/end of function brace
                  | "}"`isPrefixOf`ln = invalidateLocAndIgnoreLine
                  | "{"`isPrefixOf`ln = invalidateLocAndIgnoreLine
                  | ws_0`elem`[".entry",".func"] = invalidateLocAndIgnoreLine
                  --
                  -- these don't invalidate the last .loc, but we drop them
                  | "//" `isPrefixOf` dropWhile isSpace ln = ignoreLine
                  | isLabelLine ln = ignoreLine -- FOO:
                  | ws_0 `elem` ignore_no_invalidate_set  = ignoreLine

                  -- an instruction or block or whatnot (we want them all):
                  | any (`isPrefixOf`ws_0) ["{","}"] = addLine ln -- scope (e.g. call scope or inline asm)
                  | ws_0 `elem` [".reg",".shared"] = addLine ln
                  --
                    -- call is a little harder because it spans multiple lines;
                    -- gobble up lines until we hit the ending right paren
                  | isPtxCallInstFromTokens ws =
                    case span (not . (";"`isInfixOf`) . dropWhile isSpace . snd) lns of
                      (_,[]) -> badLine "malformed call"
                      (call_lns,(_,end_ln):lns) ->
                          addLineThen (trimLines (ln:sfx_lns)) lns
                        where sfx_lns = map snd call_lns ++ [end_ln]
                              trimLines = unwords . words  . intercalate " "
                  --
                  -- some other PTX instruction
                  | isPtxInstLineFromTokens ws = addLine ln
                  --
                  | otherwise = badLine "unmatched line"
                  where ignore_no_invalidate_set =
                          [
                              ")" -- end of a function declaration (be conservative and retain loc)
                            , ".address_size"
                            , ".const" --
                            , ".local"
                            , ".param" -- can be part of //callseq
                            , ".target"
                            , ".version"
                            , ".visible"
                          ]

                -- call spans multiple lines for some odd reason
{-
	// Callseq Start 3
	{
	.reg .b32 temp_param_reg;
	// <end>}
	.param .b64 param0;
	st.param.b64	[param0+0], %rd5;
	.param .b32 retval0;
	call.uni (retval0),
	cudaFree,
	(
	param0
	);
  // <<<< there could be more here (...){, protoype};
  //                                    ^^^^^^^^^^^^^
	ld.param.b32	%r6, [retval0+0];

	//{
	}// Callseq End 3
-}
                badLine why = trace ("PTX:" ++ show lno ++ ": " ++ why ++ ": " ++ ln) $
                   invalidateLocAndIgnoreLine
                invalidateLocAndIgnoreLine = loop pmi fs no_ptx_loc lns

                -- doesn't invalidate current location
                ignoreLine = loop pmi fs curr_ptx_loc lns

                -- add just this current line
                addLine ln = addLineThen ln lns

                -- custom version where we are collapsing lines
                addLineThen :: String -> [(Int,String)] -> PtxInstMap
                addLineThen ln lns = loop pmi1 fs curr_ptx_loc lns
                  where pmi1 = DM.insertWith (++) curr_ptx_loc [(lno,ln)] pmi

-- e.g. "E:\\dev\\nvaa\\micros/addrspaces.cu"
-- problem is that it's partially escaped
-- we are parsing ...dev\\\\nvaa...
-- so the result is still dev\\nvaa and that fails to open the file
decodeFileName :: String -> Maybe FilePath
decodeFileName = (fixSlashes <$>) . readMaybe
  where fixSlashes "" = ""
        fixSlashes ('\\':'\\':cs) = '\\':fixSlashes cs
        fixSlashes (c:cs) = c:fixSlashes cs

{-
data PtxInst =
  PtxInst {
    piLine :: Int
  , piOp
  }

tryParsePtxLine :: String -> Maybe [String]
tryParsePtxLine
-}

isPtxInstLine :: String -> Bool
isPtxInstLine = isPtxInstLineFromTokens . words
isPtxInstLineFromTokens :: [String] -> Bool
isPtxInstLineFromTokens = isPtxInstLineFromTokensWith isPtxOpName
isPtxCallInstFromTokens :: [String] -> Bool
isPtxCallInstFromTokens =
  isPtxInstLineFromTokensWith (\op -> op == "call" || "call."`isPrefixOf`op)
isPtxInstLineFromTokensWith :: (String -> Bool) -> [String] -> Bool
isPtxInstLineFromTokensWith op_pred ws =
    case ws of
      [] -> False
      (('@':_):ws_1:_) -> op_pred ws_1 -- @%p2 bra 	BB0_8;
      (ws_0:_) -> op_pred ws_0 -- e.g. ld.param.u64

-- e.g. shl.b32, cvt.u32.u64, ret;
-- nullary ops like 'ret' may be suffixed with ;
isPtxOpName :: String -> Bool
isPtxOpName "" = False
isPtxOpName str0
  | null str = False -- just ;
  | otherwise =
    isAlpha (head str) &&
      all (\c -> isAlphaNum c || c == '.') str
  where str = if last str0 == ';' then init str0 else str0

-- "\"E:\\\\dev\\\\nvaa\\\\micros/addrspaces.cu\""
readMaybe :: Read a => String -> Maybe a
readMaybe inp =
  case reads inp of
    [(a,"")] -> Just a
    _ -> Nothing