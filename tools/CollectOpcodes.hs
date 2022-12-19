module Main where

import NVT.Driver
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst
import NVT.Bits

import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.FilePath
import System.Directory
import System.Process
import Text.Printf
-- import qualified Data.Map.Strict as DM
import qualified Data.ByteString as S

sm_ver :: String
sm_ver = "sm_90"
-- sm_ver = "sm_86"
-- sm_ver = "sm_80"
-- sm_ver = "sm_75"

main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> collectOps
    _
      | any (`elem`as) ["-h","--help"] ->
        putStrLn "usage: collect_opcodes.exe"
      | otherwise -> die "unexpected option"


collectOps :: IO ()
collectOps = body
  where body = do
          -- for all .sass files
          --    for all lines in each file
          --       tryParseFilteredRawInst line
          --       split base opcode off
          --       appendFile ("examples/" ++ base_op ++ "/") $
          createDirectoryIfMissing True output_root
          --
--          samples_sass_files <-  filterSassOnly <$> getSubPaths "examples/sm_75/samples"
--          mapM_ processFile (samples_sass_files)
          --
          -- library_dirs <- getSubPaths "examples/sm_75/libs" >>= filterM doesDirectoryExist
          -- libraries_sass_files <- filterSassOnly . concat <$> mapM getSubPaths library_dirs
          -- mapM_ processFile library_dirs
          let processDir :: FilePath -> IO ()
              processDir dir = do
                let filterSassOnly = filter (".sass"`isSuffixOf`)
                lib_root_sass_files <- filterSassOnly <$> getSubPaths dir
                mapM_ processFile lib_root_sass_files
                return ()
          --
          let smpls_path = "examples\\" ++ sm_ver ++ "\\samples"
          smpls_dirs <- concat <$> (getSubPaths smpls_path >>= mapM getSubPaths)
          mapM_ processDir smpls_dirs
          --
          let lib_path = "examples\\" ++ sm_ver ++ "\\libs"
          lib_dirs <- getSubPaths lib_path
          mapM_ processDir lib_dirs
          return ()

        output_root :: FilePath
        output_root = "examples/" ++ sm_ver ++ "/ops"

        processFile :: FilePath -> IO ()
        processFile fp = do
          putStrLn $ "==== collecting ops from " ++ fp
          flns <- zip [1..] . lines <$> readFile fp
          mapM_ (uncurry (processLine fp)) flns

        processLine :: FilePath -> Int -> String -> IO ()
        processLine fp lno lnstr =
            case parseSampleInst lnstr of
              Left _ -> return ()
              Right si -> handle si
                -- | getField128 0 9 (siBits si) == 0x012 -> handle si
                -- | otherwise -> return ()
          where -- so cut and past into literal strings do not need escapes
                fixSlashes = map (\c -> if c == '\\' then '/' else c)
                handle si = do
                  let syntax =
                        printf "%016X`%016X:  " (wHi64 (siBits si)) (wLo64 (siBits si)) ++
                        fmtRawInst (siRawInst si)
                      source_info = fixSlashes fp ++ ":" ++ show lno
                  let base_op = takeWhile (/='.') (riMnemonic (siRawInst si))
                  appendFile (output_root ++ "/" ++ base_op ++ ".sass") $
                     printf "%-80s" syntax ++ " // " ++ source_info ++ "\n"

