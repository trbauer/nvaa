module NVT.Color where

import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

import Control.Exception
import System.IO

putStrYellow :: String -> IO ()
putStrYellow = hPutStrVividColor SCA.Yellow stdout
putStrLnYellow :: String -> IO ()
putStrLnYellow = putStrYellow . (++"\n")
putStrDarkYellow :: String -> IO ()
putStrDarkYellow = hPutStrDullColor SCA.Yellow stdout
putStrLnDarkYellow :: String -> IO ()
putStrLnDarkYellow = putStrDarkYellow . (++"\n")
hPutStrYellow :: Handle -> String -> IO ()
hPutStrYellow = hPutStrVividColor SCA.Yellow
hPutStrDarkYellow :: Handle -> String -> IO ()
hPutStrDarkYellow = hPutStrDullColor SCA.Yellow
--
putStrGreen :: String -> IO ()
putStrGreen = hPutStrVividColor SCA.Green stdout
putStrLnGreen :: String -> IO ()
putStrLnGreen = putStrGreen . (++"\n")
putStrDarkGreen :: String -> IO ()
putStrDarkGreen = hPutStrDullColor SCA.Green stdout
hPutStrGreen :: Handle -> String -> IO ()
hPutStrGreen = hPutStrVividColor SCA.Green
hPutStrDarkGreen :: Handle -> String -> IO ()
hPutStrDarkGreen = hPutStrDullColor SCA.Green
--
hPutStrBlue :: Handle -> String -> IO ()
hPutStrBlue = hPutStrVividColor SCA.Blue
hPutStrDarkBlue :: Handle -> String -> IO ()
hPutStrDarkBlue = hPutStrDullColor SCA.Blue
--
putStrRed :: String -> IO ()
putStrRed = hPutStrVividColor SCA.Red stdout
putStrLnRed :: String -> IO ()
putStrLnRed = putStrRed . (++"\n")
hPutStrRed :: Handle -> String -> IO ()
hPutStrRed = hPutStrVividColor SCA.Red
hPutStrDarkRed :: Handle -> String -> IO ()
hPutStrDarkRed = hPutStrDullColor SCA.Red
--
putStrWhite :: String -> IO ()
putStrWhite = hPutStrVividColor SCA.White stdout
putStrLnWhite :: String -> IO ()
putStrLnWhite = putStrWhite . (++"\n")
hPutStrWhite :: Handle -> String -> IO ()
hPutStrWhite = hPutStrVividColor SCA.White
--
putStrCyan :: String -> IO ()
putStrCyan = hPutStrVividColor SCA.Cyan stdout
putStrLnCyan :: String -> IO ()
putStrLnCyan = putStrCyan . (++"\n")
hPutStrCyan :: Handle -> String -> IO ()
hPutStrCyan = hPutStrVividColor SCA.Cyan
hPutStrDarkCyan :: Handle -> String -> IO ()
hPutStrDarkCyan = hPutStrDullColor SCA.Cyan
--
putStrMagenta :: String -> IO ()
putStrMagenta = hPutStrVividColor SCA.Magenta stdout
putStrLnMagenta :: String -> IO ()
putStrLnMagenta = putStrMagenta . (++"\n")
hPutStrMagenta :: Handle -> String -> IO ()
hPutStrMagenta = hPutStrVividColor SCA.Magenta
hPutStrDarkMagenta :: Handle -> String -> IO ()
hPutStrDarkMagenta = hPutStrDullColor SCA.Magenta
--
hPutStrDullColor :: SCA.Color -> Handle -> String -> IO ()
hPutStrDullColor = hPutStrColoredI SCA.Dull
hPutStrVividColor :: SCA.Color -> Handle -> String -> IO ()
hPutStrVividColor = hPutStrColoredI SCA.Vivid
hPutStrColoredI :: SCA.ColorIntensity -> SCA.Color -> Handle -> String -> IO ()
hPutStrColoredI i c h str = bracket_ acq rel act
  where acq = hFlush h >> SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
        act = hPutStr h str
        rel = SCA.hSetSGR h [SCA.Reset]

