module NVT.Color where

import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

import Control.Exception
import System.IO

putStrYellow :: String -> IO ()
putStrYellow = hPutStrColored SCA.Yellow stdout
putStrLnYellow :: String -> IO ()
putStrLnYellow = putStrYellow . (++"\n")
putStrDarkYellow :: String -> IO ()
putStrDarkYellow = hPutStrColoredI SCA.Dull SCA.Yellow stdout
putStrLnDarkYellow :: String -> IO ()
putStrLnDarkYellow = putStrDarkYellow . (++"\n")
hPutStrYellow :: Handle -> String -> IO ()
hPutStrYellow = hPutStrColored SCA.Yellow
hPutStrDarkYellow :: Handle -> String -> IO ()
hPutStrDarkYellow = hPutStrColoredI SCA.Dull SCA.Yellow
--
putStrGreen :: String -> IO ()
putStrGreen = hPutStrColored SCA.Green stdout
putStrLnGreen :: String -> IO ()
putStrLnGreen = putStrGreen . (++"\n")
putStrDarkGreen :: String -> IO ()
putStrDarkGreen = hPutStrColoredI SCA.Dull SCA.Green stdout
hPutStrGreen :: Handle -> String -> IO ()
hPutStrGreen = hPutStrColored SCA.Green
hPutStrDarkGreen :: Handle -> String -> IO ()
hPutStrDarkGreen = hPutStrColoredI SCA.Dull SCA.Green
--
hPutStrBlue :: Handle -> String -> IO ()
hPutStrBlue = hPutStrColored SCA.Blue
hPutStrDarkBlue :: Handle -> String -> IO ()
hPutStrDarkBlue = hPutStrColoredI SCA.Dull SCA.Blue
--
putStrRed :: String -> IO ()
putStrRed = hPutStrColored SCA.Red stdout
putStrLnRed :: String -> IO ()
putStrLnRed = putStrRed . (++"\n")
hPutStrRed :: Handle -> String -> IO ()
hPutStrRed = hPutStrColored SCA.Red
hPutStrDarkRed :: Handle -> String -> IO ()
hPutStrDarkRed = hPutStrColoredI SCA.Dull SCA.Red
--
putStrWhite :: String -> IO ()
putStrWhite = hPutStrColored SCA.White stdout
putStrLnWhite :: String -> IO ()
putStrLnWhite = putStrWhite . (++"\n")
hPutStrWhite :: Handle -> String -> IO ()
hPutStrWhite = hPutStrColored SCA.White
--
putStrCyan :: String -> IO ()
putStrCyan = hPutStrColored SCA.Cyan stdout
putStrLnCyan :: String -> IO ()
putStrLnCyan = putStrCyan . (++"\n")
hPutStrCyan :: Handle -> String -> IO ()
hPutStrCyan = hPutStrColored SCA.Cyan
hPutStrDarkCyan :: Handle -> String -> IO ()
hPutStrDarkCyan = hPutStrColoredI SCA.Dull SCA.Cyan
--
putStrMagenta :: String -> IO ()
putStrMagenta = hPutStrColored SCA.Magenta stdout
putStrLnMagenta :: String -> IO ()
putStrLnMagenta = putStrMagenta . (++"\n")
hPutStrMagenta :: Handle -> String -> IO ()
hPutStrMagenta = hPutStrColored SCA.Magenta
hPutStrDarkMagenta :: Handle -> String -> IO ()
hPutStrDarkMagenta = hPutStrColoredI SCA.Dull SCA.Magenta
--
hPutStrColored :: SCA.Color -> Handle -> String -> IO ()
hPutStrColored = hPutStrColoredI SCA.Vivid
hPutStrColoredI :: SCA.ColorIntensity -> SCA.Color -> Handle -> String -> IO ()
hPutStrColoredI i c h str = bracket_ acq rel act
  where acq = hFlush h >> SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
        act = hPutStr h str
        rel = SCA.hSetSGR h [SCA.Reset]

