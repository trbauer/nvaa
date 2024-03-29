module NVT.Fmt where

import NVT.Color

import System.IO


data FmtSpan =
  FmtSpan {
    fsStyle :: !FmtStyle
  , fsText :: !String
  } deriving Show

data FmtStyle =
    FmtNONE
  --
  -- R,G,B,Y,C,M
  | FmtRL -- red light
  | FmtRD -- red dark
  --
  | FmtGL
  | FmtGD
  --
  | FmtBL
  | FmtBD
  --
  | FmtYL
  | FmtYD
  --
  | FmtCL
  | FmtCD
  --
  | FmtML
  | FmtMD
  --
  | FmtLW -- light white
  deriving (Show,Eq)

-- infixl 5 <+>
-- (<+>) :: FmtSpan -> FmtSpan -> [FmtSpan]
-- (<+>) = \a b -> [a,b]

fssSimplify :: [FmtSpan] -> [FmtSpan]
fssSimplify [] = []
fssSimplify (FmtSpan _ "":fss) = fss
fssSimplify (FmtSpan s1 t1:FmtSpan s2 t2:fss)
  | s1 == s2 = fssSimplify (FmtSpan s1 (t1++t2):fss)
fssSimplify (fs:fss) = fs:fssSimplify fss

fssToString :: [FmtSpan] -> String
fssToString = concatMap fsText

fssLength :: [FmtSpan] -> Int
fssLength = sum . map (length . fsText)

fssPadR :: Int -> [FmtSpan] -> [FmtSpan]
fssPadR k fss
  | len >= k = fss
  | otherwise = fss ++ [FmtSpan FmtNONE (replicate (k - len) ' ')]
  where len = fssLength fss

fsEmit :: Handle -> [FmtSpan] -> IO ()
fsEmit h_out = loopSpans
  where loopSpans [] = hPutStrLn h_out ""
        loopSpans (fs:fss) = do
          fsEmitStyle h_out (fsStyle fs) (fsText fs)
          loopSpans fss

fsEmitStyle :: Handle -> FmtStyle -> String -> IO ()
fsEmitStyle h_out style =
  case style of
    FmtNONE -> hPutStr h_out
    FmtRL -> hPutStrRed h_out
    FmtRD -> hPutStrDarkRed h_out
    FmtGL -> hPutStrGreen h_out
    FmtGD -> hPutStrDarkGreen h_out
    FmtBL -> hPutStrBlue h_out
    FmtBD -> hPutStrDarkBlue h_out
    FmtYL -> hPutStrYellow h_out
    FmtYD -> hPutStrDarkYellow h_out
    FmtCL -> hPutStrCyan h_out
    FmtCD -> hPutStrDarkCyan h_out
    FmtML -> hPutStrMagenta h_out
    FmtMD -> hPutStrDarkMagenta h_out
    --
    FmtLW -> hPutStrWhite h_out

{-
fsEmitStyleOld :: Handle -> String -> String -> IO ()
fsEmitStyleOld h_out style =
  case style of
    "o" -> hPutStrCyan h_out -- op
    "s" -> hPutStrYellow h_out -- subop
    "n" -> hPutStrDarkCyan h_out -- name (register, label, barrier)
    "c" -> hPutStrDarkGreen h_out -- comment
    "l" -> hPutStrBlue h_out -- label
    "e" -> hPutStrRed h_out -- ERROR
    --
    -- explicit colors
    "ML" -> hPutStrMagenta h_out
    "MD" -> hPutStrDarkMagenta h_out
    "CL" -> hPutStrCyan h_out
    "CD" -> hPutStrDarkCyan h_out
    "YL" -> hPutStrYellow h_out
    "LW" -> hPutStrWhite h_out
    --
    -- unformatted
    "" -> hPutStr h_out
    --
    -- INVALID color code
    _ ->  \s -> hPutStrRed h_out ("<" ++ show style ++ "?>" ++ show s)
-}