module Time where

import Data.Int
-- import qualified Data.Time as DT
import qualified Data.Time.Clock.System as DT

{-
timeS :: IO a -> IO (a,Double)
timeS ioa = do
  ts <- DT.getCurrentTime
  a <- ioa
  te <- DT.getCurrentTime
  return (a,fromRational (toRational (te `DT.diffUTCTime` ts)))
-}

timeS :: IO a -> IO (a,Double)
timeS ioa = do
  ts <- timeCurrent
  a <- ioa
  te <- timeCurrent
  return (a,te`timeElapsedS`ts)

timeElapsedS :: DT.SystemTime -> DT.SystemTime -> Double
timeElapsedS te ts = fromIntegral (timeElapsedNS te ts) / 1e9
  where stToS t =
          fromIntegral (DT.systemSeconds t) +
            fromIntegral (DT.systemNanoseconds t) / 1e9

timeElapsedNS :: DT.SystemTime -> DT.SystemTime -> Int64
timeElapsedNS te ts = stToNanos te - stToNanos ts
  where stToNanos t =
            (DT.systemSeconds t)*1000*1000*1000 +
                fromIntegral (DT.systemNanoseconds t)

timeCurrent :: IO DT.SystemTime
timeCurrent = DT.getSystemTime
