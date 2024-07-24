import System.IO
import Data.List
import Data.Int
import Data.Word

run :: FilePath -> IO ()
run fp = do
  flns <- lines <$> readFile fp
  length flns `seq` return ()
  let ns = map (read . head . drop 1 . words) flns
      diffs = zipWith subtract ns (drop 1 ns)
  mapM_ print diffs
  print diffs
