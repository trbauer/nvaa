import Control.Monad
import Data.List
import Data.Char
import Debug.Trace
import System.Directory
import System.Exit
import System.FilePath
import Text.Printf
import qualified Data.IntMap.Strict as DIM


formatResults :: Int -> FilePath -> IO ()
formatResults bin_size dir = do
  fs <- filter (".rum"`isSuffixOf`) . sort <$> listDirectory dir
  let emitLib f = do
        let library = dropExtension (takeFileName (dir ++ "/" ++ f))
        putStrLn $ library ++ ":"
        fstr <- readFile f
        return (library, read fstr)
  rs <- mapM emitLib fs
  let col0 = "Bin Size":binKeys bin_size
      cols = col0:map (\(nm,r) -> nm:map show (binHist bin_size r)) rs
      rows = transpose cols
  writeFile "last.csv" $
    concatMap (\rs -> intercalate "," rs ++ "\n") rows
  let fmtRow (r0:rs) =
        printf "%-16s" r0 ++ "  " ++
          intercalate " " (map (printf "%12s") rs) ++ "\n"
  putStrLn $ concatMap fmtRow rows

scanDirs :: IO ()
scanDirs = do
  dirs <- listDirectory "." >>= filterM doesDirectoryExist >>= return . sort
  _ <- mapM scanDir dirs
  return ()

scanDir :: FilePath -> IO RegUseMap
scanDir dir = do
  sass_fs <- map ((dir++"/")++) . filter (".sass"`isSuffixOf`) <$> listDirectory dir
  rum <- rumUnions <$> mapM scanSassFile sass_fs
  let library = dropExtension (takeFileName dir)
  writeFile (library ++ ".rum") $ show rum
  putStrLn $ library ++ ":"
  putStrLn $ emitHist True 32 rum
  return rum

scanSassFile :: FilePath -> IO RegUseMap
scanSassFile sass_fp = do
  -- putStrLn $ sass_fp ++ " ..."
  -- flns <- lines <$> readFile sass_fp
  -- mapM_ (print . addLine rumEmpty) flns
  rum <- foldl' addLine rumEmpty <$> lines <$> readFile sass_fp
  DIM.size rum `seq` return ()
  return rum

--  .sectioninfo  @"SHI_REGISTERS=12"
addLine :: RegUseMap -> String -> RegUseMap
addLine rum = go
  where go [] = rum
        go s@(_:cs)
          | pfx`isPrefixOf`s =
            case reads (drop (length pfx) s) of
              [(regs,'\"':_)] -> DIM.insertWith (+) regs 1 rum
              _ -> go cs
          | otherwise = go cs
          where pfx = "\"SHI_REGISTERS="

binKeys :: Int -> [String]
binKeys bin_size = binFrom 0
  where binFrom off
          | off >= 256 = []
          | otherwise = key:binFrom (off + bin_size)
          where key = printf "%d..%d" off (min 255 (off + bin_size - 1)) :: String

binHist :: Int -> RegUseMap -> [Int]
binHist bin_size rum = binFrom 0
  where binFrom off
          | off >= 256 = []
          | otherwise = n:binFrom (off + bin_size)
          where n = foldl' (+) 0 $ map addBinElem [0 .. bin_size - 1] :: Int

                addBinElem i =
                  case (off+i)`DIM.lookup`rum of
                    Nothing -> 0
                    Just k -> k


emitHist :: Bool -> Int -> RegUseMap -> String
emitHist csv bin_size rum = emitBin 0
  where emitBin off
          | off >= 256 = ""
          | otherwise = line ++ "\n" ++ emitBin (off + bin_size)
          where line
                  | csv = key ++ "," ++ show n
                  | otherwise = printf "%-24s   %d" key n :: String
                key = printf "%d..%d" off (off + bin_size - 1) :: String

                n = foldl' (+) 0 $ map addBinElem [0 .. bin_size - 1] :: Int

                addBinElem i =
                  case (off+i)`DIM.lookup`rum of
                    Nothing -> 0
                    Just k -> k

type RegUseMap = DIM.IntMap Int

rumEmpty :: RegUseMap
rumEmpty = DIM.empty

rumUnions :: [RegUseMap] -> RegUseMap
rumUnions = DIM.unionsWith (+)

