module NVT.ElfDecode where

-- tracing decoder for ELF files
import Data.Bits
import Data.Word
import qualified Data.ByteString as S

data ElfDecodeOpts =
  ElfDecodeOpts {
    edoDebug :: !(String -> IO ())
  , edoVerbosity :: !Int
  }

dft_edo :: ElfDecodeOpts
dft_edo =
  ElfDecodeOpts {
    edoDebug = const (return ())
  , edoVerbosity = 0
  }


decodeElf :: ElfDecodeOpts -> S.ByteString -> IO ()
decodeElf edo bs = do
  putStrLn $ "TODO: handle .cubin file"
  return ()