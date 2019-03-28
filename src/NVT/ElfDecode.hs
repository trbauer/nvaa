module NVT.ElfDecode where


import NVT.Bits

-- tracing decoder for ELF files
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Text.Printf
import qualified Data.ByteString as S

import qualified Control.Monad.Trans.State as CMS
import Control.Monad.IO.Class(liftIO)

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

-----------------------------------------------------

testElf :: FilePath -> IO ()
testElf fp = do
  bs <- S.readFile fp
  decodeElf dft_edo{edoDebug = putStr} bs >>= print

-- TODO: eventually this will be object file IR
type IR = Int

decodeElf :: ElfDecodeOpts -> S.ByteString -> IO IR
decodeElf edo bs = do
  let dst_init :: DecSt
      dst_init = DecSt [] (edoDebug edo) bs 0 bs
  (a,dst) <- CMS.runStateT dElf dst_init
  let fmtWarning (off,msg) = printf "%06X. " off ++ msg
  mapM (putStrLn . fmtWarning) (dsWarnings dst)
  return a

--
type D = CMS.StateT DecSt IO
data DecSt =
  DecSt {
    dsWarnings :: [(Int,String)]
  , dsLog :: !(String -> IO ())
  , dsBits :: !S.ByteString
  --
  , dsOffset :: !Int
  , dsSuffix :: !S.ByteString
  }
dIO :: IO a -> D a
dIO = liftIO
dFail :: String -> D a
dFail msg = CMS.gets dsOffset >>= flip dFailAt msg
dFailAt :: Int -> String -> D a
dFailAt off msg = -- TODO: exception
  fail (show off ++ ". " ++ msg)
dLog :: String -> D ()
dLog s = do
  log <- CMS.gets dsLog
  dIO $ log s
dLogLn :: String -> D ()
dLogLn s = dLog (s ++ "\n")

dTrace :: Int -> String -> D ()
dTrace k str = do
  off <- CMS.gets dsOffset
  sfx <- CMS.gets dsSuffix
  let w8s = take k (S.unpack sfx)
      ellipsis = if length w8s < k then "" else ", ..."
  when (k > 0) $
    dIO (putStrLn (printf "%05X: " off ++ " {" ++ intercalate ", " (map (printf "%02X") w8s) ++ ellipsis))
  dIO (putStrLn str)

dPeek :: D a -> D a
dPeek da = do
  dst <- CMS.get
  a <- da
  CMS.put dst
  return a

---------------------------------

dElf :: D IR
dElf = do
  bs <- CMS.gets dsSuffix
  let is_32b = S.unpack (S.take 1 (S.drop 4 bs)) == [1]
  let is_msb = S.unpack (S.take 1 (S.drop 5 bs)) == [2]
  dElfWithHeaderInfo is_32b is_msb

-- SPECIFY: should we use a phantom type for the
--   dAddr
dElfWithHeaderInfo :: Bool -> Bool -> D IR
dElfWithHeaderInfo is_32b is_msb = dHeader
  where bit_size = if is_32b then "32" else "64"
        dHeader = do
          args <-
            dStruct ("Elf" ++ bit_size ++ "_Ehdr") $ do
              dStructField "e_ident[EI_MAG0..3]" (dU32 `mustBeEq` 0x464C457F) fmt0xHex -- "\\x7FELF"
              dStructField "e_ident[EI_CLASS]" dU8 $ \x ->
                case x of
                  0 -> "ELFCLASSNONE"
                  1 -> "ELFCLASS32"
                  2 -> "ELFCLASS64"
                  x -> fmt0xHex x ++ "???"
              dStructField "e_ident[EI_DATA]" dU8 $ \x ->
                case x of
                  0 -> "ELFDATANONE"
                  1 -> "ELFDATA2LSB"
                  2 -> "ELFDATA2MSB"
                  x -> fmt0xHex x ++ "???"
              dStructField "e_ident[EI_VERSION]" dU8 fmt0xHex
              dStructField "e_ident[EI_OSABI]" dU8 $ \x ->
                case x of
                  0 -> "ELFOSABI_NONE"
                  1 -> "ELFOSABI_HPUX"
                  2 -> "ELFOSABI_NETBSD"
                  3 -> "ELFOSABI_LINUX"
                  4 -> "ELFOSABI_HURD"
                  5 -> "ELFOSABI_86OPEN"
                  6 -> "ELFOSABI_SOLARIS"
                  7 -> "ELFOSABI_AIX"
                  8 -> "ELFOSABI_IRIX"
                  9 -> "ELFOSABI_FREEBSD"
                  10 -> "ELFOSABI_TRU64"
                  11 -> "ELFOSABI_MODESTO"
                  12 -> "ELFOSABI_OPENBSD"
                  13 -> "ELFOSABI_OPENVMS"
                  14 -> "ELFOSABI_NSK"
                  15 -> "ELFOSABI_AROS"
                  97 -> "ELFOSABI_ARM"
                  255 -> "ELFOSABI_STANDALONE"
                  x -> fmt0xHex x ++ "???"
              dStructField "e_ident[EI_ABIVERSION]" dU8 fmt0xHex
              dStructField "e_ident[9..15]" (dArray 7 dU8) (\as -> "{" ++ intercalate "," (map fmt0xHex as) ++ "}")

              dStructField "e_type" dU16 $ \x ->
                case x of
                  0 -> "ET_NONE"
                  1 -> "ET_REL"
                  2 -> "ET_EXEC"
                  3 -> "ET_DYN"
                  4 -> "ET_CORE"
                  x
                    | x >= 0xFE00 && x < 0xFEFF -> "0x" ++ fmt0xHex x ++ " (operating system specific)"
                    | x >= 0xFF00 -> "0x" ++ fmt0xHex x ++ " (processor specific)"
                    | otherwise -> show x ++ "???"
              dStructField "e_machine" dU16 $ \x ->
                case x of
                  0 -> "EM_NONE"
                  1 -> "EM_M32"
                  2 -> "EM_SPARC"
                  3 -> "EM_386"
                  4 -> "EM_68K"
                  5 -> "EM_88K"
                  7 -> "EM_860"
                  8 -> "EM_MIPS"
                  0xBE -> "EM_NVDA" -- my addition, readelf
                  x -> "0x" ++ fmt0xHex x ++ "???"
              dStructField "e_version" dU32 fmt0xHex
              dStructFieldAddr "e_entry"
              e_phoff <-
                dStructField "e_phoff" dOff fmt0xHex
              e_shoff <-
                dStructField "e_shoff" dOff fmt0xHex
              dStructField "e_flags" dU32 fmt0xHex

              dStructField "e_ehsize" dU16 fmt0xHex
              e_phentsize <-
                dStructField "e_phentsize" dU16 fmt0xHex
              e_phnum <-
                dStructField "e_phnum" dU16 fmt0xHex
              e_shentsize <-
                dStructField "e_shentsize" dU16 fmt0xHex
              e_shnum <-
                dStructField "e_shnum" dU16 fmt0xHex
              e_shstrndx <-
                dStructField "e_shstrndx" dU16 fmt0xHex
              return (e_phoff,e_shoff,e_phentsize,e_phnum,e_shentsize,e_shnum,e_shstrndx)
          dTrace 16 "AFTER HEADER"
          dBody args

        dAddr :: D Word64
        dAddr
          | is_32b = fromIntegral <$> dU32
          | otherwise = dU64
        dOff :: D Word64
        dOff
          | is_32b = fromIntegral <$> dU32
          | otherwise = dU64

        -- overridden so the formatting is the right width for an address
        -- (depending on if it's 64b or not)
        dStructFieldAddr :: String -> D Word64
        dStructFieldAddr fnm = dStructField fnm dAddr fmt0xHexAddr
        dStructField3264 :: String -> D Word64
        dStructField3264 = dStructFieldAddr

        fmt0xHexAddr
          | is_32b = fmt0xHex . (fromIntegral :: Word64 -> Word32)
          | otherwise = fmt0xHex

        (dU16,dU32,dU64)
          | is_msb = (dU16_BE,dU32_BE,dU64_BE)
          | otherwise = (dU16_LE,dU32_LE,dU64_LE)

        fmtBitSet :: (Integral b,FiniteBits b,PrintfArg b) => [(b,String)] -> b -> String
        fmtBitSet tbl = go [] tbl
          where go rtks _  0 = intercalate "|" rtks
                go rtks [] v = intercalate "|" rtks ++ "|(" ++ fmt0xHex v ++ "???)"
                go rtks ((mask,str):tbl) v
                  | mask .&. v /= 0 = go (str:rtks) tbl (v .&. complement mask)
                  | otherwise = go rtks tbl v

        dBody :: (Word64, Word64, Word16, Word16, Word16, Word16, Word16) -> D IR
        dBody (e_phoff,e_shoff,e_phentsize,e_phnum,e_shentsize,e_shnum,e_shstrndx) = doIt
          where doIt = do
                  dSectionHeaders
                  dProgramHeaders
                  return 0

                dProgramHeaders = dWithSeekTo (fromIntegral e_phoff) $ do
                  sequence (replicate (fromIntegral e_shnum) dProgramHeader)

                dProgramHeader = do
                  dStruct ("Elf"++bit_size++"_Phdr") $ do
                    p_type <- dStructField "p_type" dU32 $ \x ->
                      case x of
                        0 -> "PT_NULL"
                        1 -> "PT_LOAD"
                        2 -> "PT_DYNAMIC"
                        3 -> "PT_INTERP"
                        4 -> "PT_NOTE"
                        5 -> "PT_SHLIB"
                        6 -> "PT_PHDR"
                        7 -> "PT_TLS"
                        0x6464e550 -> "PT_SUNW_UNWIND"
                        0x6474e550 -> "PT_GNU_EH_FRAME"
                        0x6474e551 -> "PT_GNU_STACK"
                        0x6474e552 -> "PT_GNU_RELRO"
                        x -> fmt0xHex x ++ "???"
                    let pFlagsField =
                          dStructField "p_flags" dU32 $ fmtBitSet
                              [(0x001,"PF_X")
                              ,(0x002,"PF_W")
                              ,(0x004,"PF_R")]

                    p_flags_64b <- if not is_32b then pFlagsField else return 0
                    p_offset <- dStructFieldAddr "p_offset"
                    p_vaddr <- dStructFieldAddr "p_vaddr"
                    p_paddr <-dStructFieldAddr "p_paddr"
                    p_filesz <- dStructFieldAddr "p_filesz"
                    p_memsz <- dStructFieldAddr "p_memsz"
                    p_flags_32b <- if is_32b then pFlagsField else return 0
                    let p_flags
                          | is_32b = p_flags_32b
                          | otherwise = p_flags_64b
                    p_align <- dStructFieldAddr "p_align"
                    return (p_type,p_flags,p_offset,p_vaddr,p_paddr,p_filesz,p_memsz,p_align)








                dSectionHeaders = dWithSeekTo (fromIntegral e_shoff) $ do
                  shs <- sequence (replicate (fromIntegral e_shnum) dSectionHeader)
                  return 0

                lookupString :: Word32 -> D String
                lookupString off = do
                  bs <- strtab_bits
                  let bs_null_term = S.takeWhile (/=0) (S.drop (fromIntegral off) bs)
                      w8ToChar :: Word8 -> Char
                      w8ToChar = chr . fromIntegral
                  return $ map w8ToChar (S.unpack bs_null_term)

                strtab_bits :: D S.ByteString
                strtab_bits = getSectionBits (fromIntegral e_shstrndx)

                getSectionBits :: Int -> D S.ByteString
                getSectionBits ix =
                  -- jump to section header table index
                  dWithSeekTo (fromIntegral e_shoff + fromIntegral e_shentsize * ix) $ do
                    -- skip: sh_name, sh_type, sh_flags, sh_addr,
                    dU32 >> dU32 >> dAddr >> dAddr
                    sh_offset <- dAddr
                    sh_size <- dAddr
                    let extract = S.take (fromIntegral sh_size) . S.drop (fromIntegral sh_offset)
                    extract <$> CMS.gets dsBits

                dSectionHeader :: D (Word32,Int,Int)
                dSectionHeader = do
                  (sh_type,sh_offset,sh_size) <-
                    dStruct ("Elf"++bit_size++"_Shdr") $ do
                      snm <- fromIntegral <$> dPeek dU32
                      sh_nm_str <- lookupString snm
                      sh_name <- dStructField "sh_name" dU32 $ \x -> fmt0xHex x ++ " (" ++ sh_nm_str ++ ")"
                      sh_type <- dStructField "sh_type" dU32 $ \x ->
                        case x of
                          0 -> "SHT_NULL"
                          1 -> "SHT_PROGBITS"
                          2 -> "SHT_SYMTAB"
                          3 -> "SHT_STRTAB"
                          4 -> "SHT_RELA"
                          5 -> "SHT_HASH"
                          6 -> "SHT_DYNAMIC"
                          7 -> "SHT_NOTE"
                          8 -> "SHT_NOBITS"
                          9 -> "SHT_REL"
                          10 -> "SHT_SHLIB"
                          11 -> "SHT_DYNSYM"
                          14 -> "SHT_INIT_ARRAY"
                          15 -> "SHT_FINI_ARRAY"
                          16 -> "SHT_PREINIT_ARRAY"
                          17 -> "SHT_GROUP"
                          18 -> "SHT_SYMTAB_SHNDX"
                          0x70000001 -> "SHT_AMD64_UNWIND"
                          0x70000006 -> "SHT_MIPS_REGINFO"
                          0x7000000d -> "SHT_MIPS_OPTIONS"
                          0x7000001e -> "SHT_MIPS_DWARF"
                          x -> fmt0xHex x ++ "???"

                      -- technically Elf64_Xword (64b) for 64b ELF or Elf32_Word (32b) for 32b
                      sh_flags <- dStructField "sh_flags" dAddr $ fmtBitSet
                          [(0x001,"SHF_WRITE")
                          ,(0x002,"SHF_ALLOC")
                          ,(0x004,"SHF_EXECINSTR")
                          ,(0x010,"SHF_MERGE")
                          ,(0x020,"SHF_STRINGS")
                          ,(0x040,"SHF_INFO_LINK")
                          ,(0x080,"SHF_LINK_ORDER")
                          ,(0x100,"SHF_OS_NONCONFORMING")
                          ,(0x200,"SHF_GROUP")
                          ,(0x400,"SHF_TLS")]
                      sh_addr      <- dStructField3264 "sh_addr"
                      sh_offset    <- dStructField3264 "sh_offset"
                      sh_size      <- dStructField3264 "sh_size"
                      sh_link      <- dStructField "sh_link" dU32 fmt0xHex
                      sh_info      <- dStructField "sh_info" dU32 fmt0xHex
                      sh_addralign <- dStructField3264 "sh_addralign"
                      sh_entsize   <- dStructField3264 "sh_entsize"
                      return (sh_type,sh_offset,sh_size)
                  return (sh_type,fromIntegral sh_offset,fromIntegral sh_size)






dStruct :: String -> D a -> D a
dStruct s da = do
  dLogLn ("  " ++ s ++ " {")
  a <- da
  dLogLn "  }"
  return a
dStructField :: String -> D a -> (a -> String) -> D a
dStructField fnm da fmt = do
  dLog $ "    " ++ padR 24 fnm ++ " = "
  a <- da
  dLogLn $ fmt a ++ ";"
  return a

-- absolute seek
dWithSeekTo :: Int -> D a -> D a
dWithSeekTo off da = do
  dst <- CMS.get
  when (off < 0 || off > S.length (dsBits dst)) $
    dFailAt off ("target address is out of bounds (called from " ++ show (dsOffset dst) ++ ")")
  CMS.put dst{dsOffset = off,dsSuffix = S.drop off (dsBits dst)}
  a <- da
  dst_n <- CMS.get
  CMS.put dst_n{dsOffset = dsOffset dst,dsSuffix = S.drop (dsOffset dst) (dsBits dst)}
  return a

padR :: Int -> String -> String
padR k s = s ++ replicate (k - length s) ' '

fmtHex :: (PrintfArg i,FiniteBits i,Integral i) => i -> String
fmtHex i = printf ("%0" ++ show (2*bytes) ++ "X") i
  where bytes = finiteBitSize i `div` 8
fmt0xHex i = "0x" ++ fmtHex i


mustBe :: (Eq a,Show a) => D a -> (a -> Bool) -> D a
mustBe da pr = do
  off <- CMS.gets dsOffset
  a <- da
  if not (pr a) then dFailAt off "invalid value"
    else return a
mustBeEq :: (Eq a,Show a) => D a -> a -> D a
mustBeEq da eq = da `mustBe` (==eq)


dU8 :: D Word8
dU8 = dU fromByteStringU8
dU16_LE, dU16_BE :: D Word16
dU16_LE = dU fromByteStringLE
dU16_BE = dU fromByteStringBE
dU32_LE, dU32_BE :: D Word32
dU32_LE = dU fromByteStringLE
dU32_BE = dU fromByteStringBE
dU64_LE, dU64_BE :: D Word64
dU64_LE = dU fromByteStringLE
dU64_BE = dU fromByteStringBE

dU :: (FiniteBits u,Integral u,Show u) => (S.ByteString -> u) -> D u
dU from_byte_string = do
  dst <- CMS.get
  let num_bytes = finiteBitSize (from_byte_string undefined) `div` 8
      (bs,sfx) = S.splitAt num_bytes (dsSuffix dst)
  if S.length bs < num_bytes
    then dFail "underflow (insufficient bytes for data type)"
    else do
      let a = from_byte_string (dsSuffix dst)
      CMS.put $ dst{dsOffset = dsOffset dst + num_bytes, dsSuffix = sfx}
      return a
dArray :: Int -> D a -> D [a]
dArray k = sequence . replicate k







