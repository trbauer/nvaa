module NVT.IR.Cubin where

import NVT.IR.Syntax
import NVT.IR.Types
import NVT.IR.LExpr

import Data.Word
import qualified Data.ByteString as BS


data CubinListing =
  CubinListing {
    -- TODO: break down to attributes
    -- 	.headerflags	@"EF_CUDA_TEXMODE_UNIFIED EF_CUDA_64BIT_ADDRESS EF_CUDA_SM80 EF_CUDA_VIRTUAL_SM(EF_CUDA_SM80)"
    --  :: [CubinHeaderAttr]
    --  data EF_CUDA_TEXMODE_UNIFIED
    cblHeaderFlags :: ![HeaderFlag]
  , cblElfType :: !ElfType
  , cblSections :: !Section
  } deriving Show

data ElfType = ET_EXEC -- .elftype	@"ET_EXEC"
  deriving (Show,Eq)

data HeaderFlag =
    EF_CUDA_TEXMODE_UNIFIED
  | EF_CUDA_64BIT_ADDRESS
  | EF_CUDA_SM80 | EF_CUDA_SM75 | EF_CUDA_SM72 | EF_CUDA_SM70
  | EF_CUDA_VIRTUAL_SM !HeaderFlag -- EF_CUDA_VIRTUAL_SM(EF_CUDA_SM80)
  deriving (Show,Eq)

data Section =
  Section {
    sName :: !String
  , sType :: !SHT
  , sFlag :: ![SHF]
  , sInfo :: ![SHI]
  , sOther :: ![SHO]
  , sBody :: !SectionBody
  } deriving Show
--	.sectionflags	@"SHF_BARRIERS=1"
--	.sectioninfo	@"SHI_REGISTERS=31"
-- gives
--    e   5380   2700  0 80  PROGBITS  100006    3 1f00000b .text._Z11cudaProcessPjiiiiffy
--                                     ^ bars      ^^ regs

-- ELF section header type
data SHT =
    SHT_NULL --
  | SHT_PROGBITS -- @progbits
  | SHT_SYMTAB
  | SHT_STRAB
  | SHT_CUDA_INFO -- @"SHT_CUDA_INFO"
  -- ...
  deriving (Eq,Show,Enum)

data SHF =
    SHF_WRITE -- w
  | SHF_ALLOC -- a
  | SHF_EXECINSTR -- x
  | SHF_MERGE
  | SHF_STRINGS
  | SHF_INFO_LINK
  | SHF_LINK_ORDER
  -- TODO: other ELF standards ....
  --
  --------------------
  -- CUDA stuff
  | SHF_BARRIERS !Int -- @sectionsflags @"SHF_BARRIERS=1"
  --
  | SHF_OTHER !Word32
  deriving (Eq,Show)

data SHI =
    SHI_REGISTERS !Int -- @"SHI_REGISTERS=31" (encoded in type byte)
  | SHI_OTHER !Word32 -- bottom 24b
  deriving (Eq,Show)

data SHO =
    STO_CUDA_ENTRY
  | STV_DEFAULT
  deriving (Eq,Show)


data SectionBody =
    SectionBodyBinary ![Bits]
  | SectionBodyText   ![TextChunk]
  deriving (Show)
data TextChunk =
    TextChunkInst !Inst
  | TextChunkBits !Bits
  | TextChunkLabel !String
  deriving (Show)

data Bits =
    BitsW8s     !BS.ByteString -- .byte ......
  | BitsS       !Word16  -- .short
  | BitsSLabel  !LExpr   -- .short (...)
  | BitsW       !Word32  -- .word
  | BitsWLabel  !LExpr   -- .word
  | BitsDW      !Word64  -- .dword ...
  | BitsDWLabel !LExpr   -- .dword (...)
  --
  | BitsAlign   !Int -- .align XXX
  | BitsZero    !Int -- .zero XXXX
  deriving (Eq,Show)



----------------------------------------------------------------------------
instance Syntax CubinListing where
  format _ = "TODO: format<CubinListing>"


instance Syntax HeaderFlag where
  format (EF_CUDA_VIRTUAL_SM arg) = "EF_CUDA_VIRTUAL_SM(" ++ show arg ++ ")"
  format x = show x

instance Syntax ElfType where
  format = show

instance Syntax SHT where
  format = lowerCase . dropWhile (/='_') . show


