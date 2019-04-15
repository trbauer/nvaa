module NVT.Parsers.ParseNvdisasmListing where

import NVT.Parsers.Parser
import NVT.Bits

-- NOTES from nvdisasm analyis:
--
--  * .word is 32b, not 16, .short is 16b, .dword is 64
--
--  * index@(S) means symbol index of S
--      readelf -s ... for symbols
--
--  * any section referenced via index@...
--      gets placed into the symbol table (so it's an on demand op)
--
--  * the string literal in section definitions seems to match
--    the access flags in the section header
--      .section  .nv.constant0.nanosleep_kernel,"a",@progbits
--                                               ^^^
--      .section  .text.nanosleep_kernel,"ax",@progbits
--                                       ^^^^
--
--  * the @progbits means SH_PROGBITS
--
--  * the sh_info field for text sections (kernels)
--      seems to hold (num regs in top byte)
--    .sectioninfo  @"SHI_REGISTERS=30"
--      readelf: 0000000000000b00  0000000000000000  AX       3   503316484     128
--       503316484 == 0x1E000004
--                      ^^ 30
--       I have no idea what the other bits mean;
--       maybe the index to this symbol table entry???
--
--  * the section header alignmet appears in program text too
--     .section  .text.nanosleep_kernel,"ax",@progbits
--     .sectioninfo  @"SHI_REGISTERS=30"
--     .align  128
--      ^^^^^^^^^^ here
--     .global         nanosleep_kernel
--     .type           nanosleep_kernel,@function
--

-- for an entry that must go in .symtab
data ElfSymbol =
 ElfSymbol {
    esType :: !ElfSymbolType
  , esIndex :: !Int
  , esBinding :: ElfSymbolBinding -- GLOBAL/LOCAL
  } deriving (Show,Eq)

-- st_info is a 2x4b elements
data ElfSymbolType =
      EST_NOTYPE   -- 0
    | EST_OBJECT   -- 1
    | EST_FUNC     -- 2
    | EST_SECTION  -- 3
    | EST_FILE     -- 4
    | EST_UNKNOWN !Word8
  deriving (Show,Eq)
data ElfSymbolBinding =
      ESB_LOCAL  -- 0
    | ESB_GLOBAL -- 1
    | ESB_WEAK   -- 2
    | ESB_UNKNOWN !Word8
  deriving (Show,Eq)


parseSassListing :: FilePath -> String -> IO (Either Diagnostic Bits)
parseSassListing fp asm = do
  xe <- runP pSassListing (initSt fp)
  case xe of
    Left excp -> return excp
    Right ioedb -> ioedb

pSassListing :: P Bits
pSassListing = do
  pSpaces
  --  .headerflags  @"EF_CUDA_TEXMODE_UNIFIED EF_CUDA_64BIT_ADDRESS EF_CUDA_SM75 EF_CUDA_VIRTUAL_SM(EF_CUDA_SM75)"
  hdr_flags <- pHeaderDirective ".headerflags"

-- .foo @"bar baz"
pHeaderDirective :: String -> P [String]
pHeaderDirective dir = do
  pSymbol dir
  P.char '@'
  words <$> pString

-- functional approach would construct the binary in place
-- start the elf
--
-- section parsers return a continuation which completes
-- once all sections are parsed
--
--
-- TRANSLATION APPROACH:
--   data BitsChunk =
--      BitsLiteral     !Bits
--    | BitsVariableU64 !Loc !String
--    | BitsVariableU32 !Loc !String
--    | BitsVariableU16 !Loc !String
--
-- FUNCTIONAL APPROACH:
--   pnvSection :: P ([P Bits])
--   pnvSection =
--     1. reset the offset since all references are relative
--     2. return the chunk of backpatch processors to run
--
--   pnvParseListing :: P Bits
--   pnvParseListing = do
--     ss <- P.many pnvSection
--     -- defineSymbolTableIndexes ...
--     bConcat <$> sequence ss
--     define index@(...) when a symbol that needs to
--     go into the symbol table appears?
--
--  BETTER APPROACH may be to changes P to GP (for generic parser)
--  then override it with my state: P ElfSymbols
--
