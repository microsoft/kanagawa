{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

{-- This file is processed by hsc2hs in order to generate Haskell represenation
    for C constants/enums define in parse_tree.h header file --}

module ParseTree.Types (
      module ParseTree.Types
    ) where

import Foreign
import Text.Megaparsec (SourcePos(..), unPos)

#include "parse_tree.h"

data Location =
    Location
    { locationBegin :: SourcePos
    , locationEnd :: SourcePos
    , fileIndex :: Int
    }

instance Storable Location where
    alignment _ = #{alignment Location}
    sizeOf _ = #{size Location}

    poke ptr Location{..} = do
        #{poke Location, _beginLine } ptr $ unPos $ sourceLine locationBegin
        #{poke Location, _beginColumn } ptr $ unPos $ sourceColumn locationBegin
        #{poke Location, _endLine } ptr $ unPos $ sourceLine locationEnd
        #{poke Location, _endColumn } ptr $ unPos $ sourceColumn locationEnd
        #{poke Location, _fileIndex  } ptr fileIndex

withLocation :: Location -> (Ptr Location -> IO a) -> IO a
withLocation loc f =
    alloca $ \ptr -> do
        poke ptr loc
        f ptr

pattern DeclareFlagGlobal         = #{const DECLARE_FLAG_GLOBAL | DECLARE_FLAG_INITIALIZE}
pattern DeclareFlagClass          = #{const DECLARE_FLAG_GLOBAL | DECLARE_FLAG_INITIALIZE | DECLARE_FLAG_CLASS}
pattern DeclareFlagLocal          = #{const DECLARE_FLAG_INITIALIZE}
pattern DeclareFlagStatic         = #{const DECLARE_FLAG_GLOBAL | DECLARE_FLAG_INITIALIZE | DECLARE_FLAG_STATIC}
pattern DeclareFlagMember         = 0
pattern DeclareFlagUninitConst    = #{const DECLARE_FLAG_UNINIT_CONST}

pattern SizeofTypeBit             = #{const ParseTreeSizeofTypeBit}
pattern SizeofTypeByte            = #{const ParseTreeSizeofTypeByte}

pattern MemberModifierPrivate     = #{const ParseTreeMemberProtectionModifierPrivate}
pattern MemberModifierPublic      = #{const ParseTreeMemberProtectionModifierPublic}

pattern InlineFunc                = #{const ParseTreeFunctionModifierInline}
pattern NoInlineFunc              = #{const ParseTreeFunctionModifierNoInline}

pattern AsyncFunc                 = #{const ParseTreeFunctionModifierAsync}
pattern PipelinedFunc             = #{const ParseTreeFunctionModifierPipelined}
pattern UnorderedFunc             = #{const ParseTreeFunctionModifierUnordered}
pattern ResetFunc                 = #{const ParseTreeFunctionModifierReset}
pattern NoBackPressureFunc        = #{const ParseTreeFunctionModifierNoBackPressure}
pattern PureFunc                  = #{const ParseTreeFunctionModifierPure}
pattern ReorderByLoopingFunc      = #{const ParseTreeFunctionModifierReorderByLooping}

pattern EndTransactionParam       = #{const DECLARE_FLAG_END_TRANSACTION}

pattern CallRateAttr              = #{const ParseTreeCallRateAttr}
pattern FifoDepthAttr             = #{const ParseTreeFifoDepthAttr}
pattern LatencyAttr               = #{const ParseTreeLatencyAttr}
pattern MaxThreadsAttr            = #{const ParseTreeMaxThreadsAttr}
pattern ScheduleAttr              = #{const ParseTreeScheduleAttr}
pattern ThreadRateAttr            = #{const ParseTreeThreadRateAttr}
pattern TransactionSizeAttr       = #{const ParseTreeTransactionSizeAttr}
pattern NameAttr                  = #{const ParseTreeNameAttr}

pattern MemoryDefault             = #{const ParseTreeMemoryTypeDefault}
pattern MemoryInitialize          = #{const ParseTreeMemoryTypeInitialize}
pattern MemoryNorep               = #{const ParseTreeMemoryTypeNoReplication}
pattern MemoryQuadPort            = #{const ParseTreeMemoryTypeQuadPort}
pattern MemoryEcc                 = #{const ParseTreeMemoryTypeEcc}

pattern FormatSpecifierBin        = #{const ParseTreeFormatSpecifierBin}
pattern FormatSpecifierOct        = #{const ParseTreeFormatSpecifierOct}
pattern FormatSpecifierDec        = #{const ParseTreeFormatSpecifierDec}
pattern FormatSpecifierHex        = #{const ParseTreeFormatSpecifierHex}
pattern FormatSpecifierHexUpper   = #{const ParseTreeFormatSpecifierHexUpper}
pattern FormatSpecifierNone       = #{const ParseTreeFormatSpecifierNone}
