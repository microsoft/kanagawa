{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

{-- This file is processed by hsc2hs in order to generate Haskell represenation
    for C constants/enums define in parse_tree.h header file --}

module ParseTree.FFI (
      module ParseTree.FFI
    ) where

import Foreign
import Foreign.C.String
import Options
import ParseTree.Types (Location)

data Node
type ScopePtr = Ptr CString
type QualifiedNamePtr = Ptr CString
type NodePtr = Ptr Node


foreign import ccall "InitCompiler"                      initCompiler                      :: Ptr Options -> IO Bool
foreign import ccall "Codegen"                           codegen                           :: CString -> CString -> CString -> CString -> CString -> NodePtr -> IO Bool
foreign import ccall "SetLocation2"                      setLocation                       :: Ptr Location -> IO()
foreign import ccall "SetNodeType"                       setNodeType                       :: NodePtr -> NodePtr -> IO()
foreign import ccall "UnknownLocation"                   unknownLocation                   :: IO()
foreign import ccall "ParseAccessArray"                  parseAccessArray                  :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAccessArrayLValue"            parseAccessArrayLValue            :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAccessMember"                 parseAccessMember                 :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAnnotatedStatement"           parseAnnotatedStatement           :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAppendList"                   parseAppendList                   :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseArrayType"                    parseArrayType                    :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAssign"                       parseAssign                       :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseAttribute"                    parseAttribute                    :: Int -> NodePtr -> IO NodePtr
foreign import ccall "ParseBarrier"                      parseBarrier                      :: IO NodePtr
foreign import ccall "ParseBaseList"                     parseBaseList                     :: NodePtr -> IO NodePtr
foreign import ccall "ParseBinaryOp"                     parseBinaryOp                     :: Int -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseBoolLiteral"                  parseBoolLiteral                  :: Int -> IO NodePtr
foreign import ccall "ParseBoolType"                     parseBool                         :: IO NodePtr
foreign import ccall "ParseCast"                         parseCast                         :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseClass"                        parseClass                        :: NodePtr -> NodePtr -> NodePtr -> ScopePtr -> CString -> IO NodePtr
foreign import ccall "ParseConcat"                       parseConcat                       :: NodePtr -> IO NodePtr
foreign import ccall "ParseConst"                        parseConst                        :: NodePtr -> IO NodePtr
foreign import ccall "ParseDecimalLiteral"               parseDecimalLiteral               :: CString -> IO NodePtr
foreign import ccall "ParseDeclare"                      parseDeclare                      :: NodePtr -> NodePtr -> NodePtr -> NodePtr -> Int -> ScopePtr -> IO NodePtr
foreign import ccall "ParseDesignator"                   parseDesignator                   :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseDefaultInitialization"        parseDefaultInitialization        :: NodePtr -> IO NodePtr
foreign import ccall "ParseDoWhile"                      parseDoWhile                      :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseEnum"                         parseEnum                         :: NodePtr -> NodePtr -> NodePtr -> ScopePtr -> IO NodePtr
foreign import ccall "ParseEnumConstant"                 parseEnumConstant                 :: NodePtr -> NodePtr -> ScopePtr -> IO NodePtr
foreign import ccall "ParseEnumValue"                    parseEnumValue                    :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseExportType"                   parseExportType                   :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseExtern"                       parseExtern                       :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseExternalFunction"             parseExternalFunction             :: NodePtr -> NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseFanOut"                       parseFanOut                       :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseFlagAttribute"                parseFlagAttribute                :: Int -> IO NodePtr
foreign import ccall "ParseFloatLiteral"                 parseFloatLiteral                 :: Float -> IO NodePtr
foreign import ccall "ParseFloatType"                    parseFloatType                    :: IO NodePtr
foreign import ccall "ParseFunction"                     parseFunction                     :: NodePtr -> NodePtr -> NodePtr -> NodePtr -> NodePtr -> NodePtr -> CString -> IO NodePtr
foreign import ccall "ParseFunctionCall"                 parseFunctionCall                 :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseFunctionModifier"             parseFunctionModifier             :: Int -> IO NodePtr
foreign import ccall "ParseFunctionParam"                parseFunctionParam                :: NodePtr -> NodePtr -> NodePtr -> ScopePtr -> IO NodePtr
foreign import ccall "ParseFunctionSpecifier"            parseFunctionSpecifier            :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseFunctionType"                 parseFunctionType                 :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseFunctionTypeParam"            parseFunctionTypeParam            :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseIdentifier"                   parseIdentifier                   :: CString -> IO NodePtr
foreign import ccall "ParseIf"                           parseIf                           :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseInitializerList"              parseInitializerList              :: NodePtr -> IO NodePtr
foreign import ccall "ParseIntAttribute"                 parseIntAttribute                 :: Int -> NodePtr -> IO NodePtr
foreign import ccall "ParseInterpolationExpression"       parseInterpolationExpression       :: NodePtr -> NodePtr -> Int -> Int -> IO NodePtr
foreign import ccall "ParseInterpolatedString"           parseInterpolatedString           :: NodePtr -> IO NodePtr
foreign import ccall "ParseInterpolatedStringSegment"    parseInterpolatedStringSegment    :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseIntType"                      parseIntType                      :: Int -> IO NodePtr
foreign import ccall "ParseMemberModifier"               parseMemberModifier               :: Int -> IO NodePtr
foreign import ccall "ParseMux"                          parseMux                          :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseNamedType"                    parseNamedType                    :: NodePtr -> ScopePtr -> IO NodePtr
foreign import ccall "ParseNamedVariable"                parseNamedVariable                :: NodePtr -> IO NodePtr
foreign import ccall "ParseNamespace"                    parseNamespace                    :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseNestedScope"                  parseNestedScope                  :: NodePtr -> IO NodePtr
foreign import ccall "ParseQualifiedName"                parseQualifiedName                :: QualifiedNamePtr -> IO NodePtr
foreign import ccall "ParseRangeFor"                     parseRangeFor                     :: NodePtr -> NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseReference"                    parseReference                    :: NodePtr -> IO NodePtr
foreign import ccall "ParseReorder"                      parseReorder                      :: NodePtr -> IO NodePtr
foreign import ccall "ParseReturn"                       parseReturn                       :: IO NodePtr
foreign import ccall "ParseReturnExpression"             parseReturnExpr                   :: NodePtr -> IO NodePtr
foreign import ccall "ParseScopedIdentifier"             parseScopedIdentifier             :: NodePtr -> IO NodePtr
foreign import ccall "ParseSizeOf"                       parseSizeOf                       :: Int -> NodePtr -> IO NodePtr
foreign import ccall "ParseStatic"                       parseStatic                       :: NodePtr -> IO NodePtr
foreign import ccall "ParseStaticAssert"                 parseStaticAssert                 :: NodePtr -> IO NodePtr
foreign import ccall "ParseStringLiteral"                parseStringLiteral                :: CString -> IO NodePtr
foreign import ccall "ParseStringType"                   parseStringType                   :: IO NodePtr
foreign import ccall "ParseStruct"                       parseStruct                       :: NodePtr -> NodePtr -> ScopePtr -> CString -> IO NodePtr
foreign import ccall "ParseSwitch"                       parseSwitch                       :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseSwitchBlock"                  parseSwitchBlock                  :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseTemplateArg"                  parseTemplateArg                  :: CString -> NodePtr -> IO NodePtr
foreign import ccall "ParseTemplateInstance"             parseTemplateInstance             :: NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseThis"                         parseThis                         :: ScopePtr -> IO NodePtr
foreign import ccall "ParseTypedef"                      parseTypedef                      :: NodePtr -> NodePtr -> ScopePtr -> CString -> IO NodePtr
foreign import ccall "ParseUintType"                     parseUintType                     :: Int -> IO NodePtr
foreign import ccall "ParseUnaryOp"                      parseUnaryOp                      :: Int -> NodePtr -> IO NodePtr
foreign import ccall "ParseUnion"                        parseUnion                        :: NodePtr -> NodePtr -> ScopePtr -> CString -> IO NodePtr
foreign import ccall "ParseUnrolledFor"                  parseUnrolledFor                  :: NodePtr -> NodePtr -> NodePtr -> IO NodePtr
foreign import ccall "ParseVoidType"                     parseVoid                         :: IO NodePtr
