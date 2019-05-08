{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- |
-- Bundles compiled PureScript modules for the browser.
--
-- This module takes as input the individual generated modules from 'Language.PureScript.Make' and
-- performs dead code elimination, filters empty modules,
-- and generates the final JavaScript bundle.
--
-- Based on `Language.PureScript.Bundle` (original source: <https://github.com/purescript/purescript/blob/ba37ff258a5fe7b10c946ecf619bd7137b832cbd/src/Language/PureScript/Bundle.hs>)
-- under the following license:
--
-- ```
-- Copyright (c) 2013-17 Phil Freeman, (c) 2014-2017 Gary Burgess, and other
-- contributors
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- ```
module Bundle where

import Prelude.Compat
import Protolude (ordNub, for, exitFailure, encodeUtf8)

import Data.Word
import Data.List (intersperse)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Arrow ((&&&))
import Control.Monad.IO.Class
import System.Directory
import System.Environment
import Control.Exception (evaluate)
import System.IO
import Control.DeepSeq
import GHC.Generics (Generic)

import Data.Array ((!))
import Data.Char (chr, digitToInt)
import Data.Foldable (fold)
import Data.Generics (GenericM, everything, everywhere, gmapMo, mkMp, mkQ, mkT)
import Data.Graph
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST

import qualified Language.JavaScript.Parser as JS
import Data.Functor.Contravariant

import qualified Control.Concurrent.ParallelIO.Local as PIO

import System.FilePath (takeFileName, takeDirectory, takeDirectory, makeRelative)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           System.Clock
import           Text.Printf

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder

import Data.Store (Store)
import qualified Data.Store as Store

import Crypto.Hash as H

import Control.Concurrent (getNumCapabilities)

type SourceHash = Text

hexSha1 :: Text -> SourceHash
hexSha1 str = T.pack $ show (H.hash (T.encodeUtf8 str :: BS.ByteString) :: H.Digest H.SHA1)

-- | The type of error messages. We separate generation and rendering of errors using a data
-- type, in case we need to match on error types later.
data ErrorMessage
  = UnsupportedModulePath FilePath
  | InvalidTopLevel
  | UnableToParseModule String
  | UnsupportedExport
  | ErrorInModule ModuleIdentifier ErrorMessage
  | MissingEntryPoint ModuleName
  | MissingMainModule ModuleName
  deriving (Show)

-- | Modules are either "regular modules" (i.e. those generated by the PureScript compiler) or
-- foreign modules.
data ModuleType
  = Regular
  | Foreign
  deriving (Show, Eq, Ord, Generic, Store)

showModuleType :: ModuleType -> String
showModuleType Regular = "Regular"
showModuleType Foreign = "Foreign"

type ModuleName = Text

-- | A module is identified by its module name and its type.
data ModuleIdentifier = ModuleIdentifier ModuleName ModuleType deriving (Show, Eq, Ord, Generic, Store)

moduleName :: ModuleIdentifier -> Text
moduleName (ModuleIdentifier name _) = name

-- | Given a filename, assuming it is in the correct place on disk, infer a ModuleIdentifier.
guessModuleIdentifier :: MonadError ErrorMessage m => FilePath -> m ModuleIdentifier
guessModuleIdentifier filename = ModuleIdentifier (T.pack (takeFileName (takeDirectory filename))) <$> guessModuleType (takeFileName filename)
  where
    guessModuleType "index.js" = pure Regular
    guessModuleType "foreign.js" = pure Foreign
    guessModuleType name = throwError $ UnsupportedModulePath name

-- | A piece of code is identified by its module and its name. These keys are used to label vertices
-- in the dependency graph.
type Key = (ModuleIdentifier, Text)

-- | An export is either a "regular export", which exports a name from the regular module we are in,
-- or a reexport of a declaration in the corresponding foreign module.
--
-- Regular exports are labelled, since they might re-export an operator with another name.
data ExportType
  = RegularExport Text
  | ForeignReexport
  deriving (Show, Eq, Ord, Generic, Store)

data Phase = Raw | Parsed

data Code phase a where
  CodeRaw :: ByteString -> Code Raw a
  CodeParsed :: a -> Code Parsed a

instance Show a => Show (Code phase a) where
  show (CodeRaw x) = show x
  show (CodeParsed x) = show x

instance Store (Code Raw a) where
  size = contramap (\(CodeRaw x) -> x) Store.size
  peek = CodeRaw <$> Store.peek
  poke (CodeRaw x) = Store.poke x

unwrapParsed :: Code Parsed a -> a
unwrapParsed (CodeParsed x) = x

-- | There are four types of module element we are interested in:
--
-- 1) Require statements
-- 2) Member declarations
-- 3) Export lists
-- 4) Everything else
--
-- Each is labelled with the original AST node which generated it, so that we can dump it back
-- into the output during codegen.
data ModuleElement p
  = Require (Code p JSStatement) Text (Either Text ModuleIdentifier)
  | Member (Code p JSStatement) Bool Text (Code p JSExpression) [Key]
  | ExportsList [(ExportType, Text, Code p JSExpression, [Key])]
  | Other (Code p JSStatement)
  | Skip
  deriving (Show, Generic)

deriving instance Store (ModuleElement Raw)

-- | A module is just a list of elements of the types listed above.
data Module p = Module { module_id :: ModuleIdentifier , module_filename :: Maybe FilePath, module_sourceHash :: SourceHash, module_contents :: [ModuleElement p] }
  deriving (Show, Generic)

deriving instance Store (Module Raw)

-- | Prepare an error message for consumption by humans.
printErrorMessage :: ErrorMessage -> [String]
printErrorMessage (UnsupportedModulePath s) =
  [ "A CommonJS module has an unsupported name (" ++ show s ++ ")."
  , "The following file names are supported:"
  , "  1) index.js (PureScript native modules)"
  , "  2) foreign.js (PureScript foreign modules)"
  ]
printErrorMessage InvalidTopLevel =
  [ "Expected a list of source elements at the top level." ]
printErrorMessage (UnableToParseModule err) =
  [ "The module could not be parsed:"
  , err
  ]
printErrorMessage UnsupportedExport =
  [ "An export was unsupported. Exports can be defined in one of two ways: "
  , "  1) exports.name = ..."
  , "  2) exports = { ... }"
  ]
printErrorMessage (ErrorInModule mid e) =
  ("Error in module " ++ displayIdentifier mid ++ ":")
  : ""
  : map ("  " ++) (printErrorMessage e)
  where
    displayIdentifier (ModuleIdentifier name ty) =
      T.unpack name ++ " (" ++ showModuleType ty ++ ")"
printErrorMessage (MissingEntryPoint mName) =
  [ "Couldn't find a CommonJS module for the specified entry point: " ++ T.unpack mName
  ]
printErrorMessage (MissingMainModule mName) =
  [ "Couldn't find a CommonJS module for the specified main module: " ++ T.unpack mName
  ]

-- | Calculate the ModuleIdentifier which a require(...) statement imports.
checkImportPath :: Text -> ModuleIdentifier -> S.Set ModuleName -> Either Text ModuleIdentifier
checkImportPath "./foreign.js" m _ =
  Right (ModuleIdentifier (moduleName m) Foreign)
checkImportPath name _ names
  | Just name' <- T.stripSuffix "/index.js" =<< T.stripPrefix "../" name
  , name' `S.member` names = Right (ModuleIdentifier name' Regular)
checkImportPath name _ _ = Left name

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix xs =
  case splitAt (length xs - length suffix) xs of
    (before, after)
      | after == suffix -> Just before
      | otherwise -> Nothing

-- | Compute the dependencies of all elements in a module, and add them to the tree.
--
-- Members and exports can have dependencies. A dependency is of one of the following forms:
--
-- 1) module.name or member["name"]
--
--    where module was imported using
--
--    var module = require("Module.Name");
--
-- 2) name
--
--    where name is the name of a member defined in the current module.
withDeps :: Module Parsed -> Module Parsed
withDeps mod@Module{module_id=modulePath, module_contents=es} = mod { module_contents = map expandDeps es }
  where
  -- | Collects all modules which are imported, so that we can identify dependencies of the first type.
  imports :: [(Text, ModuleIdentifier)]
  imports = mapMaybe toImport es
    where
    toImport :: ModuleElement Parsed -> Maybe (Text, ModuleIdentifier)
    toImport (Require _ nm (Right mid)) = Just (nm, mid)
    toImport _ = Nothing

  -- | Collects all member names in scope, so that we can identify dependencies of the second type.
  boundNames :: [Text]
  boundNames = mapMaybe toBoundName es
    where
    toBoundName :: ModuleElement Parsed -> Maybe Text
    toBoundName (Member _ _ nm _ _) = Just nm
    toBoundName _ = Nothing

  -- | Calculate dependencies and add them to the current element.
  expandDeps :: ModuleElement Parsed -> ModuleElement Parsed
  expandDeps (Member n f nm decl _) = Member n f nm decl (S.toList $ dependencies modulePath $ unwrapParsed decl)
  expandDeps (ExportsList exps) = ExportsList (map expand exps)
      where
      expand (ty, nm, n1, _) = (ty, nm, n1, S.toList (dependencies modulePath $ unwrapParsed n1))
  expandDeps other = other

  dependencies :: ModuleIdentifier -> JSExpression -> S.Set (ModuleIdentifier, Text)
  dependencies m = everything (<>) (mkQ mempty toReference)
    where
    toReference :: JSExpression -> S.Set (ModuleIdentifier, Text)
    toReference (JSMemberDot mn _ nm)
      | JSIdentifier _ (T.pack -> mn') <- mn
      , JSIdentifier _ (T.pack -> nm') <- nm
      , Just mid <- lookup mn' imports
      = S.singleton (mid, nm')
    toReference (JSMemberSquare mn _ nm _)
      | JSIdentifier _ (T.pack -> mn') <- mn
      , Just nm' <- fromStringLiteral nm
      , Just mid <- lookup mn' imports
      = S.singleton (mid, nm')
    toReference (JSIdentifier _ (T.pack -> nm))
      | nm `elem` boundNames
      = S.singleton (m, nm)
    toReference _ = mempty

-- String literals include the quote chars
fromStringLiteral :: JSExpression -> Maybe Text
fromStringLiteral (JSStringLiteral _ str) = Just $ T.pack $ strValue str
fromStringLiteral _ = Nothing

strValue :: String -> String
strValue str = go $ drop 1 str
  where
  go ('\\' : 'b' : xs) = '\b' : go xs
  go ('\\' : 'f' : xs) = '\f' : go xs
  go ('\\' : 'n' : xs) = '\n' : go xs
  go ('\\' : 'r' : xs) = '\r' : go xs
  go ('\\' : 't' : xs) = '\t' : go xs
  go ('\\' : 'v' : xs) = '\v' : go xs
  go ('\\' : '0' : xs) = '\0' : go xs
  go ('\\' : 'x' : a : b : xs) = chr (a' + b') : go xs
    where
    a' = 16 * digitToInt a
    b' = digitToInt b
  go ('\\' : 'u' : a : b : c : d : xs) = chr (a' + b' + c' + d') : go xs
    where
    a' = 16 * 16 * 16 * digitToInt a
    b' = 16 * 16 * digitToInt b
    c' = 16 * digitToInt c
    d' = digitToInt d
  go ('\\' : x : xs) = x : go xs
  go "\"" = ""
  go "'" = ""
  go (x : xs) = x : go xs
  go "" = ""

commaList :: JSCommaList a -> [a]
commaList JSLNil = []
commaList (JSLOne x) = [x]
commaList (JSLCons l _ x) = commaList l ++ [x]

trailingCommaList :: JSCommaTrailingList a -> [a]
trailingCommaList (JSCTLComma l _) = commaList l
trailingCommaList (JSCTLNone l) = commaList l

-- | Attempt to create a Module from a JavaScript AST.
--
-- Each type of module element is matched using pattern guards, and everything else is bundled into the
-- Other constructor.
toModule :: forall m. (MonadError ErrorMessage m) => S.Set ModuleName -> ModuleIdentifier -> Maybe FilePath -> SourceHash -> JSAST -> m (Module Parsed)
toModule mids mid filename sourceHash top
  | JSAstProgram smts _ <- top = Module mid filename sourceHash <$> traverse toModuleElement smts
  | otherwise = err InvalidTopLevel
  where
  err :: forall a. ErrorMessage -> m a
  err = throwError . ErrorInModule mid

  toModuleElement :: JSStatement -> m (ModuleElement Parsed)
  toModuleElement stmt
    | Just (importName, importPath) <- matchRequire mids mid stmt
    = pure (Require (CodeParsed stmt) importName importPath)
  toModuleElement stmt
    | Just (exported, name, decl) <- matchMember stmt
    = pure (Member (CodeParsed stmt) exported name (CodeParsed decl) [])
  toModuleElement stmt
    | Just props <- matchExportsAssignment stmt
    = ExportsList <$> traverse toExport (trailingCommaList props)
    where
      toExport :: JSObjectProperty -> m (ExportType, Text, Code Parsed JSExpression, [Key])
      toExport (JSPropertyNameandValue name _ [val]) =
        (,,CodeParsed val,[]) <$> exportType val
                   <*> extractLabel' name
      toExport _ = err UnsupportedExport

      exportType :: JSExpression -> m ExportType
      exportType (JSMemberDot f _ _)
        | JSIdentifier _ "$foreign" <- f
        = pure ForeignReexport
      exportType (JSMemberSquare f _ _ _)
        | JSIdentifier _ "$foreign" <- f
        = pure ForeignReexport
      exportType (JSIdentifier _ s) = pure (RegularExport (T.pack s))
      exportType _ = err UnsupportedExport

      extractLabel' = maybe (err UnsupportedExport) pure . extractLabel

  toModuleElement other = pure (Other (CodeParsed other))

-- Matches JS statements like this:
-- var ModuleName = require("file");
matchRequire :: S.Set ModuleName
                -> ModuleIdentifier
                -> JSStatement
                -> Maybe (Text, Either Text ModuleIdentifier)
matchRequire mids mid stmt
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ importName <- var
  , JSVarInit _ jsInitEx <- varInit
  , JSMemberExpression req _ argsE _ <- jsInitEx
  , JSIdentifier _ "require" <- req
  , [ Just importPath ] <- map fromStringLiteral (commaList argsE)
  , importPath' <- checkImportPath importPath mid mids
  = Just (T.pack importName, importPath')
  | otherwise
  = Nothing

-- Matches JS member declarations.
matchMember :: JSStatement -> Maybe (Bool, Text, JSExpression)
matchMember stmt
  -- var foo = expr;
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ name <- var
  , JSVarInit _ decl <- varInit
  = Just (False, T.pack name, decl)
  -- exports.foo = expr; exports["foo"] = expr;
  | JSAssignStatement e (JSAssign _) decl _ <- stmt
  , Just name <- accessor e
  = Just (True, name, decl)
  | otherwise
  = Nothing
  where
  accessor :: JSExpression -> Maybe Text
  accessor (JSMemberDot exports _ nm)
    | JSIdentifier _ "exports" <- exports
    , JSIdentifier _ name <- nm
    = Just (T.pack name)
  accessor (JSMemberSquare exports _ nm _)
    | JSIdentifier _ "exports" <- exports
    , Just name <- fromStringLiteral nm
    = Just name
  accessor _ = Nothing

-- Matches assignments to module.exports, like this:
-- module.exports = { ... }
matchExportsAssignment :: JSStatement -> Maybe JSObjectPropertyList
matchExportsAssignment stmt
  | JSAssignStatement e (JSAssign _) decl _ <- stmt
  , JSMemberDot module' _ exports <- e
  , JSIdentifier _ "module" <- module'
  , JSIdentifier _ "exports" <- exports
  , JSObjectLiteral _ props _ <- decl
  = Just props
  | otherwise
  = Nothing

extractLabel :: JSPropertyName -> Maybe Text
extractLabel (JSPropertyString _ nm) = Just $ T.pack $ strValue nm
extractLabel (JSPropertyIdent _ nm) = Just $ T.pack nm
extractLabel _ = Nothing

-- | Eliminate unused code based on the specified entry point set.
compile :: forall p. [Module p] -> [ModuleIdentifier] -> [Module p]
compile modules [] = modules
compile modules entryPoints = filteredModules
  where
  (graph, vertexToNode, vertexFor) = graphFromEdges verts

  -- | The vertex set
  verts :: [(ModuleElement p, Key, [Key])]
  verts = do
    Module{module_id=mid, module_contents=els} <- modules
    concatMap (toVertices mid) els
    where
    -- | Create a set of vertices for a module element.
    --
    -- Some special cases worth commenting on:
    --
    -- 1) Regular exports which simply export their own name do not count as dependencies.
    --    Regular exports which rename and reexport an operator do count, however.
    --
    -- 2) Require statements don't contribute towards dependencies, since they effectively get
    --    inlined wherever they are used inside other module elements.
    toVertices :: ModuleIdentifier -> ModuleElement p -> [(ModuleElement p, Key, [Key])]
    toVertices p m@(Member _ _ nm _ deps) = [(m, (p, nm), deps)]
    toVertices p m@(ExportsList exps) = mapMaybe toVertex exps
      where
      toVertex (ForeignReexport, nm, _, ks) = Just (m, (p, nm), ks)
      toVertex (RegularExport nm, nm1, _, ks) | nm /= nm1 = Just (m, (p, nm1), ks)
      toVertex _ = Nothing
    toVertices _ _ = []

  -- | The set of vertices whose connected components we are interested in keeping.
  entryPointVertices :: [Vertex]
  entryPointVertices = catMaybes $ do
    (_, k@(mid, _), _) <- verts
    guard $ mid `elem` entryPoints
    return (vertexFor k)

  -- | The set of vertices reachable from an entry point
  reachableSet :: S.Set Vertex
  reachableSet = S.fromList (concatMap (reachable graph) entryPointVertices)

  -- | A map from modules to the modules that are used by its reachable members.
  moduleReferenceMap :: M.Map ModuleIdentifier (S.Set ModuleIdentifier)
  moduleReferenceMap = M.fromAscListWith mappend $ map (vertToModule &&& vertToModuleRefs) $ S.toList reachableSet
    where
    vertToModuleRefs v = foldMap (S.singleton . vertToModule) $ graph ! v
    vertToModule v = m where (_, (m, _), _) = vertexToNode v

  filteredModules :: [Module p]
  filteredModules = map filterUsed modules
    where
    filterUsed :: Module p -> Module p
    filterUsed mod@Module{module_id=mid, module_contents=ds} = mod { module_contents = map filterExports (go ds) }
      where
      go :: [ModuleElement p] -> [ModuleElement p]
      go [] = []
      go (d : rest)
        | not (isDeclUsed d) = skipDecl d : go rest
        | otherwise = d : go rest

      skipDecl :: ModuleElement p -> ModuleElement p
      skipDecl (Require s _ _) = Skip
      skipDecl (Member s _ _ _ _) = Skip
      skipDecl (ExportsList _) = Skip
      skipDecl (Other s) = Skip
      skipDecl Skip = Skip

      -- | Filter out the exports for members which aren't used.
      filterExports :: ModuleElement p -> ModuleElement p
      filterExports (ExportsList exps) = ExportsList (filter (\(_, nm, _, _) -> isKeyUsed (mid, nm)) exps)
      filterExports me = me

      isDeclUsed :: ModuleElement p -> Bool
      isDeclUsed (Member _ _ nm _ _) = isKeyUsed (mid, nm)
      isDeclUsed (Require _ _ (Right midRef)) = midRef `S.member` modulesReferenced
      isDeclUsed _ = True

      isKeyUsed :: Key -> Bool
      isKeyUsed k
        | Just me <- vertexFor k = me `S.member` reachableSet
        | otherwise = False

      modulesReferenced :: S.Set ModuleIdentifier
      modulesReferenced = fold $ M.lookup mid moduleReferenceMap

-- | Topologically sort the module dependency graph, so that when we generate code, modules can be
-- defined in the right order.
sortModules :: [Module p] -> [Module p]
sortModules modules = map (\v -> case nodeFor v of (n, _, _) -> n) (reverse (topSort graph))
  where
  (graph, nodeFor, _) = graphFromEdges $ do
    m <- modules
    return (m, module_id m, mapMaybe getKey (module_contents m))

  getKey :: ModuleElement p -> Maybe ModuleIdentifier
  getKey (Require _ _ (Right mi)) = Just mi
  getKey _ = Nothing

-- | A module is empty if it contains no exported members (in other words,
-- if the only things left after dead code elimination are module imports and
-- "other" foreign code).
--
-- If a module is empty, we don't want to generate code for it.
isModuleEmpty :: Module p -> Bool
isModuleEmpty Module{module_contents=els} = all isElementEmpty els
  where
  isElementEmpty :: ModuleElement p -> Bool
  isElementEmpty (ExportsList exps) = null exps
  isElementEmpty Require{} = True
  isElementEmpty Other{} = True
  isElementEmpty Skip{} = True
  isElementEmpty _ = False

ppStatement' :: JS.JSStatement -> ByteString
ppStatement' stmt = (<> "\n") $ BL.dropWhile isSpace $ Builder.toLazyByteString $ JS.renderJS $ JS.JSAstProgram [stmt] JS.JSNoAnnot

ppStatement :: Code Parsed JS.JSStatement -> Code Raw JS.JSStatement
ppStatement (CodeParsed stmt) = CodeRaw $ ppStatement' stmt

ppExpression' :: JS.JSExpression -> ByteString
ppExpression' expr = BL.dropWhile isSpace $ Builder.toLazyByteString $ JS.renderJS $ JS.JSAstExpression expr JS.JSNoAnnot

ppExpression :: Code Parsed JS.JSExpression -> Code Raw JS.JSExpression
ppExpression (CodeParsed expr) = CodeRaw $ ppExpression' expr

isSpace :: Word8 -> Bool
isSpace x = x == 10 || x == 32

renderModuleElement :: ModuleElement Parsed -> ModuleElement Raw
renderModuleElement = \case
  Require stmt nm req -> Require (ppStatement stmt) nm req
  Member stmt f nm decl deps -> Member (ppStatement stmt) f nm (ppExpression decl) deps
  ExportsList exps -> ExportsList (map (\(type_, s, expr, deps) -> (type_, s, ppExpression expr, deps)) exps)
  Other stmt -> Other (ppStatement stmt)
  Skip -> Skip

renderModule :: Module Parsed -> Module Raw
renderModule mod@Module{module_contents=decls} = mod { module_contents = map renderModuleElement decls }

codeGen :: Maybe ModuleName -- ^ main module
        -> String -- ^ namespace
        -> [Module Raw] -- ^ input modules
        -> ByteString
codeGen optionsMainModule optionsNamespace ms =
  ppStatement' prelude
  <> mconcat modulesJS
  <> maybe mempty (ppStatement' . runMain) optionsMainModule
  
  where

  modulesJS = map moduleToJS ms

  moduleToJS :: Module Raw -> ByteString
  moduleToJS mod@Module{module_id=mid,module_contents=ds} = wrap mid $ mconcat jsDecls
    where
    jsDecls = map declToJS ds

    declToJS :: ModuleElement Raw -> ByteString
    declToJS (Member (CodeRaw n) _ _ _ _) = n
    declToJS (Other (CodeRaw n)) = n
    declToJS Skip = ""
    declToJS (Require _ nm req) =
      ppStatement' $
        JSVariable lfsp
          (cList [
            JSVarInitExpression (JSIdentifier sp (T.unpack nm))
              (JSVarInit sp $ either require (innerModuleReference sp . moduleName) req )
          ]) (JSSemi JSNoAnnot)
    declToJS (ExportsList exps) = foldMap toExport exps

      where

      toExport :: (ExportType, Text, Code Raw JSExpression, [Key]) -> ByteString
      toExport (_, nm, CodeRaw val, _) =
        ppExpression' (JSMemberSquare (JSIdentifier lfsp "exports") JSNoAnnot (str nm) JSNoAnnot) <>
        " = " <>
        val <>
        ";\n"

  -- comma lists are reverse-consed
  cList :: [a] -> JSCommaList a
  cList [] = JSLNil
  cList [x] = JSLOne x
  cList l = go $ reverse l
    where
      go [x] = JSLOne x
      go (h:t)= JSLCons (go t) JSNoAnnot h
      go [] = error "Invalid case in comma-list"

  prelude :: JSStatement
  prelude = JSVariable (JSAnnot tokenPosnEmpty [ CommentA tokenPosnEmpty $ "// Generated by purs-fast-bundle"
                                               , WhiteSpace tokenPosnEmpty "\n" ])
              (cList [
                JSVarInitExpression (JSIdentifier sp optionsNamespace)
                  (JSVarInit sp (emptyObj sp))
              ]) (JSSemi JSNoAnnot)

  require :: Text -> JSExpression
  require mn =
    JSMemberExpression (JSIdentifier JSNoAnnot "require") JSNoAnnot (cList [ str mn ]) JSNoAnnot

  moduleReference :: JSAnnot -> ModuleName -> JSExpression
  moduleReference a mn =
    JSMemberSquare (JSIdentifier a optionsNamespace) JSNoAnnot
      (str mn) JSNoAnnot

  innerModuleReference :: JSAnnot -> ModuleName -> JSExpression
  innerModuleReference a mn =
    JSMemberSquare (JSIdentifier a "$PS") JSNoAnnot
      (str mn) JSNoAnnot

  str :: Text -> JSExpression
  str s = JSStringLiteral JSNoAnnot $ "\"" ++ T.unpack s ++ "\""

  emptyObj :: JSAnnot -> JSExpression
  emptyObj a = JSObjectLiteral a (JSCTLNone JSLNil) JSNoAnnot

  initializeObject :: JSAnnot -> (JSAnnot -> Text -> JSExpression) -> Text -> JSExpression
  initializeObject a makeReference mn =
    JSAssignExpression (makeReference a mn) (JSAssign sp)
    $ JSExpressionBinary (makeReference sp mn) (JSBinOpOr sp)
    $ emptyObj sp

  -- Like `somewhere`, but stops after the first successful transformation
  firstwhere :: MonadPlus m => GenericM m -> GenericM m
  firstwhere f x = f x `mplus` gmapMo (firstwhere f) x

  iife
    :: ByteString -- ^ contents
    -> ByteString -- ^ argument name
    -> JSExpression -- ^ argument value
    -> ByteString
  iife body param arg =
    "(function(" <> param <> ") {\n" <>
    body <>
    "})(" <> ppExpression' arg <> ");\n"

  wrap :: ModuleIdentifier -> ByteString -> ByteString
  wrap (ModuleIdentifier mn mtype) ds =
    case mtype of
      Regular -> iife (moduleExports <> ds) "$PS" (JSIdentifier JSNoAnnot optionsNamespace)
      Foreign -> iife ds "exports" (initializeObject JSNoAnnot moduleReference mn)

    where
    -- FIXME: module exports should be after "use strict"
    moduleExports :: ByteString
    moduleExports =
      ppStatement' (JSExpressionStatement (initializeObject lfsp innerModuleReference mn) (JSSemi JSNoAnnot)) <>
      ppStatement' (JSVariable lfsp (JSLOne $ JSVarInitExpression (JSIdentifier sp "exports") $ JSVarInit sp (innerModuleReference sp mn)) (JSSemi JSNoAnnot))

  runMain :: ModuleName -> JSStatement
  runMain mn =
    JSMethodCall
      (JSMemberDot (moduleReference lf mn) JSNoAnnot
        (JSIdentifier JSNoAnnot "main"))
      JSNoAnnot (cList []) JSNoAnnot (JSSemi JSNoAnnot)

  lf :: JSAnnot
  lf = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n" ]

  lfsp :: JSAnnot
  lfsp = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n  " ]

  sp :: JSAnnot
  sp = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty " " ]

type EIO = ExceptT ErrorMessage IO

-- | The bundling function.
-- This function performs dead code elimination, filters empty modules
-- and generates and prints the final JavaScript bundle.
bundle :: [(ModuleIdentifier, FilePath)] -- ^ The input modules.  Each module should be javascript rendered from the compiler.
       -> [ModuleIdentifier] -- ^ Entry points.  These module identifiers are used as the roots for dead-code elimination
       -> Maybe ModuleName -- ^ An optional main module.
       -> String -- ^ The namespace (e.g. PS).
       -> EIO ByteString
bundle inputFiles entryPoints mainModule namespace = do
  let mids = S.fromList (map (moduleName . fst) inputFiles)

  forM_ mainModule $ \mname ->
    when (mname `notElem` map (moduleName . fst) inputFiles) (throwError (MissingMainModule mname))
  forM_ entryPoints $ \mIdent ->
    when (mIdent `notElem` map fst inputFiles) (throwError (MissingEntryPoint (moduleName mIdent)))

  modules <- logPerf "load modules" $ parallelEIO $ loadModule mids <$> inputFiles

  let compiled = compile modules entryPoints
  logPerf "compile" $ liftIO $ evaluate $ length compiled
  sorted <- logPerf "sortModules" $ liftIO $ evaluate $ sortModules (filter (not . isModuleEmpty) compiled)

  logPerf "codegen" $ do
    let code = codeGen mainModule namespace sorted
    liftIO $ evaluate $ BL.length code
    pure code

parallelEIO :: [EIO a] -> EIO [a]
parallelEIO actions = do
  n <- liftIO getNumCapabilities
  results <- liftIO $ PIO.withPool n $ \pool ->
    PIO.parallel pool (((>>= evaluate) . runExceptT) <$> actions)
  ExceptT $ pure $ sequence results

loadModule :: S.Set ModuleName -> (ModuleIdentifier, FilePath) -> EIO (Module Raw)
loadModule mids (ident@(ModuleIdentifier modName moduleType), filename) = do
  exists <- liftIO $ doesFileExist cacheFilename
  js <- liftIO $ T.readFile filename
  let sourceHash = hexSha1 js
  if exists then
    liftIO (Store.decode <$> BS.readFile cacheFilename) >>= \case
      Right mod | module_sourceHash mod == sourceHash ->
        pure mod
      _ ->
        parseModuleAndWriteCache js sourceHash
  else
    parseModuleAndWriteCache js sourceHash

  where
  cacheFilename = ".purs-fast-bundle-cache/" <> T.unpack modName <> (if moduleType == Regular then "" else "_foreign")

  parseModuleAndWriteCache js sourceHash = do
    liftIO $ hPutStrLn stderr $ "Parsing " <> filename
    ast <- either (throwError . ErrorInModule ident . UnableToParseModule) pure $ parse (T.unpack js) (T.unpack (moduleName ident))
    module_ <- toModule mids ident (Just filename) sourceHash ast
    let rawModule = renderModule $ withDeps module_
    liftIO $ createDirectoryIfMissing True $ takeDirectory cacheFilename
    liftIO $ BS.writeFile cacheFilename $ Store.encode rawModule
    pure rawModule

logPerf :: MonadIO m => Text -> m t -> m t
logPerf label f = do
  start <- liftIO (getTime Monotonic)
  result <- f
  end <- liftIO (getTime Monotonic)
  liftIO $ T.hPutStrLn stderr (labelTimespec label (diffTimeSpec start end))
  pure result

  where

    labelTimespec :: Text -> TimeSpec -> Text
    labelTimespec label duration = label <> ": " <> displayTimeSpec duration

    displayTimeSpec :: TimeSpec -> Text
    displayTimeSpec ts =
      T.pack (printf "%0.2f" (fromIntegral (toNanoSecs ts) / 1000000 :: Double)) <> "ms"
