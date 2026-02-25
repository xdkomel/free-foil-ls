{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE StandaloneDeriving                 #-}

module AgnosticFoilLSAlt.LambdaPiLSAlt
  ( runLambdaPiLSAlt ) where

import qualified Lampi.AbsLampi as Raw
import qualified Lampi.LayoutLampi as Raw
import qualified Lampi.LexLampi as Raw
import qualified Lampi.ParLampi as Raw
import Control.Monad.Foil.Internal
import Control.Monad.Foil.Relative
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil
import qualified Data.Map as Map
import Control.Arrow 
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server 
import Language.LSP.Protocol.Lens ( textDocument, uri, params, newName, position )
import Control.Monad.Reader
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes, isJust)
import Data.Bifunctor.Sum
import Unsafe.Coerce (unsafeCoerce)

import Common.LambdaPi
import Common.LanguageServerCache

import AgnosticFoilLSAlt.AgnosticFoilLSAlt

type ASTAnn = Raw.BNFC'Position

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

maybeToList :: (a -> b) -> Maybe a -> [b]
maybeToList f = maybe [] (\a -> [f a])

buildCache 
  :: String 
  -> (String -> [AST binder sig n]) 
  -> LSP (AST binder sig n) ()
buildCache extension toAsts = do
  root <- getRootPath
  case root of
    Nothing ->
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Warning 
        $ T.pack "Cannot find the workspace root"
    Just rootPath -> do
      rawPaths <- liftIO $ globDir [compile ("*." ++ extension)] rootPath
      let paths = concat rawPaths
      asts <- liftIO $ mapM filePathToAst paths
      let 
        -- asts = concatMap unpackFilePathAstPair maybeAsts
          cache = Map.fromList $ map (second astToCache) asts
          -- cacheStr = show $ Map.map (printNode . SomeTerm . langAst) cache
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Info 
        $ T.pack 
        $ "Language server for '." 
          ++ extension 
          ++ "' is initialized"
      cacheStore $ LangStore cache
  where
    filePathToAst f = do
      input <- liftIO $ readFile f
      return (f, toAsts input)
    astToCache n = LangProgramStore { langAst = n }

buildAsts :: String -> [LambdaPi ASTAnn VoidS]
buildAsts = 
  maybe [] terms
  . toAst
  . Raw.resolveLayout True 
  . Raw.tokens
  where
    toAst :: [Raw.Token] -> Maybe Raw.Program
    toAst ts = either (\_ -> Nothing) Just (Raw.pProgram ts)
    terms :: Raw.Program -> [LambdaPi ASTAnn VoidS]
    terms = \case
      Raw.AProgram _ ts -> concatMap toLambdaPiCommand ts
    toLambdaPiCommand :: Raw.Command -> [LambdaPi ASTAnn VoidS]
    toLambdaPiCommand = \case
      Raw.CommandCheck _ l r -> map (toLambdaPi scope env) [l, r]
      Raw.CommandCompute _ l r -> map (toLambdaPi scope env) [l, r]
      where
        scope = emptyScope
        env = Map.empty

-- findName :: Distinct n => (Int, Int) -> LambdaPi ASTAnn n -> Maybe (SomeName NameBinder (LambdaPiSig ASTAnn))
-- findName pos@(x, y) = \case
  -- (AVar varPos (Var name) nameStr) -> 
  --   varPos >>= nameInterval name nameStr
--   (App _ fun arg) -> 
--     firstJust $ map (findName pos) [fun, arg]
--   (Lam _ binder _ body) -> case assertDistinct binder of
--     Distinct -> findName pos body
--   (Pi _ binder _ pat body) -> case assertDistinct binder of
--     Distinct -> firstJust [findName pos pat, findName pos body]
--   (Pair _ l r) -> 
--     firstJust $ map (findName pos) [l, r]
--   (First _ a) -> findName pos a
--   (Second _ a) -> findName pos a
--   (Product _ l r) -> 
--     firstJust $ map (findName pos) [l, r]
--   _ -> Nothing
--   where
    -- nameInterval name nameStr (line, col) = 
    --   if x == line && y >= col && y < col + length nameStr
    --     then Just $ SomeName name
    --     else Nothing

extractName :: SomeTerm NameBinder (LambdaPiSig ASTAnn) -> Maybe (SomeName NameBinder (LambdaPiSig ASTAnn))
extractName = \case
  (SomeTerm (AVar _ (Var name) _)) -> Just $ SomeName name
  _ -> Nothing
  -- where
  --   nameInterval name nameStr (line, col) = 
  --     if x == line && y >= col && y < col + length nameStr
  --       then Just $ SomeName name
  --       else Nothing

findSomeNarrowest :: (Int, Int) -> SomeTerm NameBinder (LambdaPiSig ASTAnn) -> Maybe (SomeTerm NameBinder (LambdaPiSig ASTAnn))
findSomeNarrowest pos (SomeTerm n) = findNarrowest pos n

findNarrowest :: Distinct n => (Int, Int) -> LambdaPi ASTAnn n -> Maybe (SomeTerm NameBinder (LambdaPiSig ASTAnn))
findNarrowest pos@(x, y) = \case
  t@(AVar varPos _ nameStr) -> varPos >>= ifMatches nameStr t
  (App _ fun arg) -> 
    firstJust $ map (findNarrowest pos) [fun, arg]
  (Lam _ binder _ body) -> case assertDistinct binder of 
    Distinct -> case assertExt binder of
      Ext -> findNarrowest pos body
  (Pi _ binder _ pat body) -> case assertDistinct binder of 
    Distinct -> case assertExt binder of
      Ext -> firstJust [findNarrowest pos pat, findNarrowest pos body]
  (Pair _ l r) -> 
    firstJust $ map (findNarrowest pos) [l, r]
  (First _ a) -> findNarrowest pos a
  (Second _ a) -> findNarrowest pos a
  (Product _ l r) -> 
    firstJust $ map (findNarrowest pos) [l, r]
  _ -> Nothing
  where
    ifMatches nameStr node (line, col) = 
      if x == line && y >= col && y < col + length nameStr
        then Just $ SomeTerm node
        else Nothing

buildTele :: Distinct n => LambdaPi a n -> [TermTelescope NameBinder (LambdaPiSig a) n]
buildTele t@AVar{} = [LeafTerm t]
buildTele (App _ fun arg) = concatMap buildTele [fun, arg]
buildTele t@(Lam _ binder _ body) =
  case (assertDistinct binder, assertExt binder) of
    (Distinct, Ext) -> [NodeTerm t binder $ buildTele body]
      -- ++ [LeafTerm t] 
buildTele t@(Pi _ binder _ fun body) = 
  case (assertDistinct binder, assertExt binder) of
    (Distinct, Ext) -> [NodeTerm t binder $ buildTele body] ++ buildTele fun 
    -- ++ buildTele fun
      -- ++ [LeafTerm t] 
      -- ++ buildTele fun
buildTele (Pair _ l r) = concatMap buildTele [l, r]
buildTele (First _ body) = buildTele body
buildTele (Second _ body) = buildTele body
buildTele (Product _ l r) = concatMap buildTele [l, r]
buildTele _ = []

astRange :: SomeTerm NameBinder (LambdaPiSig ASTAnn) -> Maybe Range
astRange (SomeTerm n) = case n of
  Lam (Just (x, y)) _ pat _ -> toRange x y $ patternLength pat
  Pi (Just (x, y)) _ pat _ _ -> toRange x y $ patternLength pat
  AVar (Just (x, y)) _ nameStr -> toRange x y $ length nameStr
  _ -> Nothing
  where
    patternLength = \case
      PatternWildcard _ -> 1
      PatternVar _ x -> length x
      _ -> 1
    toRange x y len = Just 
      $ Range (Position (pos x) (pos y)) (Position (pos x) (pos $ y + len))
    pos i = fromIntegral (i - 1)

handlers :: Handlers (LSP (AST NameBinder (LambdaPiSig ASTAnn) VoidS))
handlers =
  let cacheAsts = buildCache "lampi" buildAsts 
      defRange = definitionRange findSomeNarrowest extractName astRange buildTele
      -- symRange = symbolRange findNarrowest astRange
      usagesRanges = mentionedRanges findSomeNarrowest extractName astRange buildTele
      traceVar = traces 
      -- traceVar = traces findNarrowest extractName buildTele showSomeTerm
  in mconcat
  [ notificationHandler SMethod_Initialized $ const $ cacheAsts
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const $ cacheAsts
  , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
    LangStore cache <- getCachedStore
    let parameters = req ^. params
        bnfcPosition = toBnfcPosition $ parameters ^. position
        fileUri = parameters ^. textDocument . uri
        maybeCurrentFile = uriToFilePath fileUri
        asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        maybeRange = firstJust 
          $ map (defRange emptyScope bnfcPosition) asts
        usages = concatMap ((++ "\n\n") . traceVar StrType) asts
        --  concatMap ((++ "\n\n") . traceVar) asts
    sendNotification SMethod_WindowShowMessage 
      $ ShowMessageParams MessageType_Info 
      $ T.pack 
      $ "Range identified: " ++ show maybeRange
    sendNotification SMethod_WindowShowMessage 
      $ ShowMessageParams MessageType_Info 
      $ T.pack 
      $ "Usages: " ++ usages
    let maybeLocation = fmap (Location fileUri) maybeRange
    responder 
      $ maybeToEither (responseError "Did not find the definition") 
      $ fmap (InL . Definition . InL) maybeLocation
  , requestHandler SMethod_TextDocumentRename $ \req responder -> do
    let parameters = req ^. params
        bnfcPosition = toBnfcPosition $ parameters ^. position
        newSymName = parameters ^. newName
        fileUri = parameters ^. textDocument . uri
        maybeCurrentFile = uriToFilePath fileUri
    vdoc <- getVersionedTextDoc $ parameters ^. textDocument
    LangStore cache <- getCachedStore
    let asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        mentioned = concatMap (usagesRanges emptyScope bnfcPosition) asts
        -- maybeDefRange = firstJust $ map (defRange bnfcPosition) asts
        -- maybeNameRange = firstJust $ map (symRange bnfcPosition) asts
        toTextEdit range = InL $ TextEdit range newSymName
        -- edits = concatMap (maybeToList toTextEdit) [maybeDefRange, maybeNameRange]
        edits = map toTextEdit mentioned
        tde = TextDocumentEdit (_versionedTextDocumentIdentifier # vdoc) edits
        rsp = WorkspaceEdit Nothing (Just [InL tde]) Nothing
    responder $ Right $ InL rsp
  ]
  where
    lookupFile cache x = Map.lookup x cache
    toBnfcPosition (Position l r) = (fromIntegral l + 1, fromIntegral r + 1)
    responseError comment = TResponseError (InL LSPErrorCodes_RequestFailed) (T.pack comment) Nothing
    showSomeTerm (SomeTerm n) = showLambdaPi n

runLambdaPiLSAlt :: IO ()
runLambdaPiLSAlt = do
  langEnv <- defaultLangEnv
  void $ runServer
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = T.pack "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (flip runReaderT langEnv . runLspT env) liftIO
      , options = defaultOptions
      }

data LangType (scope :: S) where
  IntType :: Distinct scope => LangType scope
  StrType :: Distinct scope => LangType scope
  BoolType :: Distinct scope => LangType scope

instance Show (LangType n) where
  show = \case
    IntType -> "INT"
    StrType -> "STR"
    BoolType -> "BOOL"

instance AlphaEquiv LangType where
  -- alphaEquiv :: (F.Distinct n) => F.Scope n -> t n -> t n -> Bool
  alphaEquiv _ a = \case
    IntType -> case a of 
      IntType -> True 
      _ -> False
    StrType -> case a of 
      StrType -> True 
      _ -> False
    BoolType -> case a of 
      BoolType -> True 
      _ -> False

instance Sinkable LangType where
  sinkabilityProof _ = unsafeCoerce

instance TypedPattern LangType NameBinder where
  extractPatternType _ = Nothing
  extractTypedBinders binder t= TypedNameBindersCons binder t TypedNameBindersEmpty

instance TypingSig NameBinder LangType (LambdaPiSig ASTAnn) where
  -- checkSig :: Distinct n 
  --   => Context' ty n 
  --   -> sig (ScopedCheckInfer (AST binder sig) binder ty n) (CheckInfer (AST binder sig) ty n) 
  --   -> ty n
  --   -> Either String ()
  checkSig = defaultCheckSig

  -- inferSig :: Distinct n 
  --   => Context' ty n 
  --   -> sig (ScopedCheckInfer (AST binder sig) binder ty n) (CheckInfer (AST binder sig) ty n) 
  --   -> Either String (ty n)
  inferSig ctx node = case node of
    L2 (UniverseF _) -> Right BoolType
    _ -> Right IntType

