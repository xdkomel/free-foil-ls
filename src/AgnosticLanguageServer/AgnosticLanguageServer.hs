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
{-# LANGUAGE NamedFieldPuns                 #-}
{-# LANGUAGE QuantifiedConstraints                 #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE ImpredicativeTypes                 #-}

module AgnosticLanguageServer.AgnosticLanguageServer where

import Common.LanguageServerCache
import qualified Control.Monad.Foil.Relative as F
import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Control.Arrow 
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Language.LSP.Protocol.Types 
import Language.LSP.Protocol.Message 
import Language.LSP.Server 
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Monad.Reader
import Control.Monad (unless)
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes, isJust)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable
import Data.ZipMatchK.Generic (ZipMatchK)

data SomeName binder sig where
  SomeName :: F.Distinct n => F.Name n -> SomeName binder sig

data TermTelescope binder sig n where
  LeafTerm :: F.Distinct n 
    => F.AST binder sig n 
    -> TermTelescope binder sig n
  NodeTerm :: (F.Distinct n, F.DExt n l, F.CoSinkable binder) 
    => F.AST binder sig n 
    -> binder n l 
    -> [TermTelescope binder sig l]
    -> TermTelescope binder sig n

data SomeScopedAST binder sig where
  SomeScopedAST :: F.Distinct n 
    => F.Scope n 
    -> F.AST binder sig n 
    -> SomeScopedAST binder sig

type FunBuildASTs binder sig = String -> [SomeScopedAST binder sig]

type FunFindNarrowest binder sig = (Int, Int) -> SomeScopedAST binder sig -> Maybe (SomeScopedAST binder sig)

type FunExtractName binder sig = SomeScopedAST binder sig -> Maybe (SomeName binder sig)

type FunBuildTelescopes binder sig = forall n . F.Distinct n => F.Scope n -> F.AST binder sig n -> [TermTelescope binder sig n]

type FunRange binder sig = SomeScopedAST binder sig -> Maybe Range

data LSConfiguration binder sig = LSConfiguration
  { fileExtension :: String
  , buildAsts :: FunBuildASTs binder sig
  , findNarrowest :: FunFindNarrowest binder sig
  , extractName :: FunExtractName binder sig
  , buildTelescopes :: FunBuildTelescopes binder sig
  , findRange :: FunRange binder sig
  -- For logging
  , printTerm :: SomeScopedAST binder sig -> String
  }

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

firstJust :: [Maybe a] -> Maybe a
firstJust (j@(Just _):_) = j
firstJust (_:t) = firstJust t
firstJust [] = Nothing

buildCache :: String -> FunBuildASTs binder sig -> LSP (SomeScopedAST binder sig) ()
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
          -- cacheStr = show $ Map.map (printNode . SomeAST . langAst) cache
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

symbolRange :: FunFindNarrowest binder sig 
  -> FunRange binder sig 
  -> (Int, Int) 
  -> SomeScopedAST binder sig
  -> Maybe Range
symbolRange fFind fRange pos ast = do
  narrowest <- fFind pos ast
  fRange narrowest

definitionRange :: FunFindNarrowest binder sig 
  -> FunExtractName binder sig
  -> FunRange binder sig 
  -> FunBuildTelescopes binder sig
  -> (Int, Int) 
  -> SomeScopedAST binder sig
  -> Maybe Range
definitionRange fFind fExtName fRange fBuildTele pos ast@(SomeScopedAST scope ast') = do
  narrowest <- fFind pos ast
  name' <- fExtName narrowest
  let telescopes = fBuildTele scope ast'
      -- usages = definingTerm termIsValid F.emptyScope name' telescope
      -- usages :: [Maybe (SomeAST binder sig)]
      usages = 
        -- definingTerms termIsValid name' telescopes
        -- [definingTerm termIsValid F.emptyScope name' t | t <- telescopes]
        map (definingTerm termIsValid name' scope) telescopes
  usage <- firstJust usages
  fRange usage
  where
    termIsValid = isJust . fFind pos

mentionedRanges :: FunFindNarrowest binder sig 
  -> FunExtractName binder sig
  -> FunRange binder sig 
  -> FunBuildTelescopes binder sig
  -> (Int, Int) 
  -> SomeScopedAST binder sig
  -> [Range]
mentionedRanges fFind fExtName fRange fBuildTele pos ast@(SomeScopedAST scope ast') =
  let narrowest = fFind pos ast
      name' = narrowest >>= fExtName 
      telescopes = fBuildTele scope ast'
      usages = (\name'' -> 
          catMaybes $ 
            map (definingTerm termIsValid name'' scope) telescopes
        ) <$> name' 
  in maybe [] (catMaybes . map fRange) usages
  where
    termIsValid = isJust . fFind pos

definingTerm :: (SomeScopedAST binder sig -> Bool) 
  -> SomeName binder sig 
  -> F.Scope n
  -> TermTelescope binder sig n
  -> Maybe (SomeScopedAST binder sig)
definingTerm _ (SomeName n) scope _ | n `F.member` scope = Nothing
definingTerm _ _ _ LeafTerm{} = Nothing
definingTerm _ _ _ (NodeTerm _ _ []) = Nothing
definingTerm leafIsValid sn@(SomeName n) scope (NodeTerm term binder (scoped:siblings)) = 
  let scope' = F.extendScopePattern binder scope
      isValid = case scoped of
        LeafTerm a -> leafIsValid $ SomeScopedAST scope' a
        _ -> False
  in if isValid && n `F.member` scope'
    then Just (SomeScopedAST scope term)
    else case definingTerm leafIsValid sn scope' scoped of
      Nothing -> definingTerm leafIsValid sn scope (NodeTerm term binder siblings)
      just@Just{} -> just

data Family binder sig = Family
  { parent :: SomeScopedAST binder sig
  , children :: [SomeScopedAST binder sig]
  , name :: SomeName binder sig
  }

mentionsChildren :: F.Distinct n
  => [SomeScopedAST binder sig]
  -> FunExtractName binder sig
  -> SomeName binder sig 
  -> F.Scope n
  -> TermTelescope binder sig n
  -> [SomeScopedAST binder sig]
mentionsChildren children fExtName sn scope (LeafTerm a)
  | maybe False (namesEq sn) $ fExtName (SomeScopedAST scope a) = children ++ [SomeScopedAST scope a]
  where
    namesEq (SomeName n1) (SomeName n2) = (coerce n1) == n2
mentionsChildren children _ _ _ LeafTerm{} = children
mentionsChildren children _ _ _ (NodeTerm _ _ []) = children
mentionsChildren children extName name' scope (NodeTerm t binder (child:siblings)) =
  let scope' = F.extendScopePattern binder scope
      deep = mentionsChildren children extName name' scope' child
      breadth = mentionsChildren children extName name' scope (NodeTerm t binder siblings)
  in deep ++ breadth

nameFamily :: (F.Distinct n, F.ExtEndo n)
  => FunExtractName binder sig
  -> SomeName binder sig 
  -> F.Scope n
  -> TermTelescope binder sig n
  -> Maybe (Family binder sig)
nameFamily _ (SomeName n) scope _ | n `F.member` scope = Nothing
nameFamily f someName extScope tt = nameFamily' f someName extScope tt

nameFamily' :: (F.Distinct n, F.ExtEndo n)
  => FunExtractName binder sig
  -> SomeName binder sig 
  -> F.Scope n
  -> TermTelescope binder sig n
  -> Maybe (Family binder sig)
nameFamily' f sn@(SomeName n) scope tt@(NodeTerm t binder (child:siblings)) =
  let scope' = F.extendScopePattern binder scope
  in if n `F.member` scope' 
    then Just Family 
      { parent = SomeScopedAST scope t
      , children = mentionsChildren [] f sn scope tt
      , name = sn
      }
    else 
      let deepMentions = nameFamily' f sn scope' child
          breadthMentions = nameFamily' f sn scope (NodeTerm t binder siblings)
      in firstJust [deepMentions, breadthMentions]
nameFamily' _ _ _ _ = Nothing
-- nameFamily 
-- nameFamily _ sn@(SomeName n) scope (NodeTerm t binder []) = 
--   let scope' = F.extendScopePattern binder scope
--   in if n `F.member` scope' 
--     then Just Family { parent = SomeScopedAST scope t, children = [], name = sn }
--     else Nothing
-- nameFamily extName sn@(SomeName n) scope tt@(NodeTerm t binder (child:siblings)) =
--   let scope' = F.extendScopePattern binder scope
--   in if n `F.member` scope' 
--     then Just Family 
--       { parent = SomeScopedAST scope t
--       , children = mentionsChildren [] extName sn scope tt
--       , name = sn
--       }
--     else 
--       let deepMentions = nameFamily extName sn scope' child
--           breadthMentions = nameFamily extName sn scope (NodeTerm t binder siblings)
--       in firstJust [deepMentions, breadthMentions]
-- nameFamily _ _ _ _ = Nothing     

handlers :: LSConfiguration binder sig -> Handlers (LSP (SomeScopedAST binder sig))
handlers (LSConfiguration fileExtension buildAsts findNarrowest extractName buildTele findRange printTerm) =
  let cacheAsts = buildCache fileExtension buildAsts 
      defRange = definitionRange findNarrowest extractName findRange buildTele
      -- symRange = symbolRange findNarrowest astRange
      usagesRanges = mentionedRanges findNarrowest extractName findRange buildTele
      -- traceVar = traces 
      -- traceVar = traces findNarrowest extractName buildTele showSomeAST
  in mconcat
  [ notificationHandler SMethod_Initialized $ const $ cacheAsts
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const $ cacheAsts
  , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
    LangStore cache <- getCachedStore
    let parameters = req ^. LSP.params
        bnfcPosition = toBnfcPosition $ parameters ^. LSP.position
        fileUri = parameters ^. LSP.textDocument . LSP.uri
        maybeCurrentFile = uriToFilePath fileUri
        asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        maybeRange = firstJust 
          $ map (defRange bnfcPosition) asts
        -- usages = concatMap ((++ "\n\n") . traceVar StrType) asts
        --  concatMap ((++ "\n\n") . traceVar) asts
    sendNotification SMethod_WindowShowMessage 
      $ ShowMessageParams MessageType_Info 
      $ T.pack 
      $ "Range identified: " ++ show maybeRange
    -- sendNotification SMethod_WindowShowMessage 
    --   $ ShowMessageParams MessageType_Info 
    --   $ T.pack 
    --   $ "Usages: " ++ usages
    let maybeLocation = fmap (Location fileUri) maybeRange
    responder 
      $ maybeToEither (responseError "Did not find the definition") 
      $ fmap (InL . Definition . InL) maybeLocation
  , requestHandler SMethod_TextDocumentRename $ \req responder -> do
    let parameters = req ^. LSP.params
        bnfcPosition = toBnfcPosition $ parameters ^. LSP.position
        newSymName = parameters ^. LSP.newName
        fileUri = parameters ^. LSP.textDocument . LSP.uri
        maybeCurrentFile = uriToFilePath fileUri
    vdoc <- getVersionedTextDoc $ parameters ^. LSP.textDocument
    LangStore cache <- getCachedStore
    let asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        mentioned = concatMap (usagesRanges bnfcPosition) asts
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
    -- showSomeAST (SomeAST n) = printTerm

runLanguageServer :: LSConfiguration binder sig -> IO ()
runLanguageServer config = do
  langEnv <- defaultLangEnv
  void $ runServer
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = T.pack "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers config
      , interpretHandler = \env -> Iso (flip runReaderT langEnv . runLspT env) liftIO
      , options = defaultOptions
      }



-- Typechecking

--------------------------------------------------------------------------------

-- * Core Types

--------------------------------------------------------------------------------

-- | Typing context mapping names to types
type Context' ty n = F.NameMap n (ty n)

-- | Scoped value with a binder
data Scoped binder (t :: F.S -> Type) (n :: F.S) where
  Scoped :: binder n l -> t l -> Scoped binder t n

-- | Type errors
data TypeError ty
  = TypeErrorUnexpectedType ty ty
  | TypeErrorUnexpectedDependentType
  deriving (Show)

-- | Bidirectional type checking result
data CheckInfer term ty (n :: F.S) = CheckInfer
  { check :: ty n -> Either String (),
    infer :: Either String (ty n),
    getTerm :: term n
  }

-- | Type for scoped checking/inference
type ScopedCheckInfer term binder ty (n :: F.S) =
  Maybe (ty n) -> CheckInfer (Scoped binder term) (Scoped binder ty) n

-- | Typed name binders data structure
data TypedNameBinders ty n l where
  TypedNameBindersEmpty :: TypedNameBinders ty n n
  TypedNameBindersCons ::
    F.NameBinder n i -> ty n -> TypedNameBinders ty i l -> TypedNameBinders ty n l

--------------------------------------------------------------------------------

-- * Required Type Classes

--------------------------------------------------------------------------------

-- | Alpha equivalence class
class AlphaEquiv t where
  alphaEquiv :: (F.Distinct n) => F.Scope n -> t n -> t n -> Bool

-- | Default instance for Free Foil ASTs
instance
  (Bifunctor sig, Bifoldable sig, ZipMatchK sig, F.UnifiablePattern binder) =>
  AlphaEquiv (F.AST binder sig)
  where
  alphaEquiv = alphaEquiv

-- | Main typing signature class
class
  (forall n. Show (TypeError (ty n)), AlphaEquiv ty) =>
  TypingSig binder ty sig
  where
  checkSig ::
    (F.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
    ty n ->
    Either String ()
  checkSig = defaultCheckSig

  inferSig ::
    (F.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
    Either String (ty n)

-- | Class for typed patterns
class TypedPattern ty pat where
  extractPatternType :: pat n l -> Maybe (ty n)
  extractTypedBinders :: pat n l -> ty n -> TypedNameBinders ty n l

-- | Class for creating trivially scoped values
class HasTrivialBinder binder where
  triviallyScoped ::
    (F.Distinct n, F.Sinkable ty) =>
    F.Scope n ->
    ty n ->
    Scoped binder ty n

instance HasTrivialBinder F.NameBinder where
  triviallyScoped scope type_ =
    F.withFresh scope $ \binder ->
      Scoped binder (F.sink type_)

--------------------------------------------------------------------------------

-- * Generic Utilities

--------------------------------------------------------------------------------

nameMapToScope :: F.NameMap n a -> F.Scope n
nameMapToScope (F.NameMap m) = F.UnsafeScope (IntMap.keysSet m)
-- deriving instance Functor (F.NameMap n)

-- | Check if actual type matches expected type
shouldBe ::
  (AlphaEquiv ty, F.Distinct n, Show (ty n)) =>
  (F.NameMap n (ty n), ty n) ->
  ty n ->
  Either String ()
shouldBe (scope, actualType) expectedType
  | sameType = return ()
  | otherwise =
      Left $
        unlines
          [ "expected type"
          , "  " ++ show expectedType
          , "but got type"
          , "  " ++ show actualType
          , "when typechecking expression"
          -- ,  "  " ++ show e
          ]
  where
    sameType = alphaEquiv (nameMapToScope scope) actualType expectedType

-- | Default implementation of checkSig
defaultCheckSig ::
  (F.Distinct n, TypingSig binder ty sig) =>
  Context' ty n ->
  sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
  ty n ->
  Either String ()
defaultCheckSig ctx node expectedType = do
  inferredType <- inferSig ctx node
  unless (alphaEquiv (nameMapToScope ctx) inferredType expectedType) $
    Left (show (TypeErrorUnexpectedType inferredType expectedType))

-- | Extract type from a binder
extractTypeFromBinder ::
  (TypedPattern ty binder, AlphaEquiv ty, F.Distinct n) =>
  Context' ty n ->
  binder n l ->
  Maybe (ty n) ->
  Either String (ty n)
extractTypeFromBinder _scope binder Nothing =
  maybe (Left "cannot infer without type annotation for pattern") Right $
    extractPatternType binder
extractTypeFromBinder scope binder (Just ty) =
  maybe
    (Right ty)
    ( \binderTy ->
        if alphaEquiv (nameMapToScope scope) binderTy ty
          then Right ty
          else Left "type mismatch"
    )
    $ extractPatternType binder

--------------------------------------------------------------------------------

-- * Main Bidirectional Type Checking API

--------------------------------------------------------------------------------

-- | Check a term against an expected type
bidirectionalCheck ::
  ( F.Distinct n
  , Bitraversable sig
  , AlphaEquiv ty
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  ty n ->
  Either String ()
bidirectionalCheck scope t expectedType = do
  ci <- bidirectionalCheckInfer scope t
  check ci expectedType

-- | Infer the type of a term
bidirectionalInfer ::
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  Either String (ty n)
bidirectionalInfer scope t = do
  ci <- bidirectionalCheckInfer scope t
  infer ci

-- | Combined check/infer for a term
bidirectionalCheckInfer ::
  forall ty binder sig n.
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  Either String (CheckInfer (F.AST binder sig) ty n)

-- Variable case
bidirectionalCheckInfer scope t@(F.Var n) = do
  let inferredType = F.lookupName n scope
  return
    CheckInfer
      { infer = return inferredType,
        check = \expectedType -> do
          unless (alphaEquiv (nameMapToScope scope) inferredType expectedType) $
            Left (show (TypeErrorUnexpectedType inferredType expectedType)),
        getTerm = t
      }

-- Node case
bidirectionalCheckInfer scope (F.Node node) = do
  node' <-
    bitraverse
      (bidirectionalCheckInferScoped scope)
      (bidirectionalCheckInfer scope)
      node
  return
    CheckInfer
      { infer = inferSig scope node',
        check = checkSig scope node',
        getTerm = F.Node node
      }

extractExactlyOneBinder :: TypedPattern ty pat => pat n l -> ty n -> F.NameBinder n l
extractExactlyOneBinder binder ty = 
  case extractTypedBinders binder ty of
    TypedNameBindersCons extractedBinder _ty TypedNameBindersEmpty -> extractedBinder
    _ -> error "Expected exactly one binder"

-- | Bidirectional check/infer for scoped terms
bidirectionalCheckInferScoped ::
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.ScopedAST binder sig n ->
  Either String (ScopedCheckInfer (F.AST binder sig) binder ty n)
bidirectionalCheckInferScoped scope (F.ScopedAST binder body) =
  case (F.assertExt binder, F.assertDistinct binder) of
    (F.Ext, F.Distinct) -> return $ \mbinderType ->
      CheckInfer
        { infer = do
            ty <- extractTypeFromBinder scope binder mbinderType
            let scope' = F.sink <$> F.addNameBinder (extractExactlyOneBinder binder ty) ty scope
            ci <- bidirectionalCheckInfer scope' body
            Scoped binder <$> infer ci,
          check = \(Scoped binder' expectedType) -> do
            -- TODO: check binder' against binder
            ty <- extractTypeFromBinder scope binder mbinderType
            case F.unifyPatterns binder binder' of
              F.SameNameBinders _binders -> do
                let scope' =
                      F.sink <$> F.addNameBinder (extractExactlyOneBinder binder ty) ty scope
                ci <- bidirectionalCheckInfer scope' body
                check ci expectedType
              F.RenameLeftNameBinder _binders renameL ->
                case (F.assertExt binder', F.assertDistinct binder') of
                  (F.Ext, F.Distinct) -> do
                    let scope' =
                          F.sink <$> F.addNameBinder (extractExactlyOneBinder binder' ty) ty scope
                        body' =
                          F.liftRM
                            (nameMapToScope scope')
                            (F.fromNameBinderRenaming renameL)
                            body
                    ci <- bidirectionalCheckInfer scope' body'
                    check ci expectedType

              -- FIXME: RenameRightNameBinder, RenameBothNameBinders
              _ -> Left "non-unifiable patterns",
          getTerm = Scoped binder body
        }


