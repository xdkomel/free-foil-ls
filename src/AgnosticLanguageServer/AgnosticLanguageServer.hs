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
import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Control.Arrow 
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.LSP.Protocol.Types 
import Language.LSP.Protocol.Message 
import Language.LSP.Server 
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Monad.Reader
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes)
import Data.Bifoldable (Bifoldable, bifoldMap, bifoldr)
import Data.Bifunctor (Bifunctor, bimap)

data SomeName binder sig where
  SomeName :: F.Name n -> SomeName binder sig

data SomeScopeWithAST binder sig where
  SomeScopeWithAST :: (F.Distinct n) 
    => F.Scope n 
    -> F.AST binder sig n 
    -> SomeScopeWithAST binder sig

type FunBuildASTs b s = String -> [SomeScopeWithAST b s]

data SomeAST binder sig where
  SomeAST :: (F.Distinct n)
    => F.AST binder sig n
    -> SomeAST binder sig

data SomePattern binder sig where
  SomePattern :: binder n l -> SomePattern binder sig

data LSConfiguration binder sig = LSConfiguration
  { fileExtension :: String
  , buildAsts :: FunBuildASTs binder sig
  -- For logging
  , printTerm :: SomeAST binder sig -> String
  }

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

firstJustL :: [Maybe a] -> Maybe a
firstJustL (j@(Just _):_) = j
firstJustL (_:t) = firstJustL t
firstJustL [] = Nothing

lastJustR :: Maybe a -> Maybe a -> Maybe a
lastJustR = maybe id (const . Just)

buildCache :: String -> FunBuildASTs binder sig -> LSP (SomeScopeWithAST binder sig) ()
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
      let cache = Map.fromList $ map (second astToCache) asts
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

inRange :: Position -> Range -> Bool
inRange (Position l c) (Range (Position x y) (Position x' y')) = 
  let startsOK = if l == x then c >= y else True
      endsOK = if l == x' then c <= y' else True
  in l >= x && l <= x' && startsOK && endsOK

class Ranged sig where
  range :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> Maybe Range
  nameRange 
    :: SomeName binder sig 
    -> sig (F.ScopedAST binder sig n) (F.AST binder sig n) 
    -> Maybe Range

class RangedPattern pat where
  foldrPat 
    :: (SomePattern pat sig -> r -> r)
    -> r
    -> pat n l
    -> r
  rangePat :: pat n l -> Maybe Range
  binderOf :: pat n l -> Maybe (F.NameBinder n l)


findNarrowest :: 
  ( Bifoldable sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder
  , F.Distinct n )
  => Position 
  -> F.AST binder sig n 
  -> Maybe (Either (SomePattern binder sig, SomeAST binder sig) (SomeAST binder sig))
findNarrowest = ((fmap fst) .) . findNarrowest'
  where
    findNarrowest' :: 
      ( Bifoldable sig
      , Ranged sig
      , F.CoSinkable binder
      , RangedPattern binder
      , F.Distinct n )
      => Position 
      -> F.AST binder sig n 
      -> Maybe (Either (SomePattern binder sig, SomeAST binder sig) (SomeAST binder sig), Range)
    findNarrowest' pos ast = do
      sig <- case ast of
        F.Var{} -> Nothing
        F.Node s -> Just s
      r <- range sig
      if not $ inRange pos r 
        then Nothing 
        else 
          let narr = narrowest pos
          in bifoldr
            (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
              F.Distinct -> 
                let narrPat = 
                      findNarrowestPat (SomeAST a) pos (SomePattern binder)
                    narrNode = findNarrowest' pos a
                in narr $ narr narrPat narrNode
              )
            (narr . findNarrowest' pos)
            (Just (Right $ SomeAST ast, r))
            sig
    narrowest pos = \case
      a@(Just (_, r)) -> if inRange pos r then const a else id
      _ -> id
    findNarrowestPat ast pos (SomePattern pat) = do
      r <- rangePat pat
      if not $ inRange pos r 
        then Nothing
        else foldrPat 
          (\p -> maybe (findNarrowestPat ast pos p) Just) 
          (Just (Left (SomePattern pat, ast), r)) 
          pat

extractName :: (Bifoldable sig, F.Distinct n, F.CoSinkable binder)
  => F.AST binder sig n -> Maybe (SomeName binder sig)
extractName = \case
  -- Valid, but this must be a wrapper-node, not a [Var] on its own
  F.Var{} -> Nothing
  F.Node n -> bifoldr extract' extract Nothing n
  where
    extract = \case
      F.Var n -> const (Just $ SomeName n)
      _ -> id
    extract' (F.ScopedAST binder t) = case F.assertDistinct binder of
      F.Distinct -> extract t

nodeCovers :: Ranged sig 
  => Position -> sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool
nodeCovers pos = (maybe False (inRange pos)) . range

patternNameRange :: (RangedPattern binder) 
  => SomeName binder sig -> SomePattern binder sig -> Maybe Range
patternNameRange sn@(SomeName name') (SomePattern pat) = case binderOf pat of
  Just binder -> 
    if F.nameId name' == F.nameId (F.nameOf binder)
      then rangePat pat
      else Nothing
  _ -> foldrPat (\p -> maybe (patternNameRange sn p) Just) Nothing pat


definitionRange :: 
  ( Bifoldable sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder )
  => Position
  -> SomeScopeWithAST binder sig
  -> Maybe Range
definitionRange pos (SomeScopeWithAST scope ast) = 
  findNarrowest pos ast >>= \case
    Left (SomePattern pat, _) -> rangePat pat
    Right (SomeAST node) -> do
      name' <- extractName node
      (pat, _) <- findDefinition (nodeCovers pos) name' scope ast
      patternNameRange name' pat

mentionedRanges :: 
  ( Bifoldable sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder )
  => Position
  -> SomeScopeWithAST binder sig
  -> [Range]
mentionedRanges pos (SomeScopeWithAST scope ast) = maybe [] id $ do
  narrowest <- findNarrowest pos ast 
  (name', pat, definition) <- case narrowest of
    Left (sp@(SomePattern p), ast) -> fmap 
      (\nb -> (SomeName $ F.nameOf nb, sp, ast)) $ binderOf p
    Right (SomeAST node) -> do
      name' <- extractName node
      (pat, definition) <- findDefinition (nodeCovers pos) name' scope ast
      Just (name', pat, definition)
  let kidsRanges = catMaybes
        $ map (nameRange' name') 
        $ findRefs name' definition
  Just
    $ maybe kidsRanges (:kidsRanges) 
    $ patternNameRange name' pat
  where
    nameRange' :: Ranged sig 
      => SomeName binder sig -> SomeAST binder sig -> Maybe Range
    nameRange' name' (SomeAST ast') = case ast' of
      F.Var{} -> Nothing
      F.Node sig -> nameRange name' sig

findDefinition ::
  ( F.Distinct n
  , Bifoldable sig
  , F.CoSinkable binder )
  => (forall m . sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool)
  -> SomeName binder sig
  -> F.Scope n
  -> F.AST binder sig n
  -> Maybe (SomePattern binder sig, SomeAST binder sig)
findDefinition f sn@(SomeName n) scope ast
  | n `F.member` scope = Nothing
  | otherwise = findDefinition' f sn scope ast

findDefinition' ::
  ( F.Distinct n
  , Bifoldable sig
  , F.CoSinkable binder )
  => (forall m . sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool)
  -> SomeName binder sig
  -> F.Scope n
  -> F.AST binder sig n
  -> Maybe (SomePattern binder sig, SomeAST binder sig)
findDefinition' isValid sn@(SomeName name') scope = \case
  F.Var{} -> Nothing
  F.Node sig -> bifoldr
    (firstJust . defScoped sig)
    (firstJust . findDefinition' isValid sn scope)
    Nothing
    sig
  where
    firstJust = maybe id (const . Just)
    defScoped sig (F.ScopedAST binder node') = 
      case (F.assertDistinct binder, F.assertExt binder) of
        (F.Distinct, F.Ext) ->
          let scope' = F.extendScopePattern binder scope
              member = name' `F.member` scope'
              valid = case node' of
                F.Var{} -> isValid sig
                F.Node s -> isValid s
          in case (valid, member) of
            (False, _) -> Nothing
            (_, False) -> findDefinition' isValid sn scope' node'
            _ -> Just (SomePattern binder, SomeAST node')
            
findRefs :: (Bifoldable sig, F.CoSinkable binder)
  => SomeName binder sig 
  -> SomeAST binder sig
  -> [SomeAST binder sig]
findRefs sn (SomeAST ast) = case ast of
  F.Var{} -> []
  F.Node sig -> bifoldMap 
    (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
      F.Distinct -> findRefs' sn a ) 
    (findRefs' sn) 
    sig

findRefs' :: (F.Distinct m, Bifoldable sig, F.CoSinkable binder) 
  => SomeName binder sig -> F.AST binder sig m -> [SomeAST binder sig]
findRefs' sn ast = 
  let grandKids = findRefs sn (SomeAST ast)
  in maybe grandKids (:grandKids) $ do
    name' <- extractName ast
    if namesEq sn name' 
      then Just $ SomeAST ast
      else Nothing
  where
    namesEq (SomeName n) (SomeName n') = F.nameId n == F.nameId n'

class TokenizableSig sig where
  tokenize :: sig ScopedASTTokens ASTTokens -> [SemanticTokenAbsolute]

class TokenizablePattern pat where
  tokenizePat :: pat n l -> [SemanticTokenAbsolute]

placeholderTokenize :: sig -> [SemanticTokenAbsolute]
placeholderTokenize _ = []

type ASTTokens = [SemanticTokenAbsolute]
type ScopedASTTokens = (ASTTokens, ASTTokens)

tokenizeAST :: (Bifunctor sig, TokenizableSig sig, TokenizablePattern binder) 
  => F.AST binder sig n 
  -> [SemanticTokenAbsolute]
tokenizeAST ast = case ast of
  F.Var{} -> []
  F.Node node -> tokenize $ bimap tokenizeScopedAST tokenizeAST node
  where
    tokenizeScopedAST (F.ScopedAST pat body) = (tokenizePat pat, tokenizeAST body)

semanticTokens :: (Bifunctor sig, TokenizableSig sig, TokenizablePattern binder) 
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentSemanticTokensFull
semanticTokens req responder = do
  LangStore cache <- getCachedStore
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri 
      asts = maybe [] langAst $ (uriToFilePath uri) >>= (\x -> Map.lookup x cache)
      tokens = concatMap tokenizeTree asts
      encoded = encodeTokens defaultSemanticTokensLegend 
        $ relativizeTokens tokens
  either 
    (\_ -> return ()) 
    (responder . Right . InL . SemanticTokens Nothing)
    encoded
  where
    tokenizeTree (SomeScopeWithAST _ a) = tokenizeAST a

class Hoverable sig where
  hoverData :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> [String]
  hoverData = placeholderHoverData

class HoverablePat pat where
  hoverDataPat :: pat n l -> [String]

placeholderHoverData _ = []

showHover :: 
  ( Bifoldable sig
  , Hoverable sig
  , HoverablePat binder
  , DiagnosableSig sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder )
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentHover
showHover req responder = do
  LangStore cache <- getCachedStore
  let parameters = req ^. LSP.params
      uri = parameters ^. LSP.textDocument . LSP.uri 
      pos = parameters ^. LSP.position
      
      asts = maybe [] langAst $ (uriToFilePath uri) >>= (\x -> Map.lookup x cache)
      hover = firstJustL $ map (hoverMessage pos) asts
  maybe 
    (return ()) 
    (responder . Right . InL . toHover) 
    hover
  showDiagnostics uri
  where
    toHover (msg, range') = Hover (InL $ mkMarkdown $ T.pack msg) range'

hoverMessage :: 
  ( Bifoldable sig
  , Hoverable sig
  , HoverablePat binder
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder )
  => Position
  -> SomeScopeWithAST binder sig 
  -> Maybe (String, Maybe Range)
hoverMessage p (SomeScopeWithAST _ ast) = do
  narrowest <- findNarrowest p ast
  let (hd, maybeRange) = case narrowest of
        Right (SomeAST (F.Node sig)) -> (hoverData sig, range sig)
        Left (SomePattern pat, _) -> (hoverDataPat pat, rangePat pat)
        _ -> ([], Nothing)
  if null hd
    then Nothing 
    else Just 
      $ ( concatMap (\l -> "- " ++ l ++ "\n\n") hd
        , maybeRange )

class DiagnosableSig sig where
  diagnose :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> [Diagnostic]
  diagnose = placeholderDiagnose

placeholderDiagnose _ = []

showDiagnostics :: (F.CoSinkable binder, Bifoldable sig, DiagnosableSig sig)
  => Uri
  -> LSP (SomeScopeWithAST binder sig) ()
showDiagnostics uri = do
  LangStore cache <- getCachedStore
  sendNotification SMethod_WindowShowMessage 
    $ ShowMessageParams MessageType_Info 
    $ T.pack 
    $ "[showDiagnostics] called with cache length: " ++ show (length cache)
  let asts = maybe [] langAst $ uriToFilePath uri
        >>= (\x -> Map.lookup x cache)
      messages = concatMap diagnoseTree asts
  sendNotification SMethod_TextDocumentPublishDiagnostics 
      $ PublishDiagnosticsParams uri Nothing messages
  where
    diagnoseTree (SomeScopeWithAST _ tree) = diagnoseAST tree
  -- sendNotification SMethod_WindowShowMessage 
  --   $ ShowMessageParams MessageType_Info 
  --   $ T.pack 
  --   $ "diagnostic messages: " ++ show messages

diagnoseAST :: (F.CoSinkable binder, Bifoldable sig, DiagnosableSig sig)
  => F.AST binder sig n -> [Diagnostic]
diagnoseAST = \case
  F.Var _ -> []
  F.Node n -> diagnose n ++ bifoldMap
    -- TODO: DiagnosablePattern
    (\(F.ScopedAST _ a) -> diagnoseAST a)
    diagnoseAST
    n

handlers :: 
  ( Bifunctor sig
  , Bifoldable sig
  , TokenizableSig sig
  , Hoverable sig
  , HoverablePat binder
  , DiagnosableSig sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder
  , TokenizablePattern binder )
  => LSConfiguration binder sig 
  -> Handlers (LSP (SomeScopeWithAST binder sig))
handlers (LSConfiguration fileExtension buildAsts printTerm) =
  let cacheAsts = buildCache fileExtension buildAsts 
  in mconcat
  [ notificationHandler SMethod_Initialized $ const cacheAsts
  , notificationHandler SMethod_TextDocumentDidOpen $ \noti -> do
    let uri = noti ^. LSP.params . LSP.textDocument . LSP.uri 
    showDiagnostics uri
  , notificationHandler SMethod_TextDocumentDidChange $ \noti -> do
    let uri = noti ^. LSP.params . LSP.textDocument . LSP.uri 
    showDiagnostics uri
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const cacheAsts
  , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
    LangStore cache <- getCachedStore
    let parameters = req ^. LSP.params
        pos = parameters ^. LSP.position
        fileUri = parameters ^. LSP.textDocument . LSP.uri
        maybeCurrentFile = uriToFilePath fileUri
        asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        maybeRange = firstJustL 
          $ map (definitionRange pos) asts
    sendNotification SMethod_WindowShowMessage 
      $ ShowMessageParams MessageType_Info 
      $ T.pack 
      $ "Current ASTS: " ++ show (map (\(SomeScopeWithAST _ ast) -> printTerm (SomeAST ast)) asts)
    let maybeLocation = fmap (Location fileUri) maybeRange
    responder 
      $ maybeToEither (responseError "Did not find the definition") 
      $ fmap (InL . Definition . InL) maybeLocation
  , requestHandler SMethod_TextDocumentRename $ \req responder -> do
    let parameters = req ^. LSP.params
        pos = parameters ^. LSP.position
        newSymName = parameters ^. LSP.newName
        fileUri = parameters ^. LSP.textDocument . LSP.uri
        maybeCurrentFile = uriToFilePath fileUri
    vdoc <- getVersionedTextDoc $ parameters ^. LSP.textDocument
    LangStore cache <- getCachedStore
    let asts = maybe [] langAst (maybeCurrentFile >>= lookupFile cache)
        mentioned = concatMap (mentionedRanges pos) asts
        toTextEdit range' = InL $ TextEdit range' newSymName
        edits = map toTextEdit mentioned
        tde = TextDocumentEdit (_versionedTextDocumentIdentifier # vdoc) edits
        rsp = WorkspaceEdit Nothing (Just [InL tde]) Nothing
    responder $ Right $ InL rsp
  , requestHandler SMethod_TextDocumentSemanticTokensFull semanticTokens
  , requestHandler SMethod_TextDocumentHover showHover
  ]
  where
    lookupFile cache x = Map.lookup x cache
    responseError comment = TResponseError (InL LSPErrorCodes_RequestFailed) (T.pack comment) Nothing

runLanguageServer :: 
  ( Bifunctor sig
  , Bifoldable sig
  , TokenizableSig sig
  , Hoverable sig
  , HoverablePat binder
  , DiagnosableSig sig
  , Ranged sig
  , F.CoSinkable binder
  , RangedPattern binder
  , TokenizablePattern binder )
  => LSConfiguration binder sig 
  -> IO ()
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

class TypeDeductive sig sig' ty binder where
  deduceType 
    :: ty 
    -> sig 
      (ty, ty -> (F.ScopedAST binder sig' n, ty)) 
      (ty -> (F.AST binder sig' n, ty)) 
    -> sig' (F.ScopedAST binder sig' n) (F.AST binder sig' n)

class Typed sig ty where
  ty :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> ty

class TypedPattern pat ty where
  patTy :: pat n l -> ty
  addPattern :: pat n l -> F.NameMap n ty -> F.NameMap l ty

typecheck :: (Bifunctor sig, TypeDeductive sig sig' ty binder, Typed sig' ty, TypedPattern binder ty, F.Distinct n, F.CoSinkable binder) 
  => F.NameMap n ty -> ty -> F.AST binder sig n -> F.AST binder sig' n
typecheck nm typ = \case 
  F.Var n -> F.Var n
  F.Node sig -> F.Node $ deduceType typ $ bimap 
    (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
      F.Distinct -> 
        let f = (first (F.ScopedAST binder)) . check (addPattern binder nm) a
        in (patTy binder, f)
      ) 
    (check nm)
    sig
  where
    astTy :: (Typed sig ty) => F.NameMap n ty -> F.AST binder sig n -> ty
    astTy nm' = \case
      F.Var n -> F.lookupName n nm'
      F.Node sig -> ty sig
    check :: (Bifunctor sig, TypeDeductive sig sig' ty binder, Typed sig' ty, TypedPattern binder ty, F.Distinct n, F.CoSinkable binder) 
      => F.NameMap n ty 
      -> F.AST binder sig n
      -> ty 
      -> (F.AST binder sig' n, ty)
    check nm' a t = 
      let a' = typecheck nm' t a
      in (a', astTy nm' a')
