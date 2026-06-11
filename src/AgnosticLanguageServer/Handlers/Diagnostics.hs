{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module AgnosticLanguageServer.Handlers.Diagnostics
  ( diagnoseAST
  , showDiagnostics
  , showAllDiagnostics
  , rebuildFileCache
  , debounceRediagnose
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import qualified Data.Map as Map
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Functor (void)
import Data.IORef (readIORef)
import Control.Monad.Reader (runReaderT, liftIO)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Types

diagnoseAST ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => F.AST binder sig n -> [Diagnostic]
diagnoseAST = \case
  F.Var _ -> []
  F.Node n -> diagnose n ++ bifoldMap
    (\(F.ScopedAST binder a) -> diagnosePat binder ++ diagnoseAST a)
    diagnoseAST
    n

showDiagnostics ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => Uri
  -> LSP (SomeScopeWithAST binder sig) ()
showDiagnostics uri = do
  LangStore cache <- getCachedStore
  let asts = maybe [] langAst $ uriToFilePath uri >>= (`Map.lookup` cache)
      messages = concatMap diagnoseTree asts
  sendNotification SMethod_TextDocumentPublishDiagnostics
    $ PublishDiagnosticsParams uri Nothing messages
  where
    diagnoseTree (SomeScopeWithAST _ tree) = diagnoseAST tree

showAllDiagnostics ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => LSP (SomeScopeWithAST binder sig) ()
showAllDiagnostics = do
  LangStore cache <- getCachedStore
  mapM_ (showDiagnostics . filePathToUri) (Map.keys cache)

rebuildFileCache
  :: (String -> [SomeScopeWithAST binder sig])
  -> Uri
  -> String
  -> LSP (SomeScopeWithAST binder sig) ()
rebuildFileCache buildAsts' uri text = case uriToFilePath uri of
  Nothing -> return ()
  Just path -> do
    LangStore cache <- getCachedStore
    let asts = buildAsts' text
        cache' = Map.insert path LangProgramStore { langAst = asts } cache
    cacheStore (LangStore cache')

debounceRediagnose ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => (String -> [SomeScopeWithAST binder sig])
  -> LangEnv (SomeScopeWithAST binder sig)
  -> Uri
  -> String
  -> LSP (SomeScopeWithAST binder sig) ()
debounceRediagnose buildAsts' langEnv uri text = liftIO $ do
  timers <- readTVarIO (debounceTimers langEnv)
  mapM_ killThread (Map.lookup uri timers)
  maybeEnv <- readIORef (lspEnvRef langEnv)
  case maybeEnv of
    Nothing -> return ()
    Just env -> do
      tid <- forkIO $ do
        threadDelay 500000
        flip runReaderT langEnv . runLspT env $ do
          rebuildFileCache buildAsts' uri text
          showDiagnostics uri
          void $ sendRequest SMethod_WorkspaceSemanticTokensRefresh Nothing $ \_ -> return ()
      atomically $ modifyTVar' (debounceTimers langEnv) (Map.insert uri tid)
