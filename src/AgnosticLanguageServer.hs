{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module AgnosticLanguageServer
  ( module AgnosticLanguageServer.Cache
  , module AgnosticLanguageServer.Classes
  , module AgnosticLanguageServer.Common
  , module AgnosticLanguageServer.Types
  , module AgnosticLanguageServer.AST
  , module AgnosticLanguageServer.Handlers.Diagnostics
  , module AgnosticLanguageServer.Handlers.Definition
  , module AgnosticLanguageServer.Handlers.Hover
  , module AgnosticLanguageServer.Handlers.Rename
  , module AgnosticLanguageServer.Handlers.SemanticTokens
  , module AgnosticLanguageServer.Handlers.Sync
  , handlers
  , runLanguageServer
  ) where

import AgnosticLanguageServer.AST
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Common
import AgnosticLanguageServer.Handlers.Definition
import AgnosticLanguageServer.Handlers.Diagnostics
import AgnosticLanguageServer.Handlers.Hover
import AgnosticLanguageServer.Handlers.Rename
import AgnosticLanguageServer.Handlers.SemanticTokens
import AgnosticLanguageServer.Handlers.Sync
import AgnosticLanguageServer.Types
import qualified Control.Monad.Foil.Internal as F
import qualified Data.Text as T
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Functor (void)
import Data.IORef (writeIORef)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Control.Monad.Reader (runReaderT, liftIO)

handlers ::
  ( Bifunctor sig
  , Bifoldable sig
  , TokenizableSig sig
  , HoverableSig sig
  , HoverablePat binder
  , DiagnosableSig sig
  , DiagnosablePat binder
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder
  , TokenizablePat binder )
  => LSConfiguration binder sig
  -> LangEnv (SomeScopeWithAST binder sig)
  -> Handlers (LSP (SomeScopeWithAST binder sig))
handlers (LSConfiguration ext buildAsts') langEnv = mconcat
  [ notificationHandler SMethod_Initialized (handleInitialized cacheAsts)
  , notificationHandler SMethod_TextDocumentDidOpen handleDidOpen
  , notificationHandler SMethod_TextDocumentDidChange (handleDidChange buildAsts' langEnv)
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles (handleDidChangeWatchedFiles cacheAsts)
  , requestHandler SMethod_TextDocumentDefinition handleDefinition
  , requestHandler SMethod_TextDocumentRename handleRename
  , requestHandler SMethod_TextDocumentSemanticTokensFull semanticTokens
  , requestHandler SMethod_TextDocumentHover showHover
  ]
  where
    cacheAsts = buildCache ext buildAsts'

runLanguageServer ::
  ( Bifunctor sig
  , Bifoldable sig
  , TokenizableSig sig
  , HoverableSig sig
  , HoverablePat binder
  , DiagnosableSig sig
  , DiagnosablePat binder
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder
  , TokenizablePat binder )
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
      , doInitialize = \env _req -> do
          liftIO $ writeIORef (lspEnvRef langEnv) (Just env)
          pure $ Right env
      , staticHandlers = \_caps -> handlers config langEnv
      , interpretHandler = \env -> Iso
          (flip runReaderT langEnv . runLspT env)
          liftIO
      , options = defaultOptions
          { optTextDocumentSync = Just TextDocumentSyncOptions
              { _openClose = Just True
              , _change = Just TextDocumentSyncKind_Full
              , _willSave = Nothing
              , _willSaveWaitUntil = Nothing
              , _save = Nothing
              }
          }
      }
