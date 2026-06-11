{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module AgnosticLanguageServer.Handlers.Sync
  ( buildCache
  , handleInitialized
  , handleDidOpen
  , handleDidChange
  , handleDidChangeWatchedFiles
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Arrow (second)
import Data.Bifoldable (Bifoldable)
import Control.Monad.Reader (liftIO)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.))
import System.FilePath.Glob (compile, globDir)
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Handlers.Diagnostics
import AgnosticLanguageServer.Types

buildCache
  :: String
  -> (String -> [SomeScopeWithAST binder sig])
  -> LSP (SomeScopeWithAST binder sig) ()
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

handleInitialized ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => LSP (SomeScopeWithAST binder sig) ()
  -> Handler (LSP (SomeScopeWithAST binder sig)) 'Method_Initialized
handleInitialized cacheAsts _ = cacheAsts >> showAllDiagnostics

handleDidOpen ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentDidOpen
handleDidOpen noti = do
  let uri = noti ^. LSP.params . LSP.textDocument . LSP.uri
  showDiagnostics uri

handleDidChange ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => (String -> [SomeScopeWithAST binder sig])
  -> LangEnv (SomeScopeWithAST binder sig)
  -> Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentDidChange
handleDidChange buildAsts' langEnv noti = do
  let uri = noti ^. LSP.params . LSP.textDocument . LSP.uri
      changes = noti ^. LSP.params . LSP.contentChanges
      maybeText = case changes of
        [TextDocumentContentChangeEvent (InR whole)] ->
          Just (T.unpack (whole ^. LSP.text))
        _ -> Nothing
  case maybeText of
    Just text -> debounceRediagnose buildAsts' langEnv uri text
    Nothing -> showDiagnostics uri

handleDidChangeWatchedFiles ::
  ( F.CoSinkable binder
  , Bifoldable sig
  , DiagnosableSig sig
  , DiagnosablePat binder )
  => LSP (SomeScopeWithAST binder sig) ()
  -> Handler (LSP (SomeScopeWithAST binder sig)) 'Method_WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles cacheAsts _ = cacheAsts >> showAllDiagnostics
