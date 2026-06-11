{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module AgnosticLanguageServer.Handlers.Rename
  ( handleRename
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Data.Map as Map
import Data.Bifoldable (Bifoldable)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.), (#))
import AgnosticLanguageServer.AST (mentionedRanges)
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Types

handleRename ::
  ( Bifoldable sig
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentRename
handleRename req responder = do
  let parameters = req ^. LSP.params
      pos = parameters ^. LSP.position
      newSymName = parameters ^. LSP.newName
      fileUri = parameters ^. LSP.textDocument . LSP.uri
  vdoc <- getVersionedTextDoc $ parameters ^. LSP.textDocument
  LangStore cache <- getCachedStore
  let asts = maybe [] langAst (uriToFilePath fileUri >>= (`Map.lookup` cache))
      mentioned = concatMap (mentionedRanges pos) asts
      toTextEdit range' = InL $ TextEdit range' newSymName
      edits = map toTextEdit mentioned
      tde = TextDocumentEdit (_versionedTextDocumentIdentifier # vdoc) edits
      rsp = WorkspaceEdit Nothing (Just [InL tde]) Nothing
  responder $ Right $ InL rsp
