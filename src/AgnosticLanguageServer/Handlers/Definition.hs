{-# LANGUAGE DataKinds #-}

module AgnosticLanguageServer.Handlers.Definition
  ( handleDefinition
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Bifoldable (Bifoldable)
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Language.LSP.Server
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.))
import AgnosticLanguageServer.AST (definitionRange)
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Common (firstJustL, maybeToEither)
import AgnosticLanguageServer.Types

handleDefinition ::
  ( Bifoldable sig
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentDefinition
handleDefinition req responder = do
  LangStore cache <- getCachedStore
  let parameters = req ^. LSP.params
      pos = parameters ^. LSP.position
      fileUri = parameters ^. LSP.textDocument . LSP.uri
      asts = maybe [] langAst (uriToFilePath fileUri >>= (`Map.lookup` cache))
      maybeRange = firstJustL $ map (definitionRange pos) asts
      maybeLocation = fmap (Location fileUri) maybeRange
  responder
    $ maybeToEither (TResponseError (InL LSPErrorCodes_RequestFailed) (T.pack "Did not find the definition") Nothing)
    $ fmap (InL . Definition . InL) maybeLocation
