{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module AgnosticLanguageServer.Handlers.Hover
  ( hoverMessage
  , showHover
  ) where

import qualified Control.Monad.Free.Foil as F
import qualified Control.Monad.Foil.Internal as F
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Bifoldable (Bifoldable)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.))
import AgnosticLanguageServer.AST (findNarrowest)
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Common (firstJustL)
import AgnosticLanguageServer.Types

hoverMessage ::
  ( Bifoldable sig
  , HoverableSig sig
  , HoverablePat binder
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
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
    else Just (concatMap (\l -> "- " ++ l ++ "\n\n") hd, maybeRange)

showHover ::
  ( Bifoldable sig
  , HoverableSig sig
  , HoverablePat binder
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentHover
showHover req responder = do
  LangStore cache <- getCachedStore
  let parameters = req ^. LSP.params
      uri = parameters ^. LSP.textDocument . LSP.uri
      pos = parameters ^. LSP.position
      asts = maybe [] langAst $ uriToFilePath uri >>= (`Map.lookup` cache)
      hover = firstJustL $ map (hoverMessage pos) asts
  maybe
    (return ())
    (responder . Right . InL . toHover)
    hover
  where
    toHover (msg, range') = Hover (InL $ mkMarkdown $ T.pack msg) range'
