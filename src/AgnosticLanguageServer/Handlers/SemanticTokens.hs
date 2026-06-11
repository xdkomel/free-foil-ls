{-# LANGUAGE DataKinds #-}

module AgnosticLanguageServer.Handlers.SemanticTokens
  ( tokenizeAST
  , semanticTokens
  ) where

import qualified Control.Monad.Free.Foil as F
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor, bimap)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.))
import AgnosticLanguageServer.Cache
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Types

tokenizeAST :: (Bifunctor sig, TokenizableSig sig, TokenizablePat binder)
  => F.AST binder sig n
  -> [SemanticTokenAbsolute]
tokenizeAST ast = case ast of
  F.Var{} -> []
  F.Node node -> tokenize $ bimap tokenizeScopedAST tokenizeAST node
  where
    tokenizeScopedAST (F.ScopedAST pat body) = (tokenizePat pat, tokenizeAST body)

semanticTokens :: (Bifunctor sig, TokenizableSig sig, TokenizablePat binder)
  => Handler (LSP (SomeScopeWithAST binder sig)) 'Method_TextDocumentSemanticTokensFull
semanticTokens req responder = do
  LangStore cache <- getCachedStore
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
      asts = maybe [] langAst $ uriToFilePath uri >>= (`Map.lookup` cache)
      tokens = concatMap tokenizeTree asts
      encoded = encodeTokens defaultSemanticTokensLegend $ relativizeTokens tokens
  either
    (\_ -> return ())
    (responder . Right . InL . SemanticTokens Nothing)
    encoded
  where
    tokenizeTree (SomeScopeWithAST _ a) = tokenizeAST a
