{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module AgnosticLanguageServer.Types
  ( SomeName(..)
  , SomeScopeWithAST(..)
  , SomeAST(..)
  , SomePattern(..)
  , LSConfiguration(..)
  , ASTTokens
  , ScopedASTTokens
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Language.LSP.Protocol.Types (SemanticTokenAbsolute)

data SomeName binder sig where
  SomeName :: F.Name n -> SomeName binder sig

data SomeScopeWithAST binder sig where
  SomeScopeWithAST :: F.Distinct n
    => F.Scope n
    -> F.AST binder sig n
    -> SomeScopeWithAST binder sig

data SomeAST binder sig where
  SomeAST :: F.Distinct n
    => F.AST binder sig n
    -> SomeAST binder sig

data SomePattern binder sig where
  SomePattern :: binder n l -> SomePattern binder sig

data LSConfiguration binder sig = LSConfiguration
  { fileExtension :: String
  , buildAsts :: String -> [SomeScopeWithAST binder sig]
  }

type ASTTokens = [SemanticTokenAbsolute]
type ScopedASTTokens = (ASTTokens, ASTTokens)
