{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module AgnosticLanguageServer.Classes
  ( RangedSig(..)
  , FoldablePat(..)
  , HoverableSig(..)
  , HoverablePat(..)
  , TokenizableSig(..)
  , TokenizablePat(..)
  , TypeDeductiveSig(..)
  , TypedSig(..)
  , TypedPat(..)
  , DiagnosableSig(..)
  , DiagnosablePat(..)
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Language.LSP.Protocol.Types (Diagnostic, Range, SemanticTokenAbsolute)
import AgnosticLanguageServer.Types (ASTTokens, ScopedASTTokens, SomePattern)

class RangedSig sig where
  range :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> Maybe Range
  range = const Nothing

class FoldablePat pat where
  foldrPat
    :: (SomePattern pat sig -> r -> r)
    -> r
    -> pat n l
    -> r
  foldrPat _ r _ = r
  rangePat :: pat n l -> Maybe Range
  rangePat = const Nothing
  binderOf :: pat n l -> Maybe (F.NameBinder n l)
  binderOf = const Nothing

class HoverableSig sig where
  hoverData :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> [String]
  hoverData = const []

class HoverablePat pat where
  hoverDataPat :: pat n l -> [String]
  hoverDataPat = const []

class TokenizableSig sig where
  tokenize :: sig ScopedASTTokens ASTTokens -> [SemanticTokenAbsolute]
  tokenize = const []

class TokenizablePat pat where
  tokenizePat :: pat n l -> [SemanticTokenAbsolute]
  tokenizePat = const []

class TypeDeductiveSig sig sig' ty binder where
  deduceType
    :: ty
    -> sig
      (ty, ty -> (F.ScopedAST binder sig' n, ty))
      (ty -> (F.AST binder sig' n, ty))
    -> sig' (F.ScopedAST binder sig' n) (F.AST binder sig' n)

class TypedSig sig ty where
  ty :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> ty

class TypedPat pat ty where
  patTy :: pat n l -> ty
  addPattern :: pat n l -> F.NameMap n ty -> F.NameMap l ty

class DiagnosableSig sig where
  diagnose :: sig (F.ScopedAST binder sig n) (F.AST binder sig n) -> [Diagnostic]
  diagnose = const []

class DiagnosablePat pat where
  diagnosePat :: pat n l -> [Diagnostic]
  diagnosePat = const []
