{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}

module LanguageServer.AgnosticLanguageServerLampi where

import AgnosticLanguageServer
import qualified Lampi.Abs as Raw
import qualified Lampi.Lex as Raw
import qualified Lampi.Par as Raw
import qualified Lampi.Layout as Raw
import Control.Monad.Foil
import Data.Bifunctor.Sum (Sum (L2, R2))
import qualified Data.Map as Map
import Language.LSP.Protocol.Types
  ( SemanticTokenAbsolute(..)
  , SemanticTokenTypes(..)
  )
import LanguageServer.Lampi

type ASTAnn = Raw.BNFC'Position
type Sig = LambdaPiSig ASTAnn
type SomeScopeWithLampi = SomeScopeWithAST NameBinder Sig

buildAsts' :: String -> [SomeScopeWithLampi]
buildAsts' = 
  maybe [] terms
  . toAst
  . Raw.resolveLayout True 
  . Raw.tokens
  where
    toAst :: [Raw.Token] -> Maybe Raw.Program
    toAst ts = either (\_ -> Nothing) Just (Raw.pProgram ts)
    terms :: Raw.Program -> [SomeScopeWithLampi]
    terms = \case
      Raw.AProgram _ ts -> concatMap toLambdaPiCommand ts
    toLambdaPiCommand :: Raw.Command -> [SomeScopeWithLampi]
    toLambdaPiCommand = \case
      Raw.CommandCheck _ l r -> 
        [ SomeScopeWithAST scope (toLambdaPi scope env l)
        , SomeScopeWithAST scope (toLambdaPi scope env r)
        ]
      Raw.CommandCompute _ l r -> 
        [ SomeScopeWithAST scope (toLambdaPi scope env l)
        , SomeScopeWithAST scope (toLambdaPi scope env r)
        ]
      where
        scope = emptyScope
        env = Map.empty

instance Raw.HasPosition ASTAnn where
  hasPosition = id

mkToken :: Raw.HasPosition a 
    => a -> Int -> SemanticTokenTypes -> [SemanticTokenAbsolute]
mkToken ann len tokenTy = maybe [] (pure . build) (Raw.hasPosition ann)
  where
    build (x, y) = SemanticTokenAbsolute
      { _tokenType = tokenTy
      , _tokenModifiers = []
      , _startChar = fromIntegral $ y - 1
      , _line = fromIntegral $ x - 1
      , _length = fromIntegral len
      }

tokenizeLambdaPi 
    :: LambdaPiF ASTAnn ScopedASTTokens ASTTokens -> [SemanticTokenAbsolute]
tokenizeLambdaPi = \case
  AVarF a _ str -> mkToken a (length str) SemanticTokenTypes_Variable
  AppF _ x y -> x ++ y
  LamF a pt (_, bt) ->
    mkToken a (length "λ") SemanticTokenTypes_Keyword
      ++ pt
      ++ bt
  PiF a pt tt (_, bt) ->
    mkToken a (length "Π") SemanticTokenTypes_Keyword
      ++ pt
      ++ tt
      ++ bt
  UniverseF a -> mkToken a (length "𝕌") SemanticTokenTypes_Type
  PairF a lt rt ->
    mkToken a 1 SemanticTokenTypes_Keyword
      ++ lt
      ++ rt
  FirstF a t ->
    mkToken a (length "π₁") SemanticTokenTypes_Keyword
      ++ t
  SecondF a t ->
    mkToken a (length "π₂") SemanticTokenTypes_Keyword
      ++ t
  ProductF _ lt rt -> lt ++ rt
  PatternWildcardF a -> mkToken a 1 SemanticTokenTypes_Variable
  PatternVarF a str -> mkToken a (length str) SemanticTokenTypes_Variable
  PatternPairF a l r ->
    mkToken a 1 SemanticTokenTypes_Keyword
      ++ l
      ++ r

tokenizeError :: LambdaPiErrorF ASTAnn ScopedASTTokens ASTTokens -> [SemanticTokenAbsolute]
tokenizeError = \case
  UnboundSymF a str -> mkToken a (length str) SemanticTokenTypes_Variable
  UnsupportedF _ -> []

instance TokenizableSig Sig where
  tokenize = \case
    L2 f -> tokenizeLambdaPi f
    R2 f -> tokenizeError f
instance TokenizablePat NameBinder
instance HoverableSig Sig
instance HoverablePat NameBinder
instance DiagnosableSig Sig
instance DiagnosablePat NameBinder
instance RangedSig Sig
instance FoldablePat NameBinder

runLampiLS :: IO ()
runLampiLS = runLanguageServer LSConfiguration
  { fileExtension = "lampi"
  , buildAsts = buildAsts'
  }
