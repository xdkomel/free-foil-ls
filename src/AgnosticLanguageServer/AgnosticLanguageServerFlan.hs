{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE NamedFieldPuns                 #-}
{-# LANGUAGE QuantifiedConstraints                 #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE StandaloneDeriving                 #-}


module AgnosticLanguageServer.AgnosticLanguageServerFlan where

import Common.LanguageServerCache
import qualified Control.Monad.Foil.Relative as F
import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Control.Arrow 
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Language.LSP.Protocol.Types 
import Language.LSP.Protocol.Message 
import Language.LSP.Server 
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Monad.Reader
import Control.Monad (unless)
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes, isJust)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable
import Data.ZipMatchK.Generic (ZipMatchK)
import qualified Flan.Abs as R
import qualified Flan.Print as R
import qualified Flan.Lex as R
import qualified Flan.Par as R
import Common.Flan

import AgnosticLanguageServer.AgnosticLanguageServer 

type FlanAnn = R.BNFC'Position

buildAsts' :: FunBuildASTs F.NameBinders (FlanF FlanAnn)
buildAsts' =
  maybe [] terms
  . toAst
  . R.tokens
  where
    toAst ts = either (\_ -> Nothing) Just (R.pProgram ts)
    terms = \case
      R.AProgram _ t -> 
        [ SomeScopedAST scope (toFlan scope Map.empty t)
        ]
    scope = F.emptyScope

findNarrowest' :: FunFindNarrowest F.NameBinders (FlanF FlanAnn)
findNarrowest' pos@(x, y) (SomeScopedAST scope n) = case n of
  t@(Const pos' c) -> case c of
    ConstInt n -> pos' >>= ifMatches scope (show n) t
    ConstBool b -> pos' >>= ifMatches scope (show b) t
    ConstStr s -> pos' >>= ifMatches scope s t
  t@(AVar pos' _ nameStr) -> pos' >>= ifMatches scope nameStr t
  (App _ fun arg) -> 
    firstJust $ map (findNarrowest' pos . SomeScopedAST scope) [fun, arg]
  (Lam _ _ _ binder body) -> case (F.assertDistinct binder, F.assertExt binder) of 
    (F.Distinct, F.Ext) -> 
      let scope' = F.extendScopePattern binder scope
      in findNarrowest' pos (SomeScopedAST scope' body)
  (Let _ _ _ exp binder body) -> case (F.assertDistinct binder, F.assertExt binder) of 
    (F.Distinct, F.Ext) -> 
      let scope' = F.extendScopePattern binder scope
      in firstJust 
        [ findNarrowest' pos (SomeScopedAST scope exp)
        , findNarrowest' pos (SomeScopedAST scope' body)
        ]
  (Pair _ l r) -> 
    firstJust $ map (findNarrowest' pos . SomeScopedAST scope) [l, r]
  (If _ p x y) ->
    firstJust $ map (findNarrowest' pos . SomeScopedAST scope) [p, x, y]
  F.Var{} -> Nothing
  (FlanError _ _) -> Nothing
  where
    ifMatches scope nameStr node (line, col) = 
      if x == line && y >= col && y < col + length nameStr
        then Just $ SomeScopedAST scope node
        else Nothing

extractName' :: FunExtractName F.NameBinders (FlanF FlanAnn)
extractName' = \case
  (SomeScopedAST _ (AVar _ (F.Var name) _)) -> Just (SomeName name)
  _ -> Nothing

buildTelescopes' :: FunBuildTelescopes F.NameBinders (FlanF FlanAnn)
buildTelescopes' scope = \case
  t@AVar{} -> [LeafTerm t]
  (App _ fun arg) -> concatMap (buildTelescopes' scope) [fun, arg]
  t@(Lam _ _ _ binder body) -> case (F.assertDistinct binder, F.assertExt binder) of
    (F.Distinct, F.Ext) -> 
      let scope' = F.extendScopePattern binder scope
          telescopes = buildTelescopes' scope' body
      in [NodeTerm t binder telescopes]
  t@(Let _ _ _ exp binder body) -> case (F.assertDistinct binder, F.assertExt binder) of
    (F.Distinct, F.Ext) -> 
      let scope' = F.extendScopePattern binder scope
          telescopes = buildTelescopes' scope' body
      in [NodeTerm t binder telescopes]
      ++ buildTelescopes' scope exp 
  (Pair _ l r) -> concatMap (buildTelescopes' scope) [l, r]
  (If _ p x y) -> concatMap (buildTelescopes' scope) [p, x, y]
  _ -> []
  -- where
  --   build :: F.Distinct n 
  --     => F.Scope n 
  --     -> F.AST F.NameBinders (FlanF FlanAnn) n 
  --     -> [TermTelescope F.NameBinders (FlanF FlanAnn) n]
  --   build scope = 

findRange' :: FunRange F.NameBinders (FlanF FlanAnn)
findRange' (SomeScopedAST _ x) = case x of
  Let (Just (x, y)) pat _ _ _ _ -> toRange x y $ patternLength pat
  Lam (Just (x, y)) pat _ _ _ -> toRange x y $ patternLength pat
  AVar (Just (x, y)) _ nameStr -> toRange x y $ length nameStr
  _ -> Nothing
  where
    patternLength = \case
      PatternWildcard _ -> 1
      PatternVar _ x -> length x
      PatternPair _ l r -> (patternLength l) + (patternLength r) + 3
    toRange x y len = Just 
      $ Range (Position (pos x) (pos y)) (Position (pos x) (pos $ y + len))
    pos i = fromIntegral (i - 1)

printTerm' :: SomeScopedAST F.NameBinders (FlanF FlanAnn) -> String
printTerm' (SomeScopedAST _ x) = ppFlan Nothing x

-- instance AlphaEquiv (FlanType a) where
--   alphaEquiv _ a = \case
--     IntType _ -> case a of 
--       IntType _ -> True 
--       _ -> False
--     StrType _ -> case a of 
--       StrType _ -> True 
--       _ -> False
--     BoolType _ -> case a of 
--       BoolType _ -> True 
--       _ -> False
--     FunType _ x y -> case a of
--       FunType _ x' y' -> (alphaEquiv x x') && (alphaEquiv y y')
--       _ -> False
--     PairType _ l r -> case a of
--       PairType _ l' r' ->   (alphaEquiv l l') && (alphaEquiv r r')
--       _ -> False
--     _ -> False

runFlanLS :: IO ()
runFlanLS = runLanguageServer LSConfiguration
  { fileExtension = "flan"
  , buildAsts = buildAsts'
  , findNarrowest = findNarrowest'
  , extractName = extractName'
  , buildTelescopes = buildTelescopes'
  , findRange = findRange'
  , printTerm = printTerm'
  }


