{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module AgnosticLanguageServer.AST
  ( findNarrowest
  , extractName
  , nodeCovers
  , patternNameRange
  , definitionRange
  , mentionedRanges
  , findDefinition
  , findDefinition'
  , findRefs
  , findRefs'
  , typecheck
  ) where

import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Control.Arrow (first)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Bifoldable (Bifoldable, bifoldMap, bifoldr)
import Data.Bifunctor (Bifunctor, bimap)
import Language.LSP.Protocol.Types (Position, Range)
import AgnosticLanguageServer.Types
import AgnosticLanguageServer.Classes
import AgnosticLanguageServer.Common (inRange)

findNarrowest ::
  ( Bifoldable sig
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder
  , F.Distinct n )
  => Position
  -> F.AST binder sig n
  -> Maybe (Either (SomePattern binder sig, SomeAST binder sig) (SomeAST binder sig))
findNarrowest = (fmap fst .) . findNarrowest'
  where
    findNarrowest' ::
      ( Bifoldable sig
      , RangedSig sig
      , F.CoSinkable binder
      , FoldablePat binder
      , F.Distinct n )
      => Position
      -> F.AST binder sig n
      -> Maybe (Either (SomePattern binder sig, SomeAST binder sig) (SomeAST binder sig), Range)
    findNarrowest' pos ast = do
      sig <- case ast of
        F.Var{} -> Nothing
        F.Node s -> Just s
      r <- range sig
      if not $ inRange pos r
        then Nothing
        else
          let narr = narrowest pos
          in bifoldr
            (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
              F.Distinct ->
                let narrPat = findNarrowestPat (SomeAST a) pos (SomePattern binder)
                    narrNode = findNarrowest' pos a
                in narr $ narr narrPat narrNode
              )
            (narr . findNarrowest' pos)
            (Just (Right $ SomeAST ast, r))
            sig
    narrowest pos = \case
      a@(Just (_, r)) -> if inRange pos r then const a else id
      _ -> id
    findNarrowestPat ast pos (SomePattern pat) = do
      r <- rangePat pat
      if not $ inRange pos r
        then Nothing
        else foldrPat
          (\p -> maybe (findNarrowestPat ast pos p) Just)
          (Just (Left (SomePattern pat, ast), r))
          pat

extractName :: (Bifoldable sig, F.Distinct n, F.CoSinkable binder)
  => F.AST binder sig n -> Maybe (SomeName binder sig)
extractName = \case
  F.Var{} -> Nothing
  F.Node n -> bifoldr extract' extract Nothing n
  where
    extract = \case
      F.Var n -> const (Just $ SomeName n)
      _ -> id
    extract' (F.ScopedAST binder t) = case F.assertDistinct binder of
      F.Distinct -> extract t

nodeCovers :: RangedSig sig
  => Position -> sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool
nodeCovers pos = maybe False (inRange pos) . range

patternNameRange :: FoldablePat binder
  => SomeName binder sig -> SomePattern binder sig -> Maybe Range
patternNameRange sn@(SomeName name') (SomePattern pat) = case binderOf pat of
  Just binder ->
    if F.nameId name' == F.nameId (F.nameOf binder)
      then rangePat pat
      else Nothing
  _ -> foldrPat (\p -> maybe (patternNameRange sn p) Just) Nothing pat

definitionRange ::
  ( Bifoldable sig
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
  => Position
  -> SomeScopeWithAST binder sig
  -> Maybe Range
definitionRange pos (SomeScopeWithAST scope ast) =
  findNarrowest pos ast >>= \case
    Left (SomePattern pat, _) -> rangePat pat
    Right (SomeAST node) -> do
      name' <- extractName node
      (pat, _) <- findDefinition (nodeCovers pos) name' scope ast
      patternNameRange name' pat

mentionedRanges ::
  ( Bifoldable sig
  , RangedSig sig
  , F.CoSinkable binder
  , FoldablePat binder )
  => Position
  -> SomeScopeWithAST binder sig
  -> [Range]
mentionedRanges pos (SomeScopeWithAST scope ast) = fromMaybe [] $ do
  narrowest <- findNarrowest pos ast
  (name', pat, definition) <- case narrowest of
    Left (sp@(SomePattern p), ast') -> fmap
      (\nb -> (SomeName $ F.nameOf nb, sp, ast'))
      (binderOf p)
    Right (SomeAST node) -> do
      name' <- extractName node
      (pat, definition) <- findDefinition (nodeCovers pos) name' scope ast
      Just (name', pat, definition)
  let kidsRanges = mapMaybe nameRange' (findRefs name' definition)
  Just
    $ maybe kidsRanges (:kidsRanges)
    $ patternNameRange name' pat
  where
    nameRange' :: RangedSig sig => SomeAST binder sig -> Maybe Range
    nameRange' (SomeAST ast') = case ast' of
      F.Var{} -> Nothing
      F.Node sig -> range sig

findDefinition ::
  ( F.Distinct n
  , Bifoldable sig
  , F.CoSinkable binder )
  => (forall m. sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool)
  -> SomeName binder sig
  -> F.Scope n
  -> F.AST binder sig n
  -> Maybe (SomePattern binder sig, SomeAST binder sig)
findDefinition f sn@(SomeName n) scope ast
  | n `F.member` scope = Nothing
  | otherwise = findDefinition' f sn scope ast

findDefinition' ::
  ( F.Distinct n
  , Bifoldable sig
  , F.CoSinkable binder )
  => (forall m. sig (F.ScopedAST binder sig m) (F.AST binder sig m) -> Bool)
  -> SomeName binder sig
  -> F.Scope n
  -> F.AST binder sig n
  -> Maybe (SomePattern binder sig, SomeAST binder sig)
findDefinition' isValid sn@(SomeName name') scope = \case
  F.Var{} -> Nothing
  F.Node sig -> bifoldr
    (firstJust . defScoped sig)
    (firstJust . findDefinition' isValid sn scope)
    Nothing
    sig
  where
    firstJust = maybe id (const . Just)
    defScoped sig (F.ScopedAST binder node') =
      case (F.assertDistinct binder, F.assertExt binder) of
        (F.Distinct, F.Ext) ->
          let scope' = F.extendScopePattern binder scope
              member = name' `F.member` scope'
              valid = case node' of
                F.Var{} -> isValid sig
                F.Node s -> isValid s
          in case (valid, member) of
            (False, _) -> Nothing
            (_, False) -> findDefinition' isValid sn scope' node'
            _ -> Just (SomePattern binder, SomeAST node')

findRefs :: (Bifoldable sig, F.CoSinkable binder)
  => SomeName binder sig
  -> SomeAST binder sig
  -> [SomeAST binder sig]
findRefs sn (SomeAST ast) = case ast of
  F.Var{} -> []
  F.Node sig -> bifoldMap
    (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
      F.Distinct -> findRefs' sn a)
    (findRefs' sn)
    sig

findRefs' :: (F.Distinct m, Bifoldable sig, F.CoSinkable binder)
  => SomeName binder sig -> F.AST binder sig m -> [SomeAST binder sig]
findRefs' sn ast =
  let grandKids = findRefs sn (SomeAST ast)
  in maybe grandKids (:grandKids) $ do
    name' <- extractName ast
    if namesEq sn name'
      then Just $ SomeAST ast
      else Nothing
  where
    namesEq (SomeName n) (SomeName n') = F.nameId n == F.nameId n'

typecheck ::
  ( Bifunctor sig
  , TypeDeductiveSig sig sig' ty binder
  , TypedSig sig' ty
  , TypedPat binder ty
  , F.Distinct n
  , F.CoSinkable binder )
  => F.NameMap n ty -> ty -> F.AST binder sig n -> F.AST binder sig' n
typecheck nm typ = \case
  F.Var n -> F.Var n
  F.Node sig -> F.Node $ deduceType typ $ bimap
    (\(F.ScopedAST binder a) -> case F.assertDistinct binder of
      F.Distinct ->
        let f = first (F.ScopedAST binder) . check (addPattern binder nm) a
        in (patTy binder, f)
      )
    (check nm)
    sig
  where
    astTy :: TypedSig sig ty => F.NameMap n ty -> F.AST binder sig n -> ty
    astTy nm' = \case
      F.Var n -> F.lookupName n nm'
      F.Node sig -> ty sig
    check ::
      ( Bifunctor sig
      , TypeDeductiveSig sig sig' ty binder
      , TypedSig sig' ty
      , TypedPat binder ty
      , F.Distinct n
      , F.CoSinkable binder )
      => F.NameMap n ty
      -> F.AST binder sig n
      -> ty
      -> (F.AST binder sig' n, ty)
    check nm' a t =
      let a' = typecheck nm' t a
      in (a', astTy nm' a')
