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

module AgnosticFoilLSAlt.AgnosticFoilLSAlt where

import Common.LanguageServerCache
-- import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Relative as F
import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import Control.Arrow 
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
-- import Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
-- import Language.LSP.Server as LSP
-- import Language.LSP.Protocol.Lens as LSP
import Control.Monad.Reader
import Control.Monad (unless)
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes, isJust)
-- import Unsafe.Coerce (coerce)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable
import Data.ZipMatchK.Generic (ZipMatchK)

data SomeTerm binder sig where
  SomeTerm :: F.Distinct n => F.AST binder sig n -> SomeTerm binder sig

data SomeName binder sig where
  SomeName :: F.Distinct n => F.Name n -> SomeName binder sig

data TermTelescope binder sig n where
  LeafTerm :: F.Distinct n 
    => F.AST binder sig n 
    -> TermTelescope binder sig n
  NodeTerm :: F.DExt n l 
    => F.AST binder sig n 
    -> F.NameBinder n l 
    -> [TermTelescope binder sig l]
    -> TermTelescope binder sig n

type FunFindNarrowest binder sig = (Int, Int) -> SomeTerm binder sig -> Maybe (SomeTerm binder sig)

type FunExtractName binder sig = SomeTerm binder sig -> Maybe (SomeName binder sig)

type FunBuildTelescopes binder sig n = F.Distinct n => F.AST binder sig n -> [TermTelescope binder sig n]

type FunRange binder sig = SomeTerm binder sig -> Maybe LSP.Range

symbolRange :: F.Distinct n => FunFindNarrowest binder sig -> FunRange binder sig -> (Int, Int) -> F.AST binder sig n -> Maybe LSP.Range
symbolRange fFind fRange pos ast = do
  narrowest <- fFind pos (SomeTerm ast)
  fRange narrowest

definitionRange :: F.Distinct n
  => FunFindNarrowest binder sig 
  -> FunExtractName binder sig
  -> FunRange binder sig 
  -> FunBuildTelescopes binder sig n
  -> F.Scope n
  -> (Int, Int) 
  -> F.AST binder sig n
  -> Maybe LSP.Range
definitionRange fFind fExtName fRange fBuildTele teleScope pos ast = do
  narrowest <- fFind pos (SomeTerm ast)
  name' <- fExtName narrowest
  let telescopes = fBuildTele ast
      -- usages = definingTerm termIsValid F.emptyScope name' telescope
      -- usages :: [Maybe (SomeTerm binder sig)]
      usages = 
        -- definingTerms termIsValid name' telescopes
        -- [definingTerm termIsValid F.emptyScope name' t | t <- telescopes]
        map (definingTerm termIsValid teleScope name') telescopes
  usage <- firstJust usages
  fRange usage
  where
    termIsValid = isJust . fFind pos

traces :: 
  ( Bitraversable sig
  , AlphaEquiv ty
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) 
  => ty F.VoidS
  -> F.AST binder sig F.VoidS 
  -> String
traces ty ast = 
  let typecheckRes = bidirectionalCheck F.emptyNameMap ast ty
  in either (\m -> "The message is " ++ m) (\_ -> "Successful!") typecheckRes

-- traces :: F.Distinct n 
--   => FunFindNarrowest binder sig n 
--   -> FunExtractName binder sig
--   -> FunBuildTelescopes binder sig n
--   -> (SomeTerm binder sig -> String)
--   -> (Int, Int) 
--   -> F.AST binder sig n
--   -> String
-- traces fFind fExt fBuildTele pp pos ast = 
--   let narrowest = fFind pos ast
--       name' = narrowest >>= fExt 
--       telescopes = fBuildTele ast
--       families = (\name'' -> catMaybes $ 
--           map (mentions fExt F.emptyScope name'') telescopes) 
--         <$> name'
--   in maybe 
--     "Name was not found!" 
--     (concatMap ((++ "\n\n") . (\f -> (pp $ parent f) ++ " => " ++ (show $ map pp $ children f))))
--     families

printTele :: F.Distinct n => (SomeTerm binder sig -> String) -> TermTelescope binder sig n -> String
printTele pp (LeafTerm a) = pp (SomeTerm a)
printTele pp (NodeTerm a binder ls) = pp (SomeTerm a) ++ " >> " ++ show binder ++ " >> " ++ (show $ map (printTele pp) ls) ++ ""

-- TODO: Find the term usage history to rename all locations where it was mentioned
mentionedRanges :: F.Distinct n
  => FunFindNarrowest binder sig 
  -> FunExtractName binder sig
  -> FunRange binder sig 
  -> FunBuildTelescopes binder sig n
  -> F.Scope n
  -> (Int, Int) 
  -> F.AST binder sig n
  -> [LSP.Range]
mentionedRanges fFind fExtName fRange fBuildTele teleScope pos ast =
  let narrowest = fFind pos (SomeTerm ast)
      name' = narrowest >>= fExtName 
      telescopes = fBuildTele ast
      usages = (\name'' -> 
          catMaybes $ 
            map (definingTerm termIsValid teleScope name'') telescopes
        ) <$> name' 
  in maybe [] (catMaybes . map fRange) usages
  where
    termIsValid = isJust . fFind pos

definingTerm :: F.Distinct n
  => (SomeTerm binder sig -> Bool) 
  -> F.Scope n 
  -> SomeName binder sig 
  -> TermTelescope binder sig n
  -> Maybe (SomeTerm binder sig)
definingTerm _ scope (SomeName n) _ | n `F.member` scope = Nothing
definingTerm _ _ _ (LeafTerm{}) = Nothing
definingTerm _ _ _ (NodeTerm _ _ []) = Nothing
definingTerm leafIsValid scope sn@(SomeName n) (NodeTerm term binder (scoped:siblings)) = 
  let scope' = F.extendScopePattern binder scope
      isValid = case scoped of
        LeafTerm a -> leafIsValid $ SomeTerm a
        _ -> False
  in if isValid && n `F.member` scope'
    then Just (SomeTerm term)
    else case definingTerm leafIsValid scope' sn scoped of
      Nothing -> definingTerm leafIsValid scope sn (NodeTerm term binder siblings)
      just@Just{} -> just

data Family binder sig = Family
  { parent :: SomeTerm binder sig
  , children :: [SomeTerm binder sig]
  , name :: SomeName binder sig
  }
  
  -- (SomeTerm binder sig, [SomeTerm binder sig])

mentionsChildren :: F.Distinct n
  => [SomeTerm binder sig]
  -> FunExtractName binder sig
  -> SomeName binder sig 
  -> TermTelescope binder sig n
  -> [SomeTerm binder sig]
mentionsChildren children fExtName sn (LeafTerm a) 
  | maybe False (namesEq sn) $ fExtName (SomeTerm a) = children ++ [SomeTerm a]
  where
    namesEq (SomeName n1) (SomeName n2) = n1 == (coerce n2)
mentionsChildren children _ _ LeafTerm{} = children
mentionsChildren children _ _ (NodeTerm _ _ []) = children
mentionsChildren children extName name' (NodeTerm t binder (child:siblings)) =
  let deep = mentionsChildren children extName name' child
      breadth = mentionsChildren children extName name' (NodeTerm t binder siblings)
  in deep ++ breadth

mentions :: F.Distinct n
  => FunExtractName binder sig
  -> F.Scope n 
  -> SomeName binder sig 
  -> TermTelescope binder sig n
  -> Maybe (Family binder sig)
mentions _ scope sn@(SomeName n) (NodeTerm t binder []) = 
  let scope' = F.extendScopePattern binder scope
  in if n `F.member` scope' 
    then Just Family { parent = SomeTerm t, children = [], name = sn }
    else Nothing
mentions extName scope sn@(SomeName n) tt@(NodeTerm t binder (child:siblings)) =
  let scope' = F.extendScopePattern binder scope
  in if n `F.member` scope' 
    then Just Family 
      { parent = SomeTerm t
      , children = mentionsChildren [] extName sn tt
      , name = sn
      }
    else 
      let deepMentions = mentions extName scope' sn child
          breadthMentions = mentions extName scope sn (NodeTerm t binder siblings)
      in firstJust [deepMentions, breadthMentions]
mentions _ _ _ _ = Nothing     

-- mentions :: F.Distinct n
--   => Family binder sig
--   -> F.Scope n 
--   -> SomeName binder sig 
--   -> TermTelescope binder sig n
--   -> Family binder sig
-- mentions (parent, children) scope (SomeName n) (LeafTerm a) | n `F.member` scope = 
--   (parent, children ++ [SomeTerm a])
-- mentions (Nothing{}, children) scope sn@(SomeName n) (NodeTerm t binder (child:siblings)) =
--   let scope' = F.extendScopePattern binder scope
--       parent = if n `F.member` scope' then Just (SomeTerm t) else Nothing
--       deepMentions = mentions (parent, children) scope' sn child
--       breadthMentions = mentions (parent, children) scope sn (NodeTerm t binder siblings)
--   in 

-- mentions :: F.Distinct n
--   => [SomeTerm binder sig]
--   -> F.Scope n 
--   -> SomeName binder sig 
--   -> TermTelescope binder sig n
--   -> [SomeTerm binder sig]
-- mentions acc scope (SomeName n) (LeafTerm a) | n `F.member` scope = 
--   acc ++ [SomeTerm a]
-- mentions l scope (SomeName n) (NodeTerm term binder (scoped:siblings)) =
--   let scope' = F.extendScopePattern binder scope
--       isValid = case scoped of
--         LeafTerm a -> leafIsValid $ SomeTerm a
--         _ -> False
--   in if isValid && n `F.member` scope'
--     then Just (SomeTerm term)
--     else case definingTerm leafIsValid scope' sn scoped of
--       Nothing -> definingTerm leafIsValid scope sn (NodeTerm term binder siblings)
--       j@Just{} -> j

-- mentions l v scope sn@(SomeName n) tt | n `F.member` scope = case tt of
--   NodeTerm _ binder children -> 
--     let scope' = F.extendScopePattern binder scope
--     in l ++ concatMap (mentions l v scope' sn) children
--   LeafTerm a -> if leafIsValid $ SomeTerm a
--     then 
-- mentions l _ _ _ (LeafTerm{}) = l
-- mentions l _ _ _ (NodeTerm _ _ []) = l
-- mentions l leafIsValid scope sn@(SomeName n) (NodeTerm term binder (scoped:siblings)) = 
--   let scope' = F.extendScopePattern binder scope
--       isValid = case scoped of
--         LeafTerm a -> leafIsValid $ SomeTerm a
--         _ -> False
--   in if isValid && n `F.member` scope'
--     then Just (SomeTerm term)
--     else case mentions leafIsValid scope' sn scoped of
--       Nothing -> mentions leafIsValid scope sn (NodeTerm term binder siblings)
--       j@Just{} -> j

firstJust :: [Maybe a] -> Maybe a
firstJust (j@(Just _):_) = j
firstJust (_:t) = firstJust t
firstJust [] = Nothing

-- Typechecking

--------------------------------------------------------------------------------

-- * Core Types

--------------------------------------------------------------------------------

-- | Typing context mapping names to types
type Context' ty n = F.NameMap n (ty n)

-- | Scoped value with a binder
data Scoped binder (t :: F.S -> Type) (n :: F.S) where
  Scoped :: binder n l -> t l -> Scoped binder t n

-- | Type errors
data TypeError ty
  = TypeErrorUnexpectedType ty ty
  | TypeErrorUnexpectedDependentType
  deriving (Show)

-- | Bidirectional type checking result
data CheckInfer term ty (n :: F.S) = CheckInfer
  { check :: ty n -> Either String (),
    infer :: Either String (ty n),
    getTerm :: term n
  }

-- | Type for scoped checking/inference
type ScopedCheckInfer term binder ty (n :: F.S) =
  Maybe (ty n) -> CheckInfer (Scoped binder term) (Scoped binder ty) n

-- | Typed name binders data structure
data TypedNameBinders ty n l where
  TypedNameBindersEmpty :: TypedNameBinders ty n n
  TypedNameBindersCons ::
    F.NameBinder n i -> ty n -> TypedNameBinders ty i l -> TypedNameBinders ty n l

--------------------------------------------------------------------------------

-- * Required Type Classes

--------------------------------------------------------------------------------

-- | Alpha equivalence class
class AlphaEquiv t where
  alphaEquiv :: (F.Distinct n) => F.Scope n -> t n -> t n -> Bool

-- | Default instance for Free Foil ASTs
instance
  (Bifunctor sig, Bifoldable sig, ZipMatchK sig, F.UnifiablePattern binder) =>
  AlphaEquiv (F.AST binder sig)
  where
  alphaEquiv = alphaEquiv

-- | Main typing signature class
class
  (forall n. Show (TypeError (ty n)), AlphaEquiv ty) =>
  TypingSig binder ty sig
  where
  checkSig ::
    (F.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
    ty n ->
    Either String ()
  checkSig = defaultCheckSig

  inferSig ::
    (F.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
    Either String (ty n)

-- | Class for typed patterns
class TypedPattern ty pat where
  extractPatternType :: pat n l -> Maybe (ty n)
  extractTypedBinders :: pat n l -> ty n -> TypedNameBinders ty n l

-- | Class for creating trivially scoped values
class HasTrivialBinder binder where
  triviallyScoped ::
    (F.Distinct n, F.Sinkable ty) =>
    F.Scope n ->
    ty n ->
    Scoped binder ty n

instance HasTrivialBinder F.NameBinder where
  triviallyScoped scope type_ =
    F.withFresh scope $ \binder ->
      Scoped binder (F.sink type_)

--------------------------------------------------------------------------------

-- * Generic Utilities

--------------------------------------------------------------------------------

nameMapToScope :: F.NameMap n a -> F.Scope n
nameMapToScope (F.NameMap m) = F.UnsafeScope (IntMap.keysSet m)
-- deriving instance Functor (F.NameMap n)

-- | Check if actual type matches expected type
shouldBe ::
  (AlphaEquiv ty, F.Distinct n, Show (ty n)) =>
  (F.NameMap n (ty n), ty n) ->
  ty n ->
  Either String ()
shouldBe (scope, actualType) expectedType
  | sameType = return ()
  | otherwise =
      Left $
        unlines
          [ "expected type"
          , "  " ++ show expectedType
          , "but got type"
          , "  " ++ show actualType
          , "when typechecking expression"
          -- ,  "  " ++ show e
          ]
  where
    sameType = alphaEquiv (nameMapToScope scope) actualType expectedType

-- | Default implementation of checkSig
defaultCheckSig ::
  (F.Distinct n, TypingSig binder ty sig) =>
  Context' ty n ->
  sig (ScopedCheckInfer (F.AST binder sig) binder ty n) (CheckInfer (F.AST binder sig) ty n) ->
  ty n ->
  Either String ()
defaultCheckSig ctx node expectedType = do
  inferredType <- inferSig ctx node
  unless (alphaEquiv (nameMapToScope ctx) inferredType expectedType) $
    Left (show (TypeErrorUnexpectedType inferredType expectedType))

-- | Extract type from a binder
extractTypeFromBinder ::
  (TypedPattern ty binder, AlphaEquiv ty, F.Distinct n) =>
  Context' ty n ->
  binder n l ->
  Maybe (ty n) ->
  Either String (ty n)
extractTypeFromBinder _scope binder Nothing =
  maybe (Left "cannot infer without type annotation for pattern") Right $
    extractPatternType binder
extractTypeFromBinder scope binder (Just ty) =
  maybe
    (Right ty)
    ( \binderTy ->
        if alphaEquiv (nameMapToScope scope) binderTy ty
          then Right ty
          else Left "type mismatch"
    )
    $ extractPatternType binder

--------------------------------------------------------------------------------

-- * Main Bidirectional Type Checking API

--------------------------------------------------------------------------------

-- | Check a term against an expected type
bidirectionalCheck ::
  ( F.Distinct n
  , Bitraversable sig
  , AlphaEquiv ty
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  ty n ->
  Either String ()
bidirectionalCheck scope t expectedType = do
  ci <- bidirectionalCheckInfer scope t
  check ci expectedType

-- | Infer the type of a term
bidirectionalInfer ::
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  Either String (ty n)
bidirectionalInfer scope t = do
  ci <- bidirectionalCheckInfer scope t
  infer ci

-- | Combined check/infer for a term
bidirectionalCheckInfer ::
  forall ty binder sig n.
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.AST binder sig n ->
  Either String (CheckInfer (F.AST binder sig) ty n)

-- Variable case
bidirectionalCheckInfer scope t@(F.Var n) = do
  let inferredType = F.lookupName n scope
  return
    CheckInfer
      { infer = return inferredType,
        check = \expectedType -> do
          unless (alphaEquiv (nameMapToScope scope) inferredType expectedType) $
            Left (show (TypeErrorUnexpectedType inferredType expectedType)),
        getTerm = t
      }

-- Node case
bidirectionalCheckInfer scope (F.Node node) = do
  node' <-
    bitraverse
      (bidirectionalCheckInferScoped scope)
      (bidirectionalCheckInfer scope)
      node
  return
    CheckInfer
      { infer = inferSig scope node',
        check = checkSig scope node',
        getTerm = F.Node node
      }

extractExactlyOneBinder :: TypedPattern ty pat => pat n l -> ty n -> F.NameBinder n l
extractExactlyOneBinder binder ty = 
  case extractTypedBinders binder ty of
    TypedNameBindersCons extractedBinder _ty TypedNameBindersEmpty -> extractedBinder
    _ -> error "Expected exactly one binder"

-- | Bidirectional check/infer for scoped terms
bidirectionalCheckInferScoped ::
  ( F.Distinct n
  , Bitraversable sig
  , TypingSig binder ty sig
  , F.UnifiablePattern binder
  , F.Sinkable ty
  , TypedPattern ty binder
  , F.SinkableK binder
  ) =>
  Context' ty n ->
  F.ScopedAST binder sig n ->
  Either String (ScopedCheckInfer (F.AST binder sig) binder ty n)
bidirectionalCheckInferScoped scope (F.ScopedAST binder body) =
  case (F.assertExt binder, F.assertDistinct binder) of
    (F.Ext, F.Distinct) -> return $ \mbinderType ->
      CheckInfer
        { infer = do
            ty <- extractTypeFromBinder scope binder mbinderType
            let scope' = F.sink <$> F.addNameBinder (extractExactlyOneBinder binder ty) ty scope
            ci <- bidirectionalCheckInfer scope' body
            Scoped binder <$> infer ci,
          check = \(Scoped binder' expectedType) -> do
            -- TODO: check binder' against binder
            ty <- extractTypeFromBinder scope binder mbinderType
            case F.unifyPatterns binder binder' of
              F.SameNameBinders _binders -> do
                let scope' =
                      F.sink <$> F.addNameBinder (extractExactlyOneBinder binder ty) ty scope
                ci <- bidirectionalCheckInfer scope' body
                check ci expectedType
              F.RenameLeftNameBinder _binders renameL ->
                case (F.assertExt binder', F.assertDistinct binder') of
                  (F.Ext, F.Distinct) -> do
                    let scope' =
                          F.sink <$> F.addNameBinder (extractExactlyOneBinder binder' ty) ty scope
                        body' =
                          F.liftRM
                            (nameMapToScope scope')
                            (F.fromNameBinderRenaming renameL)
                            body
                    ci <- bidirectionalCheckInfer scope' body'
                    check ci expectedType

              -- FIXME: RenameRightNameBinder, RenameBothNameBinders
              _ -> Left "non-unifiable patterns",
          getTerm = Scoped binder body
        }

