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

module Common.Flan where

import Control.Monad.Foil 
import Control.Monad.Free.Foil 
import Data.Bifunctor.Sum
import           Data.Bifunctor.TH
import           Data.Map (Map)
import qualified Data.Map                        as Map
import qualified Data.IntSet as IntSet
import           Generics.Kind.TH                (deriveGenericK)
import qualified Flan.Abs as R
import qualified Flan.Print as R
import Data.Coerce (coerce)

data FlanType a
  = IntType a
  | StrType a
  | BoolType a
  | FunType a (FlanType a) (FlanType a) 
  | PairType a (FlanType a) (FlanType a)
  | UnknownType a String
  deriving (Eq, Show, Functor, Foldable, Traversable)

data FlanPattern a 
  = PatternWildcard a
  | PatternVar a String
  | PatternPair a (FlanPattern a) (FlanPattern a)
  deriving (Eq, Show)

type PatternType a = Maybe (FlanType a)

data FlanConst
  = ConstInt Integer
  | ConstStr String
  | ConstBool Bool

data FlanF a s t 
  = VarF a t String
  | AppF a t t
  | LamF a (FlanPattern a) (PatternType a) s
  | LetF a (FlanPattern a) (PatternType a) t s
  | PairF a t t
  | IfF a t t t
  | ConstF a FlanConst
  | ErrorF a String
  deriving (Functor, Foldable, Traversable)
deriveBifunctor ''FlanF
deriveBifoldable ''FlanF
deriveBitraversable ''FlanF
deriveGenericK ''FlanF

-- newtype FlanPattern a n l = FlanPattern a (FlanPat a n l) (Maybe (FlanType a))

-- type FlanSig a s t = FlanF a s t
type Flan a = AST NameBinders (FlanF a)

pattern AVar :: a -> Flan a n -> String -> Flan a n
pattern AVar a t nameStr = Node (VarF a t nameStr)

pattern App :: a -> Flan a n -> Flan a n -> Flan a n
pattern App a fun arg = Node (AppF a fun arg)

pattern Lam :: a -> FlanPattern a -> PatternType a -> NameBinders n l -> Flan a l -> Flan a n
pattern Lam a pat patType binder body = Node (LamF a pat patType (ScopedAST binder body))

pattern Let :: a -> FlanPattern a -> PatternType a -> Flan a n -> NameBinders n l -> Flan a l -> Flan a n
pattern Let a pat patType exp binder body = Node (LetF a pat patType exp (ScopedAST binder body))

pattern Pair :: a -> Flan a n -> Flan a n -> Flan a n
pattern Pair a x y = Node (PairF a x y)

pattern If :: a -> Flan a n -> Flan a n -> Flan a n -> Flan a n
pattern If a pred x y = Node (IfF a pred x y)

pattern Const :: a  -> FlanConst -> Flan a n
pattern Const a c = Node (ConstF a c)

pattern FlanError :: a -> String -> Flan a n
pattern FlanError a s = Node (ErrorF a s)

{-# COMPLETE Var, AVar, App, Lam, Let, Pair, If, Const, FlanError #-}

toFlanType :: R.FlanType' a -> FlanType a
toFlanType = \case
  R.ValType a (R.VarIdent x) -> case x of
    "Int" -> IntType a
    "Str" -> StrType a
    "Bool" -> BoolType a
    _ -> UnknownType a x
  R.FunType a x y -> FunType a (toFlanType x) (toFlanType y)
  R.PairType a x y -> PairType a (toFlanType x) (toFlanType y)

toFlanPattern :: Distinct n
  => Scope n
  -> Map String (Name n)
  -> R.TypedPattern' a
  -> (forall l. DExt n l 
    => FlanPattern a 
    -> PatternType a 
    -> NameBinders n l 
    -> Map String (Name l) 
    -> r)
  -> r
toFlanPattern scope env pat cont = case pat of 
  R.UntypedPattern _ p -> patternBinders scope env p 
    $ cont (patternStruct p) Nothing
  R.ATypedPattern _ p ty -> patternBinders scope env p 
    $ cont (patternStruct p) (Just $ toFlanType ty)
  where
    patternStruct = \case
      R.PatternWildcard a -> PatternWildcard a
      R.PatternVar a (R.VarIdent x) -> PatternVar a x
      R.PatternPair a l r -> PatternPair a (patternStruct l) (patternStruct r)

patternBinders :: Distinct n
  => Scope n
  -> Map String (Name n)
  -> R.Pattern' a
  -> (forall l . DExt n l => NameBinders n l -> Map String (Name l) -> r)
  -> r
patternBinders scope env pat cont = case pat of
  R.PatternWildcard _ -> cont (coerce emptyNameBinders) env
  R.PatternVar _ (R.VarIdent x) -> withFresh scope $ \binder ->
    cont (nameBindersSingleton binder) (Map.insert x (nameOf binder) (sink <$> env))
  R.PatternPair _ l r ->
    patternBinders scope env l $ \lBinders env' ->
      let scope' = extendScopePattern lBinders scope
      in patternBinders scope' env' r $ \rBinders env'' ->
        cont (mergeNameBinders lBinders rBinders) env''

bindingToFlan :: Distinct n
  => Scope n 
  -> Map String (Name n) 
  -> R.Binding' a 
  -> (forall l . DExt n l => Scope l -> Map String (Name l) -> Flan a l)
  -> Flan a n
bindingToFlan scope env binding cont = case binding of
  R.LetBinding a pat exp -> toFlanPattern scope env pat 
    $ \flanPat patType binder env' ->
      let scope' = extendScopePattern binder scope
      in Let a flanPat patType (toFlan scope env exp) binder (cont scope' env')

letToFlan :: Distinct n 
  => Scope n 
  -> Map String (Name n) 
  -> [R.Binding' a]
  -> R.Term' a
  -> Flan a n
letToFlan scope env [] body = toFlan scope env body
letToFlan scope env (b:bs) body = bindingToFlan scope env b 
  $ \scope' env' -> letToFlan scope' env' bs body 

toFlan :: Distinct n 
  => Scope n 
  -> Map String (Name n) 
  -> R.Term' a 
  -> Flan a n
toFlan scope env = \case
  R.ConstFalse a -> Const a (ConstBool False)
  R.ConstTrue a -> Const a (ConstBool True)
  R.ConstInt a n -> Const a (ConstInt n)
  R.ConstStr a s -> Const a (ConstStr s)
  R.Var a (R.VarIdent x) -> 
    case Map.lookup x env of
      Just name -> AVar a (Var name) x
      Nothing -> FlanError a ("[]" ++ x ++ " is unbound]")
  R.App a fun arg -> App a (toFlan scope env fun) (toFlan scope env arg)
  R.Pair a x y -> Node (PairF a (toFlan scope env x) (toFlan scope env y))
    -- Pair a (toFlan scope env x) (toFlan scope env y)
  R.If a pred x y -> If a (toFlan scope env pred) (toFlan scope env x) (toFlan scope env y)
  R.Lam a pat body -> toFlanPattern scope env pat 
    $ \flanPat patType binder env' ->
      let scope' = extendScopePattern binder scope
      in Lam a flanPat patType binder (toFlan scope' env' body)
  R.Let _ bindings body -> letToFlan scope env bindings body

patternFromFlan :: a -> FlanPattern a -> PatternType a -> R.TypedPattern' a
patternFromFlan a pat ty = 
  let pat' = toRawPattern pat
  in maybe (R.UntypedPattern a pat') (R.ATypedPattern a pat' . toRawType) ty
  where
    toRawPattern = \case
      PatternWildcard a -> R.PatternWildcard a
      PatternVar a n -> R.PatternVar a (R.VarIdent n)
      PatternPair a l r -> R.PatternPair a (toRawPattern l) (toRawPattern r)
    toRawType = \case
      IntType a -> R.ValType a (R.VarIdent "Int")
      StrType a -> R.ValType a (R.VarIdent "Str")
      BoolType a -> R.ValType a (R.VarIdent "Bool")
      FunType a x y -> R.FunType a (toRawType x) (toRawType y)
      PairType a l r -> R.PairType a (toRawType l) (toRawType r)
      UnknownType a x -> R.ValType a (R.VarIdent $ "[Unknown-" ++ x ++ "]")

fromFlan :: a
  -> Flan a n
  -> R.Term' a
fromFlan errAnn = \case
  AVar a _ n -> R.Var a (R.VarIdent n)
  Var name -> R.Var errAnn (ppName name)
  App a fun arg -> R.App a (fromFlan errAnn fun) (fromFlan errAnn arg)
  Pair a l r -> R.Pair a (fromFlan errAnn l) (fromFlan errAnn r)
  If a p x y -> R.If a (fromFlan errAnn p) (fromFlan errAnn x) (fromFlan errAnn y)
  Lam a flanPat patType _ body -> 
    R.Lam a (patternFromFlan errAnn flanPat patType) (fromFlan errAnn body)
  Let a flanPat patType exp _ body ->
    let binding = R.LetBinding a 
          (patternFromFlan errAnn flanPat patType) 
          (fromFlan errAnn exp)
        body' = fromFlan errAnn body
    in R.Let errAnn [binding] body'
  Const a c -> case c of
    ConstInt n -> R.ConstInt a n
    ConstBool b -> if b then (R.ConstTrue a) else (R.ConstFalse a)
    ConstStr s -> R.ConstStr a s
  FlanError a desc -> R.Var a (R.VarIdent $ "[E-Var: " ++ desc ++ "]")
  where
    ppName name = R.VarIdent ("x" ++ show (nameId name))

ppFlan :: a -> Flan a n -> String
ppFlan errAnn = R.printTree . (fromFlan errAnn)

showFlan :: Flan R.BNFC'Position n -> String
showFlan = show . (fromFlan Nothing)
