{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Common.Flan where

import Control.Monad.Foil 
import Control.Monad.Free.Foil 
import Data.Bifunctor.TH
import Generics.Kind.TH (deriveGenericK)

data FunTypeAnn a = FunTypeAnn
  { funTypeDef :: a
  , funTypeArrow :: a 
  } deriving (Eq, Show)
data PairTypeAnn a = PairTypeAnn
  { pairTypeDef :: a
  , pairTypeComma :: a
  , pairTypeClosingBracket :: a 
  } deriving (Eq, Show)

data FlanType a
  = IntType a
  | StrType a
  | BoolType a
  | FunType (FunTypeAnn a) (FlanType a) (FlanType a)
  | PairType (PairTypeAnn a) (FlanType a) (FlanType a)
  | UnknownType a String
  | AnyType a
  deriving Eq

instance Show (FlanType a) where
  show = \case
    IntType _ -> "Int"
    StrType _ -> "Str"
    BoolType _ -> "Bool"
    FunType _ l r -> "(" ++ show l ++ ") -> " ++ show r 
    PairType _ l r -> "[" ++ show l ++ ", " ++ show r ++ "]"
    UnknownType _ s -> s
    AnyType _ -> "_"

data PatternPairAnn a = PatternPairAnn
  { patPairDef :: a
  , patPairComma :: a
  , patPairClosingBracket :: a 
  } deriving Eq

data FlanPattern a ty (n :: S) (l :: S) where
  PatternWildcard :: a -> a -> ty -> FlanPattern a ty n n
  PatternVar :: a -> a -> ty -> String -> NameBinder n l -> FlanPattern a ty n l
  PatternPair :: PatternPairAnn a -> a -> ty -> FlanPattern a ty n i -> FlanPattern a ty i l -> FlanPattern a ty n l

instance CoSinkable (FlanPattern a ty) where
  coSinkabilityProof f pat cont = case pat of
    PatternWildcard a a' ty -> cont f $ PatternWildcard a a' ty
    PatternVar a a' ty str binder -> 
      coSinkabilityProof f binder $ \f' binder' ->
        cont f' $ PatternVar a a' ty str binder'
    PatternPair a a' ty l r -> 
      coSinkabilityProof f l $ \f' l' ->
        coSinkabilityProof f' r $ \f'' r' ->
          cont f'' $ PatternPair a a' ty l' r'

  withPattern withBinder unit comp scope pat cont = case pat of
    PatternWildcard a a' ty -> cont unit $ PatternWildcard a a' ty
    PatternVar a a' ty str binder -> withBinder scope binder $ \f binder' ->
      cont (comp unit f) $ PatternVar a a' ty str binder'
    PatternPair a a' ty l r -> withPattern withBinder unit comp scope l $ \f l' ->
      let scope' = extendScopePattern l' scope
      in withPattern withBinder unit comp scope' r $ \f' r' ->
        cont (comp f f') $ PatternPair a a' ty l' r'

data FlanConst
  = ConstInt Integer
  | ConstStr String
  | ConstBool Bool

instance Show FlanConst where
  show = \case
    ConstInt i -> show i
    ConstStr s -> "\"" ++ s ++ "\""
    ConstBool b -> show b

data LamAnn a = LamAnn
  { lamDef :: a
  , lamReturn :: a 
  } deriving (Eq, Show)
data LetAnn a = LetAnn
  { letDef :: a
  , letEq :: a
  , letIn :: a 
  } deriving (Eq, Show)
data PairAnn a = PairAnn
  { pairDef :: a
  , pairComma :: a
  , pairClosingBracket :: a 
  } deriving (Eq, Show)
data IfAnn a = IfAnn
  { ifPred :: a 
  , ifThen :: a
  , ifElse :: a 
  } deriving (Eq, Show)

data FlanF a a' s t 
  = VarF a a' t String
  | AppF a a' t t
  | LamF (LamAnn a) a' s
  | LetF (LetAnn a) a' s s
  | PairF (PairAnn a) a' t t
  | IfF (IfAnn a) a' t t t
  | ConstF a a' FlanConst
  | ErrorF a a' String
  deriving (Functor, Foldable, Traversable)
deriveBifunctor ''FlanF
deriveBifoldable ''FlanF
deriveBitraversable ''FlanF
deriveGenericK ''FlanF

type Flan a a' = AST (FlanPattern a (FlanType a)) (FlanF a a')

pattern AVar :: a -> a' -> Flan a a' n -> String -> Flan a a' n
pattern AVar a a' t nameStr = Node (VarF a a' t nameStr)

pattern App :: a -> a' -> Flan a a' n -> Flan a a' n -> Flan a a' n
pattern App a a' fun arg = Node (AppF a a' fun arg)

pattern Lam :: LamAnn a -> a' -> FlanPattern a (FlanType a) n l -> Flan a a' l -> Flan a a' n
pattern Lam a a' binder body = Node (LamF a a' (ScopedAST binder body))

pattern Let :: LetAnn a -> a' -> FlanPattern a (FlanType a) n l -> Flan a a' l -> FlanPattern a (FlanType a) n m -> Flan a a' m -> Flan a a' n
pattern Let a a' binderExp exp binderBody body = Node (LetF a a' (ScopedAST binderExp exp) (ScopedAST binderBody body))

pattern Pair :: PairAnn a -> a' -> Flan a a' n -> Flan a a' n -> Flan a a' n
pattern Pair a a' x y = Node (PairF a a' x y)

pattern If :: IfAnn a -> a' -> Flan a a' n -> Flan a a' n -> Flan a a' n -> Flan a a' n
pattern If a a' pred x y = Node (IfF a a' pred x y)

pattern Const :: a -> a'  -> FlanConst -> Flan a a' n
pattern Const a a' c = Node (ConstF a a' c)

pattern FlanError :: a -> a' -> String -> Flan a a' n
pattern FlanError a a' s = Node (ErrorF a a' s)

{-# COMPLETE Var, AVar, App, Lam, Let, Pair, If, Const, FlanError #-}
