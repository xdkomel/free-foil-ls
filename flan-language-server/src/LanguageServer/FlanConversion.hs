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
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE ImpredicativeTypes                 #-}
{-# LANGUAGE QuantifiedConstraints                 #-}

module LanguageServer.FlanConversion where

import Control.Monad.Foil 
import Control.Monad.Free.Foil 
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Generics.Kind.TH                (deriveGenericK)
import qualified Flan.Abs as R
import qualified Flan.Print as R
import Data.Coerce (coerce)
import LanguageServer.Flan

toFlanType :: R.FlanType' a -> FlanType a
toFlanType = \case
  R.AnyType a -> AnyType a
  R.ValType a (R.VarIdent x) -> case x of
    "Int" -> IntType a
    "Str" -> StrType a
    "Bool" -> BoolType a
    _ -> UnknownType a x
  R.FunType a x (R.AFunTypeArrow a') y -> 
    FunType (FunTypeAnn a a') (toFlanType x) (toFlanType y)
  R.PairType a x (R.APairTypeComma a') y (R.APairTypeClosingBracket a'') -> 
    PairType (PairTypeAnn a a' a'') (toFlanType x) (toFlanType y)

toFlanPattern :: Distinct n
  => a
  -> Scope n
  -> Map String (Name n)
  -> R.TypedPattern' a
  -> (forall l. DExt n l 
    => FlanPattern a (FlanType a) n l 
    -> Map String (Name l)
    -> r)
  -> r
toFlanPattern ea scope env pat cont = case pat of 
  R.UntypedPattern _ p -> 
    patternBinders scope env ea ea p (AnyType ea) cont
  R.ATypedPattern _ p (R.ATypedPatternColon a) ty -> 
    patternBinders scope env ea a p (toFlanType ty) cont 

patternBinders :: Distinct n
  => Scope n
  -> Map String (Name n)
  -> a
  -> a
  -> R.Pattern' a
  -> FlanType a
  -> (forall l . DExt n l 
    => FlanPattern a (FlanType a) n l 
    -> Map String (Name l) 
    -> r)
  -> r
patternBinders scope env ea a' pat mType cont = case (pat, mType) of
  (R.PatternWildcard a, ty) -> cont (PatternWildcard a a' ty) env 
  (R.PatternVar a (R.VarIdent str), ty) -> withFresh scope $ \binder ->
    let env' = Map.insert str (nameOf binder) (sink <$> env)
        var = PatternVar a a' ty str binder
    in cont var env' 
  (R.PatternPair a l (R.APatternPairComma a'') r (R.APatternPairClosingBracket a'''), ty) -> 
    pairType ea ty $ \pairAnn tl tr ->
     patternBinders scope env ea ea l tl $ \pl env' ->
      let scope' = extendScopePattern pl scope
      in patternBinders scope' env' ea ea r tr $ \pr env'' ->
        let pairTy = PairType pairAnn tl tr
            pairPat = PatternPair (PatternPairAnn a a'' a''') a' pairTy pl pr
        in cont pairPat env'' 
  where
    pairType :: a -> FlanType a -> (PairTypeAnn a -> FlanType a -> FlanType a -> r) -> r
    pairType ea ty cont' = case ty of
      PairType a tl tr -> cont' a tl tr
      _ -> cont' (PairTypeAnn ea ea ea) (AnyType ea) (AnyType ea)

bindingToFlan :: Distinct n
  => a
  -> Scope n 
  -> Map String (Name n) 
  -> a
  -> R.Binding' a 
  -> (forall l . DExt n l => Scope l -> Map String (Name l) -> Flan a () l)
  -> Flan a () n
bindingToFlan ea scope env letInAnn binding cont = case binding of
  R.LetBinding a pat (R.ALetEq a') exp -> 
    toFlanPattern ea scope env pat $ \binder env' ->
      let scope' = extendScopePattern binder scope
      in Let 
        (LetAnn a a' letInAnn) 
        ()
        binder 
        (toFlan ea scope' env' exp) 
        binder 
        (cont scope' env')

letToFlan :: Distinct n 
  => a
  -> Scope n 
  -> Map String (Name n) 
  -> a
  -> [R.Binding' a]
  -> R.Term' a
  -> Flan a () n
letToFlan ea scope env _ [] body = toFlan ea scope env body
letToFlan ea scope env inAnn (b:bs) body = 
  let letInAnn = if null bs then inAnn else ea
  in bindingToFlan ea scope env letInAnn b $ \scope' env' -> 
    letToFlan ea scope' env' inAnn bs body 

toFlan :: Distinct n 
  => a
  -> Scope n 
  -> Map String (Name n) 
  -> R.Term' a 
  -> Flan a () n
toFlan ea scope env = \case
  R.ConstFalse a -> Const a () (ConstBool False)
  R.ConstTrue a -> Const a () (ConstBool True)
  R.ConstInt a n -> Const a () (ConstInt n)
  R.ConstStr a s -> Const a () (ConstStr s)
  R.Var a (R.VarIdent x) -> 
    case Map.lookup x env of
      Just name -> AVar a () (Var name) x
      Nothing -> FlanError a () x
  R.App a fun arg -> App a () (toFlan ea scope env fun) (toFlan ea scope env arg)
  R.Pair a l (R.APairComma a') r (R.PairClosingBracket a'') ->
    Pair (PairAnn a a' a'') () (toFlan ea scope env l) (toFlan ea scope env r)
  R.If a predicate (R.IfThenKW a') x (R.IfElseKW a'') y -> 
    If 
      (IfAnn a a' a'') 
      ()
      (toFlan ea scope env predicate) 
      (toFlan ea scope env x) 
      (toFlan ea scope env y)
  R.Lam a pat (R.ALamArrow a') body -> 
    toFlanPattern ea scope env pat $ \binder env' ->
      let scope' = extendScopePattern binder scope
      in Lam (LamAnn a a') () binder (toFlan ea scope' env' body)
  R.Let _ bindings (R.ALetIn a) body -> letToFlan ea scope env a bindings body

patternFromFlan :: (a -> a') -> FlanPattern a (FlanType a) n l -> R.TypedPattern' [a']
patternFromFlan ta pat = patFrom ta pat $ \pat' -> 
  let ann = case pat of
        PatternWildcard _ a _ -> [ta a]
        PatternVar _ a _ _ _ -> [ta a]
        PatternPair _ a _ _ _ -> [ta a]
  in R.ATypedPattern [] pat' (R.ATypedPatternColon ann)
  where
    patFrom 
      :: (a -> a')
      -> FlanPattern a (FlanType a) n l 
      -> (R.Pattern' [a'] -> R.FlanType' [a'] -> R.TypedPattern' [a'])
      -> R.TypedPattern' [a']
    patFrom ta pat cont = case pat of
      PatternWildcard a _ ty -> cont (R.PatternWildcard [ta a]) (toRawType ta ty)
      PatternVar a _ ty str binder -> 
        let intId = nameId $ nameOf binder
            ident = R.VarIdent $ str ++ "@" ++ (show intId)
        in cont (R.PatternVar [ta a] ident) (toRawType ta ty)
      PatternPair (PatternPairAnn a a' a'') _ ty l r -> patFrom ta l $ \l' tyL -> 
        patFrom ta r $ \r' tyR -> 
          cont 
            (R.PatternPair [ta a] l' (R.APatternPairComma [ta a']) r' (R.APatternPairClosingBracket [ta a'']))
            (toRawType ta ty)
    toRawType ta = \case
      IntType a -> R.ValType [ta a] (R.VarIdent "Int")
      StrType a -> R.ValType [ta a] (R.VarIdent "Str")
      BoolType a -> R.ValType [ta a] (R.VarIdent "Bool")
      FunType (FunTypeAnn a a') x y -> 
        R.FunType [ta a] (toRawType ta x) (R.AFunTypeArrow [ta a']) (toRawType ta y)
      PairType (PairTypeAnn a a' a'') l r -> 
        R.PairType [ta a] (toRawType ta l) (R.APairTypeComma [ta a']) (toRawType ta r) (R.APairTypeClosingBracket [ta a''])
      UnknownType a x -> R.ValType [ta a] (R.VarIdent $ "[Unknown-" ++ x ++ "]")
      AnyType a -> R.AnyType [ta a]

fromFlan 
  :: (a -> a'')
  -> (a' -> a'')
  -> Flan a a' n
  -> R.Term' [a'']
fromFlan ta ta' = \case
  AVar a a' _ n -> R.Var [ta a, ta' a'] (R.VarIdent n)
  Var name -> R.Var [] (ppName name)
  App a a' fun arg -> R.App [ta a, ta' a'] (fromFlan ta ta' fun) (fromFlan ta ta' arg)
  Pair (PairAnn a a' a'') a''' l r -> R.Pair [ta a, ta' a''']
    (fromFlan ta ta' l) 
    (R.APairComma [ta a'])
    (fromFlan ta ta' r) 
    (R.PairClosingBracket [ta a''])
  If (IfAnn a a' a'') a''' p x y -> R.If [ta a, ta' a''']
    (fromFlan ta ta' p) 
    (R.IfThenKW [ta a'])
    (fromFlan ta ta' x)
    (R.IfElseKW [ta a''])
    (fromFlan ta ta' y)
  Lam (LamAnn a a') a'' binder body -> R.Lam [ta a, ta' a'']
    (patternFromFlan ta binder) 
    (R.ALamArrow [ta a'])
    (fromFlan ta ta' body)
  Let (LetAnn a a' a'') a''' binderExp expr _ body ->
    let binding = R.LetBinding [ta a, ta' a''']
          (patternFromFlan ta binderExp) 
          (R.ALetEq [ta a'])
          (fromFlan ta ta' expr)
    in R.Let [] [binding] (R.ALetIn [ta a'']) (fromFlan ta ta' body)
  Const a a' c -> case c of
    ConstInt n -> R.ConstInt [ta a, ta' a'] n
    ConstBool b -> if b then (R.ConstTrue [ta a, ta' a']) else (R.ConstFalse [ta a, ta' a'])
    ConstStr s -> R.ConstStr [ta a, ta' a'] s
  FlanError a a' desc -> R.Var [ta a, ta' a'] (R.VarIdent $ "[E-Var: " ++ desc ++ "]")
  where
    ppName name = R.VarIdent ("x" ++ show (nameId name))

ppFlan :: (a -> a'') -> (a' -> a'') -> Flan a a' n -> String
ppFlan = ((R.printTree .) .) . fromFlan 

showFlan :: Show a'' => (a -> a'') -> (a' -> a'') -> Flan a a' n -> String
showFlan = ((show .) .) . fromFlan
