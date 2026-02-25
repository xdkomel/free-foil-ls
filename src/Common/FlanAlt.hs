-- {-# LANGUAGE KindSignatures    #-}
-- {-# LANGUAGE TypeFamilies      #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs             #-}
-- {-# LANGUAGE LambdaCase        #-}
-- {-# LANGUAGE PatternSynonyms   #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE TypeOperators     #-}
-- {-# LANGUAGE DataKinds     #-}
-- {-# LANGUAGE ViewPatterns     #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE MultiParamTypeClasses                 #-}
-- {-# LANGUAGE PolyKinds                 #-}

-- module Common.Flan where

-- import Control.Monad.Foil 
-- import Control.Monad.Free.Foil 
-- import Data.Bifunctor.Sum
-- import           Data.Bifunctor.TH
-- import           Data.Map (Map)
-- import qualified Data.Map                        as Map
-- import qualified Data.IntSet as IntSet
-- import           Generics.Kind.TH                (deriveGenericK)
-- import qualified Flan.Abs as R
-- import qualified Flan.Print as R
-- import Data.Coerce (coerce)

-- data FlanType a
--   = IntType a
--   | StrType a
--   | BoolType a
--   | FunType a (FlanType a) (FlanType a) 
--   | PairType a (FlanType a) (FlanType a) 
--   deriving (Eq, Show, Functor, Foldable, Traversable)

-- data FlanPat a n l where
--   PatWildcard :: a -> FlanPat a n n
--   PatVar :: a -> NameBinder n l -> FlanPat a n l
--   PatPair :: a -> FlanPat a n i -> FlanPat a i l -> FlanPat a n l
--   -- deriving (Eq, Show)

-- data FlanPattern a where 
--   FlanPattern :: DExt n l => a -> (FlanPat a n l) -> (Maybe (FlanType a)) -> FlanPattern a
--   -- deriving (Eq, Show)

-- data FlanF a s t where
--   VarF :: a -> t -> String -> FlanF a s t
--   AppF :: a -> t -> t -> FlanF a s t
--   LamF :: a -> FlanPattern a -> s -> FlanF a s t
--   LetF :: a -> FlanPattern a -> t -> s -> FlanF a s t
--   PairF :: a -> t -> t -> FlanF a s t
--   IfF :: a -> t -> t -> t -> FlanF a s t
--   -- PatternWildcardF :: a -> t -> FlanF a s t
--   -- PatternVarF :: a -> s -> FlanF a s t
--   -- PatternPairF :: a -> t -> FlanF a s t
--   ErrorF :: a -> String -> FlanF a s t
--   deriving (Functor, Foldable, Traversable)
-- deriveBifunctor ''FlanF
-- deriveBifoldable ''FlanF
-- deriveBitraversable ''FlanF
-- deriveGenericK ''FlanF

-- -- newtype FlanPattern a n l = FlanPattern a (FlanPat a n l) (Maybe (FlanType a))

-- -- type FlanSig a s t = FlanF a s t
-- type Flan a = AST NameBinders (FlanF a) n

-- pattern AVar :: a -> Flan a n -> String -> Flan a n
-- pattern AVar a t nameStr = Node (VarF a t nameStr)

-- pattern App :: a -> Flan a n -> Flan a n -> Flan a n
-- pattern App a fun arg = Node (AppF a fun arg)

-- pattern Lam :: a -> FlanPattern a -> NameBinders n l -> Flan a l -> Flan a n
-- pattern Lam a pat binder body = Node (LamF a pat (ScopedAST binder body))

-- pattern Let :: a -> FlanPattern a -> Flan a n -> NameBinders n l -> Flan a l -> Flan a n
-- pattern Let a pat exp binder body = Node (LetF a pat exp (ScopedAST binder body))

-- pattern Pair :: a -> Flan a n -> Flan a n -> Flan a n
-- pattern Pair a x y = Node (PairF a x y)

-- pattern If :: a -> Flan a n -> Flan a n -> Flan a n -> Flan a n
-- pattern If a pred x y = Node (IfF a pred x y)

-- -- pattern PatternWildcard :: a -> Flan a n -> Flan a n
-- -- pattern PatternWildcard a t = Node (PatternWildcardF a t)

-- -- pattern PatternVar :: a -> NameBinder n l -> Flan a l -> Flan a n
-- -- pattern PatternVar a binder body = Node (PatternVarF a (ScopedAST binder body))

-- -- pattern PatternPair :: a -> Flan a n -> Flan a n -> Flan a n
-- -- pattern PatternPair a x y = Node (PatternPairF a x y)

-- pattern FlanError :: a -> String -> Flan a n
-- pattern FlanError a s = Node (ErrorF a s)

-- {-# COMPLETE Var, AVar, App, Lam, Let, Pair, If, FlanError #-}

-- toFlanType :: R.FlanType' a -> FlanType a
-- toFlanType = \case
--   R.ValType a (R.VarIdent x) -> case x of
--     "Int" -> IntType a
--     "Str" -> StrType a
--     "Bool" -> BoolType a
--   R.FunType a x y -> FunType a (toFlanType x) (toFlanType y)
--   R.PairType a x y -> PairType a (toFlanType x) (toFlanType y)

-- toFlanPattern :: DExt n l
--   => Scope n
--   -> Map String (Name n)
--   -> R.TypedPattern' a
--   -> (FlanPattern a n -> NameBinders n l -> Map String (Name l) -> r)
--   -> r
-- toFlanPattern scope env pat cont = case pat of 
--   R.UntypedPattern a p -> toFlanPat scope env p 
--     $ \p' -> cont (FlanPattern a p' Nothing)
--   R.ATypedPattern a p ty -> toFlanPat scope env p 
--     $ \p' -> cont (FlanPattern a p' (Just (toFlanType ty)))
    
-- toFlanPat :: DExt n l
--   => Scope n
--   -> Map String (Name n)
--   -> R.Pattern' a
--   -> (FlanPat a n l -> NameBinders n l -> Map String (Name l) -> r)
--   -> r
-- toFlanPat scope env pat cont = case pat of
--   R.PatternWildcard a -> cont (PatWildcard a) emptyNameBinders env
--   R.PatternVar a (R.VarIdent x) -> withFresh scope $ \binder ->
--     cont (PatVar a binder) (nameBindersSingleton binder) (Map.insert x (nameOf binder) (sink <$> env))
--   R.PatternPair a l r ->
--     toFlanPat scope env l $ \l' lBinders env' ->
--       let scope' = extendScopePattern l' scope
--       in toFlanPat scope' env'  r $ \r' rBinders env'' ->
--         cont (PatPair a l' r') (mergeNameBinders lBinders rBinders) env''


-- instance CoSinkable (FlanPat a) where
--   coSinkabilityProof rename pat cont =
--     case pat of
--       PatWildcard a ->
--         cont rename (PatWildcard a)
--       PatVar a x ->
--         coSinkabilityProof rename x $ \rename' x' ->
--           cont rename' (PatVar a x')
--       PatPair a l r ->
--         coSinkabilityProof rename l $ \rename' l' ->
--           coSinkabilityProof rename' r $ \rename'' r' ->
--             cont rename'' (PatPair a l' r')

--   withPattern withNameBinder id' combine scope pat cont =
--     case pat of
--       PatWildcard a -> cont id' (PatWildcard a)
--       PatVar a x    -> withNameBinder scope x $ \f x' ->
--         cont f (PatVar a x')
--       PatPair a l r -> withPattern withNameBinder id' combine scope l $ \fl l' ->
--         let scope' = extendScopePattern l' scope
--         in withPattern withNameBinder id' combine scope' r $ \fr r' ->
--               cont (combine fl fr) (PatPair a l' r')

-- instance UnifiablePattern (FlanPat a) where
--   unifyPatterns (PatWildcard _) (PatWildcard _) = SameNameBinders emptyNameBinders
--   unifyPatterns (PatVar _ x) (PatVar _ x') = unifyNameBinders x x'
--   unifyPatterns (PatPair _ l r) (PatPair _ l' r') = 
--     case (assertDistinct l, assertDistinct l') of
--       (Distinct, Distinct) -> 
--         unifyPatterns l l' `andThenUnifyPatterns` (r, r')
--   unifyPatterns _ _ = NotUnifiable

-- -- instance InjectName Term where
-- --   injectName = Var


-- -- patternNames :: FlanPattern a -> [String]

-- -- toFlanLam :: Distinct n
-- --   => Scope n
-- --   -> Map String (Name n)
-- --   -> R.TypedPattern' a 
-- --   -> R.Term' a 
-- --   -> (Flan a n, Scope n)
-- -- toFlanLam scope env pat body = 

-- bindingToFlan :: DExt n l
--   => Scope n 
--   -> Map String (Name n) 
--   -> R.Binding' a 
--   -> (Scope l -> Map String (Name l) -> Flan a l)
--   -> Flan a n
-- bindingToFlan scope env binding cont = case binding of
--   R.LetBinding a pat exp -> toFlanPattern scope env pat $ \pat'@(FlanPattern _ p' _) binders env' ->
--     let scope' = extendScopePattern p' scope
--     in Let a p' (toFlan scope env exp) binders (cont scope' env')

-- toFlan :: Distinct n 
--   => Scope n 
--   -> Map String (Name n) 
--   -> R.Term' a 
--   -> Flan a n
-- toFlan scope env = \case
--   R.Var a (R.VarIdent x) -> 
--     case Map.lookup x env of
--       Just name -> AVar a (Var name) x
--       Nothing -> FlanError a ("[]" ++ x ++ " is unbound]")
--   R.App a fun arg -> App a (toFlan scope env fun) (toFlan scope env arg)
--     -- let (funFlan, funDiff) = toFlan scope env fun
--     --     scope' = shiftScope scope funDiff
--     --     (argFlan, argDiff) = toFlan scope' env arg
--     -- in (App a funFlan argFlan, shiftScope scope' argDiff)
--   R.Pair a x y -> Pair a (toFlan scope env x) (toFlan scope env y)
--     -- let (xFlan, xDiff) = toFlan scope env x
--     --     scope' = shiftScope scope xDiff
--     --     (yFlan, yDiff) = toFlan scope' env arg
--     -- in (Pair a xFlan, xDiff yFlan, shiftScope scope' yDiff)
--   R.If a pred x y -> If a (toFlan scope env pred) (toFlan scope env x) (toFlan scope env y)
--     -- let (predFlan, predDiff) = toFlan scope env x
--     --     scope' = shiftScope scope predDiff
--     --     (xFlan, xDiff) = toFlan scope' env x
--     --     scope'' = shiftScope scope' xDiff
--     --     (yFlan, yDiff) = toFlan scope'' env arg
--     -- in (If a predFlan xFlan yFlan, shiftScope scope'' yDiff)
--   R.Lam a pat body -> 
--     toFlanPattern scope env pat $ \pat'@(FlanPattern _ p' _) binders env' ->
--       let scope' = extendScopePattern p' scope
--       in Lam a pat' binders (toFlan scope' env' body)
--     -- let pattern' = toFlanPattern pat 
--     --     newNames = patternNames pattern
--     --     (s, e) = foldl extendScope (scope env) newNames
--     -- in Lam a binder pattern (toFlan s e body)
--   -- R.Let a exps body ->

--   --   toFlanPattern scope env pat $ \p' binders env' ->
--   --     let scope' = extendScopePattern p' scope
--   --     in Let a p' (toFlan scope env a) binders (toFlan scope' env' b)
--     -- let pattern = toFlanPattern pat 
--     --     newNames = patternNames pattern
--     --     (s, e) = foldl extendScope (scope env) newNames
--     -- in Let a binder pattern (toFlan scope env exp) (toFlan s e body)
--   -- where
--   --   extendScope :: (Scope n, Map String (Name n)) 
--   --     -> String 
--   --     -> (Scope n, Map String (Name n))
--   --   extendScope (s, e) x = withFresh s $ \binder ->
--   --     ( extendScope binder s
--   --     , Map.insert x (nameOf binder) (fmap sink e)
--   --     )
