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

module Common.LambdaPi where

import qualified Control.Monad.Foil              as Foil
import           Control.Monad.Free.Foil
import           Data.Bifunctor.Sum
import           Data.Bifunctor.TH
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
-- import           Data.String                     (IsString (..))
import           Generics.Kind.TH                (deriveGenericK)
import qualified Lampi.AbsLampi as Raw
import qualified Lampi.PrintLampi as Raw

-- | The signature 'Bifunctor' for the \(\lambda\Pi\).
data LambdaPiF ann scope term
  = AVarF ann term String
  | AppF ann term term
  | LamF ann term scope
  | PiF ann term term scope
  | UniverseF ann 
  | PairF ann term term
  | FirstF ann term
  | SecondF ann term
  | ProductF ann term term
  | PatternWildcardF ann
  | PatternVarF ann String
  | PatternPairF ann term term
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveBifunctor ''LambdaPiF
deriveBifoldable ''LambdaPiF
deriveBitraversable ''LambdaPiF
deriveGenericK ''LambdaPiF
-- instance ZipMatchK LambdaPiF

data LambdaPiErrorF ann scope term
  = UnboundSymF ann String
  | UnsupportedF ann 

  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveBifunctor ''LambdaPiErrorF
deriveBifoldable ''LambdaPiErrorF
deriveBitraversable ''LambdaPiErrorF
deriveGenericK ''LambdaPiErrorF
-- instance ZipMatchK LambdaPiErrorF

-- | Sum of signature bifunctors.
type (:+:) = Sum

-- | \(\lambda\Pi\)-terms in scope @n@, freely generated from the sum of signatures 'LambdaPiF' and t'PairF'.
type LambdaPiSig ann = (LambdaPiF ann) :+: (LambdaPiErrorF ann)
type LambdaPi ann n = AST Foil.NameBinder (LambdaPiSig ann) n

pattern AVar :: ann -> LambdaPi ann n -> String -> LambdaPi ann n
pattern AVar ann t nameStr =  Node (L2 (AVarF ann t nameStr))

pattern App :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
pattern App ann fun arg = Node (L2 (AppF ann fun arg))

pattern Lam :: ann -> Foil.NameBinder n l -> LambdaPi ann n -> LambdaPi ann l -> LambdaPi ann n
pattern Lam ann binder pat body = Node (L2 (LamF ann pat (ScopedAST binder body)))

pattern Pi :: ann -> Foil.NameBinder n l -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann l -> LambdaPi ann n
pattern Pi ann binder pat a b = Node (L2 (PiF ann pat a (ScopedAST binder b)))

pattern Pair :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
pattern Pair ann l r = Node (L2 (PairF ann l r))

pattern First :: ann -> LambdaPi ann n -> LambdaPi ann n
pattern First ann t = Node (L2 (FirstF ann t))

pattern Second :: ann -> LambdaPi ann n -> LambdaPi ann n
pattern Second ann t = Node (L2 (SecondF ann t))

pattern Product :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
pattern Product ann l r = Node (L2 (ProductF ann l r))

pattern Universe :: ann -> LambdaPi ann n
pattern Universe ann = Node (L2 (UniverseF ann))

pattern PatternWildcard :: ann -> LambdaPi ann n
pattern PatternWildcard ann = Node (L2 (PatternWildcardF ann))

pattern PatternVar :: ann -> String -> LambdaPi ann n
pattern PatternVar ann name = Node (L2 (PatternVarF ann name))

pattern PatternPair :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
pattern PatternPair ann a b = Node (L2 (PatternPairF ann a b))

-- pattern CommandCheck :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
-- pattern CommandCheck ann l r = Node (L2 (CommandCheckF ann l r))

-- pattern CommandCompute :: ann -> LambdaPi ann n -> LambdaPi ann n -> LambdaPi ann n
-- pattern CommandCompute ann l r = Node (L2 (CommandComputeF ann l r))

-- pattern LampiProgram :: ann -> [LambdaPi ann n] -> LambdaPi ann n
-- pattern LampiProgram ann ts = Node (L2 (LampiProgramF ann ts))

pattern ErrorUnbound :: ann -> String -> LambdaPi ann n
pattern ErrorUnbound ann name = Node (R2 (UnboundSymF ann name))

pattern ErrorUnsupported :: ann -> LambdaPi ann n
pattern ErrorUnsupported ann = Node (R2 (UnsupportedF ann))

{-# COMPLETE Var, App, Lam, Pi, Pair, First, Second, Product, Universe #-}

-- | \(\lambda\Pi\)-terms are pretty-printed using BNFC-generated printer via 'Raw.Term'.
-- instance Show (LambdaPi ann Foil.VoidS) where
--   show = ppLambdaPi

-- data SomeLambdaPi a where
--   SomeLambdaPi :: LambdaPi a n -> SomeLambdaPi a

-- foldLambdaPi :: (acc -> LambdaPi a n -> acc) -> acc -> LambdaPi a n -> acc
-- foldLambdaPi f acc = \case
--     t@Var{} -> f acc t
--     t@AVar{} -> f acc t
--     t@(App _ fun arg) -> f (f (f acc t) fun) arg
--     t@(Lam _ _ body) -> f (f acc t) (unsafeCoerce body)
--     t@(Pi _ _ pat body) -> f (f (f acc t) pat) (unsafeCoerce body)
--     t@(Pair _ l r) -> f (f (f acc t) l) r
--     t@(First _ a) -> f (f acc t) a
--     t@(Second _ a) -> f (f acc t) a
--     t@(Product _ l r) -> f (f (f acc t) l) r
--     t@Universe{} -> f acc t
--     t@(ProgramDef _ a) -> f
--     t@ErrorUnbound{} -> f acc t
--     t@ErrorUnsupported{} -> f acc t

-- | \(\lambda\Pi\)-terms can be (unsafely) parsed from a 'String' via 'Raw.Term'.
-- instance IsString (LambdaPi Raw.BNFC'Position Foil.VoidS) where
--   fromString input =
--     case Raw.pTerm (Raw.tokens input) of
--       Left err -> error ("could not parse λΠ-term: " <> input <> "\n  " <> err)
--       Right term -> toLambdaPiClosed term

-- data Annotated ann n = Annotated ann n
-- data AnnotatedName ann n = AnnotatedName ann (Foil.Name n)

-- mapAnnotatedName :: (Foil.Name a -> Foil.Name b) -> AnnotatedName ann a -> AnnotatedName ann b
-- mapAnnotatedName f (AnnotatedName ann a) = (AnnotatedName ann (f a))

-- type NameDefinition = Maybe (Int, Interval Int)

-- data ExtendedAnnotation ann0 ann1 
--   = ExtendedAnnotation ann0 ann1
--   | OriginalAnnotation ann0

-- | Convert a raw \(\lambda\)-abstraction into a scope-safe \(\lambda\Pi\)-term.
toLambdaPiLam
  :: Foil.Distinct n
  => Foil.Scope n 
  -> Map String (Foil.Name n)
  -> Raw.Pattern' ann 
  -> Raw.ScopedTerm' ann 
  -> LambdaPi ann n
toLambdaPiLam scope env pat (Raw.AScopedTerm _ body) =
  case pat of
    Raw.PatternWildcard ann -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
      in Lam ann binder (PatternWildcard ann) (toLambdaPi scope' (fmap Foil.sink env) body)

    Raw.PatternVar ann (Raw.VarIdent x) -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
          env' = Map.insert x (Foil.nameOf binder) (fmap Foil.sink env)
      in Lam ann binder (PatternVar ann x) (toLambdaPi scope' env' body)

    Raw.PatternPair ann _ _ -> ErrorUnsupported ann

-- | Convert a raw \(\Pi\)-type into a scope-safe \(\lambda\Pi\)-term.
toLambdaPiPi
  :: Foil.Distinct n
  => Foil.Scope n 
  -> Map String (Foil.Name n)
  -> Raw.Pattern' ann 
  -> Raw.Term' ann 
  -> Raw.ScopedTerm' ann 
  -> LambdaPi ann n
toLambdaPiPi scope env pat a (Raw.AScopedTerm _ b) =
  case pat of
    Raw.PatternWildcard ann -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
       in Pi ann binder (PatternWildcard ann) (toLambdaPi scope env a) (toLambdaPi scope' (fmap Foil.sink env) b)

    Raw.PatternVar ann (Raw.VarIdent x) -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
          env' = Map.insert x (Foil.nameOf binder) (fmap Foil.sink env)
       in Pi ann binder (PatternVar ann x) (toLambdaPi scope env a) (toLambdaPi scope' env' b)

    Raw.PatternPair ann _ _ -> ErrorUnsupported ann

-- | Convert a raw expression into a scope-safe \(\lambda\Pi\)-term.
toLambdaPi 
  :: Foil.Distinct n
  => Foil.Scope n 
  -> Map String (Foil.Name n)
  -> Raw.Term' ann 
  -> LambdaPi ann n
toLambdaPi scope env = \case
    Raw.Var ann (Raw.VarIdent x) -> 
      case Map.lookup x env of
        Just name -> AVar ann (Var name) x
        Nothing   -> ErrorUnbound ann x

    Raw.App ann fun arg ->
      App ann (toLambdaPi scope env fun) (toLambdaPi scope env arg)

    Raw.Lam _loc pat body -> toLambdaPiLam scope env pat body
    Raw.Pi _loc pat a b -> toLambdaPiPi scope env pat a b

    Raw.Pair ann l r -> Pair ann (toLambdaPi scope env l) (toLambdaPi scope env r)
    Raw.First ann t -> First ann (toLambdaPi scope env t)
    Raw.Second ann t -> Second ann (toLambdaPi scope env t)
    Raw.Product ann l r -> Product ann (toLambdaPi scope env l) (toLambdaPi scope env r)

    Raw.Universe ann -> Universe ann

-- | Convert a raw expression into a /closed/ scope-safe \(\lambda\Pi\)-term.
-- toLambdaPiClosed :: Raw.Term' ann -> LambdaPi ann Foil.VoidS
-- toLambdaPiClosed = toLambdaPi Foil.emptyScope (\) Map.empty

-- toLambdaPiCommand :: Foil.Distinct n
--   => Foil.Scope n 
--   -> Map String (Foil.Name n)
--   -> Raw.Command' ann 
--   -> LambdaPi ann n
-- toLambdaPiCommand scope env = \case
--   Raw.CommandCheck ann l r -> CommandCheck ann (toLambdaPi scope env l) (toLambdaPi scope env r)
--   Raw.CommandCompute ann l r -> CommandCompute ann (toLambdaPi scope env l) (toLambdaPi scope env r)

-- toLambdaPiProgram:: Foil.Distinct n
--   => Foil.Scope n 
--   -> Map String (Foil.Name n)
--   -> Raw.Program' ann 
--   -> LambdaPi ann n
-- toLambdaPiProgram scope env = \case
--   Raw.AProgram ann ts -> LampiProgram ann (map (toLambdaPiCommand scope env) ts)

fromLambdaPiPattern :: LambdaPi ann n -> Maybe (Raw.Pattern' ann)
fromLambdaPiPattern = \case
  PatternWildcard loc -> Just $ Raw.PatternWildcard loc
  PatternVar loc x -> Just $ Raw.PatternVar loc $ Raw.VarIdent x
  _ -> Nothing

-- | Convert back from a scope-safe \(\lambda\Pi\)-term into a raw expression or type.
fromLambdaPi :: LambdaPi ann n -> Raw.Term' ann
fromLambdaPi = \case
  AVar loc _ name -> Raw.Var loc $ Raw.VarIdent name
  Var name -> Raw.Var locErr (ppName name)
  App loc fun arg -> Raw.App loc (fromLambdaPi fun) (fromLambdaPi arg)
  Lam loc _ pat body -> 
    let p = maybe (errPattern loc) id (fromLambdaPiPattern pat)
    in Raw.Lam loc p (Raw.AScopedTerm loc (fromLambdaPi body))
    -- maybe errPattern 
    -- (\p -> )
    -- $ fromLambdaPiPattern pat 
  Pi loc _ pat a b -> 
    let p = maybe (errPattern loc) id (fromLambdaPiPattern pat)
    in Raw.Pi loc p (fromLambdaPi a) (Raw.AScopedTerm loc (fromLambdaPi b))
  Pair loc l r -> Raw.Pair loc (fromLambdaPi l) (fromLambdaPi r)
  First loc t -> Raw.First loc (fromLambdaPi t)
  Second loc t -> Raw.Second loc (fromLambdaPi t)
  Product loc l r -> Raw.Product loc (fromLambdaPi l) (fromLambdaPi r)
  Universe loc -> Raw.Universe loc
  ErrorUnbound loc strName -> Raw.Var loc $ Raw.VarIdent $ "[unbound " ++ strName ++ "]"
  ErrorUnsupported loc -> Raw.Var loc $ Raw.VarIdent "[unsupported]"
  where
    locErr = error "no location info available when converting from an AST"
    ppName name = Raw.VarIdent ("x" ++ show (Foil.nameId name))
    errPattern loc = Raw.PatternVar loc $ Raw.VarIdent "[corrupted]"

-- fromLambdaPiCommand :: LambdaPi ann n -> Maybe (Raw.Command' ann)
-- fromLambdaPiCommand = \case
--   CommandCheck loc l r -> Just $ Raw.CommandCheck loc (fromLambdaPi l) (fromLambdaPi r)
--   CommandCompute loc l r -> Just $ Raw.CommandCompute loc (fromLambdaPi l) (fromLambdaPi r)
--   _ -> Nothing

-- fromLambdaPiProgram :: LambdaPi ann n -> Maybe (Raw.Program' ann)
-- fromLambdaPiProgram = \case
--   LampiProgram loc ts -> Just $ Raw.AProgram loc (catMaybes $ map fromLambdaPiCommand ts)
--   _ -> Nothing

-- type PrintedLambdaPi a = Either (Raw.Term' a) (Either (Raw.Command' a) (Raw.Program' a))

-- printLambdaPi :: PrintedLambdaPi a -> String
-- printLambdaPi (Left t) = Raw.printTree t
-- printLambdaPi (Right (Left c)) = Raw.printTree c
-- printLambdaPi (Right (Right p)) = Raw.printTree p

ppLambdaPi :: LambdaPi ann n -> String
ppLambdaPi = Raw.printTree . fromLambdaPi
-- ppLambdaPi n = printLambdaPi 
--   $ maybe (Left $ fromLambdaPi n) Right
--   $ maybe (fmap Left (fromLambdaPiCommand n)) (Just . Right) (fromLambdaPiProgram n)

-- showLambdaPi' :: Show a => PrintedLambdaPi a -> String
-- showLambdaPi' (Left t) = show t
-- showLambdaPi' (Right (Left c)) = show c
-- showLambdaPi' (Right (Right p)) = show p

showLambdaPi :: LambdaPi Raw.BNFC'Position n -> String
showLambdaPi = show . fromLambdaPi
-- showLambdaPi n = showLambdaPi'
--   $ maybe (Left $ fromLambdaPi n) Right
--   $ maybe (fmap Left (fromLambdaPiCommand n)) (Just . Right) (fromLambdaPiProgram n)

