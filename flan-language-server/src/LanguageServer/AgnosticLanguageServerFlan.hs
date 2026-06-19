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


module LanguageServer.AgnosticLanguageServerFlan where

import qualified Control.Monad.Foil.Internal as F
import qualified Control.Monad.Free.Foil as F
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Protocol.Types 
  ( SemanticTokenAbsolute(..)
  , SemanticTokenTypes(..) )
import Data.Bifunctor (bimap)
import qualified Flan.Abs as R
import qualified Flan.Lex as R
import qualified Flan.Par as R
import LanguageServer.Flan
import LanguageServer.FlanConversion

import AgnosticLanguageServer

type PosAnn = R.BNFC'Position 

instance R.HasPosition PosAnn where
  hasPosition = id

data NodeAnn = NodeAnn (Maybe (Int, Int)) FlanType' [String]
  deriving (Show, Eq)

type FlanType' = FlanType PosAnn
type Binder = FlanPattern PosAnn FlanType'
type Sig = FlanF PosAnn NodeAnn
type AST = F.AST Binder Sig

buildAsts' :: String -> [SomeScopeWithAST Binder Sig]
buildAsts' input = 
  let ts = R.tokens input
      ast = either (const Nothing) Just (R.pProgram ts) 
  in maybe [] (\p ->
      let term = case p of
            R.AProgram _ t -> toFlan Nothing scope Map.empty t
          ranged = buildRanges term
          typed = typecheck nameMap anyTypeEmp ranged
      in [SomeScopeWithAST scope typed]
    ) ast
  where
    scope = F.emptyScope
    nameMap = F.emptyNameMap

-- TODO: do not store in annotation
buildRanges :: Flan PosAnn () n -> Flan PosAnn PosAnn n
buildRanges = \case
  F.Var n -> F.Var n
  F.Node sig -> 
    let sig' = bimap (\(F.ScopedAST b ast) -> F.ScopedAST b $ buildRanges ast) buildRanges sig
    in F.Node (deduceRange sig')
  where
    deduceRange = \case
      VarF a _ t s -> VarF a (cols (length s) a) t s
      AppF a _ f b -> AppF a (endingOf b) f b
      LamF a _ body'@(F.ScopedAST _ body) -> LamF a (endingOf body) body'
      LetF a _ e body'@(F.ScopedAST _ body) -> LetF a (endingOf body) e body'
      PairF a@(PairAnn _ _ a') _ l r -> 
        let plusOne (x, y) = (x, y + 1)
        in PairF a (fmap plusOne a') l r
      IfF a _ p x y -> IfF a (endingOf y) p x y
      ConstF a _ c -> ConstF a (cols (length $ show c) a) c
      ErrorF a _ s -> ErrorF a (cols (length s) a) s
    cols len = fmap (\(x, y) -> (x, y + len))
    endingOf = \case
      F.Var{} -> Nothing
      F.Node sig -> case sig of
        VarF _ a _ _ -> a
        AppF _ a _ _ -> a
        LamF _ a _ -> a
        LetF _ a _ _ -> a
        PairF _ a _ _ -> a
        IfF _ a _ _ _ -> a
        ConstF _ a _ -> a
        ErrorF _ a _ -> a

instance RangedSig Sig where
  range = \case
    VarF a a' _ _ -> r a a'
    AppF a a' _ _ -> r a a'
    LamF (LamAnn a _) a' _ -> r a a'
    LetF (LetAnn a _ _) a' _ _ -> r a a'
    PairF (PairAnn a _ _) a' _ _ -> r a a'
    IfF (IfAnn a _ _) a' _ _ _ -> r a a'
    ConstF a a' _ -> r a a'
    ErrorF a a' _ -> r a a'
    where
      r s (NodeAnn e _ _) = s >>= \sp -> fmap (\ep -> toRangePos sp ep) e

instance FoldablePat Binder where
  foldrPat f acc = \case
    PatternWildcard{} -> acc
    PatternVar{} -> acc
    PatternPair _ _ _ l r -> f (SomePattern l) $ f (SomePattern r) acc
  
  rangePat = \case
    PatternWildcard pos _ _ -> fmap (flip toRangeLen $ 1) pos
    PatternVar pos _ _ str _ -> fmap (flip toRangeLen $ length str) pos
    PatternPair (PatternPairAnn pos _ pos') _ _ _ _ -> do
      p <- pos
      p' <- pos'
      Just $ toRangePos p p'
  
  binderOf = \case
    PatternWildcard{} -> Nothing
    PatternVar _ _ _ _ b -> Just b
    PatternPair{} -> Nothing


lspos i = fromIntegral $ i - 1

toRangeLen :: (Int, Int) -> Int -> LSP.Range
toRangeLen (x, y) len = LSP.mkRange (lspos x) (lspos y) (lspos x) (lspos $ y + len)

toRangePos :: (Int, Int) -> (Int, Int) -> LSP.Range
toRangePos (x, y) (x', y') = LSP.mkRange (lspos x) (lspos y) (lspos x') (lspos y')

-- printTerm' :: SomeAST Binder Sig -> String
-- printTerm' (SomeAST x) = showFlan Left Right x

data ScopedType a (s :: F.S) where
  ScopedType :: FlanType a -> ScopedType a n

resolveTy :: FlanType' -> FlanType' -> (FlanType', [String])
resolveTy a b = case (a, b) of
  (_, AnyType _) -> (a, [])
  (AnyType _, _) -> (b, [])
  (IntType _, IntType _) -> (a, [])
  (StrType _, StrType _) -> (a, [])
  (BoolType _, BoolType _) -> (a, [])
  (FunType _ x y, FunType _ x' y') -> 
    complex FunType funTypeAnnEmp x y x' y'
  (PairType _ x y, PairType _ x' y') -> 
    complex PairType pairTypeAnnEmp x y x' y'
  (x, y) -> (anyTypeEmp, ["Expected type: " ++ show x ++ ", but got " ++ show y])
  where
    complex f ann x y x' y' = 
      let (lt, le) = resolveTy x x'
          (rt, re) = resolveTy y y'
      in (f ann lt rt, le ++ re)

instance F.Sinkable (ScopedType a) where
  sinkabilityProof _ (ScopedType t) = ScopedType t

instance TypedSig Sig FlanType' where
  ty = \case
    VarF _ (NodeAnn _ t _) _ _ -> t
    AppF _ (NodeAnn _ t _) _ _ -> t
    LamF _ (NodeAnn _ t _) _ -> t
    LetF _ (NodeAnn _ t _) _ _ -> t
    PairF _ (NodeAnn _ t _) _ _ -> t
    IfF _ (NodeAnn _ t _) _ _ _ -> t
    ConstF _ (NodeAnn _ t _) _ -> t
    ErrorF _ (NodeAnn _ t _) _ -> t

instance TypedPat Binder FlanType' where
  patTy = \case
    PatternWildcard _ _ t -> t 
    PatternVar _ _ t _ _ -> t 
    PatternPair _ _ t _ _ -> t 
  addPattern = \case
    PatternWildcard _ _ _ -> id
    PatternVar _ _ t _ b -> F.addNameBinder b t
    PatternPair _ _ _ l r -> addPattern r . addPattern l

anyTypeEmp :: FlanType'
anyTypeEmp = AnyType Nothing
funTypeAnnEmp :: FunTypeAnn PosAnn
funTypeAnnEmp = FunTypeAnn Nothing Nothing
pairTypeAnnEmp :: PairTypeAnn PosAnn
pairTypeAnnEmp = PairTypeAnn Nothing Nothing Nothing
boolTypeEmp :: FlanType'
boolTypeEmp = BoolType Nothing
intTypeEmp :: FlanType'
intTypeEmp = IntType Nothing
strTypeEmp :: FlanType'
strTypeEmp = StrType Nothing

instance TypeDeductiveSig (FlanF PosAnn PosAnn) Sig FlanType' Binder where
  deduceType exTy = \case
    VarF a a' vOf s -> 
      let (v, vTy) = vOf exTy
          (t, msg) = verdict vTy
      in VarF a (NodeAnn a' t msg) v s
    AppF a a' xOf yOf ->
      let (y, yTy) = yOf anyTypeEmp
          (x, xTy) = xOf (FunType funTypeAnnEmp yTy exTy)
          xReturnTy = case xTy of
            FunType _ _ r -> r
            _ -> anyTypeEmp
          (t, msg) = verdict xReturnTy
      in AppF a (NodeAnn a' t msg) x y
    LamF a a' (patTyp, bodyOf) ->
      let (exPatTyp, exBodyTy) = case exTy of
            FunType _ x y -> (x, y)
            _ -> (anyTypeEmp, anyTypeEmp)
          (body, bodyTy) = bodyOf exBodyTy
          (patTyp', patMsg) = resolveTy exPatTyp patTyp
          (t, msg) = verdict (FunType funTypeAnnEmp patTyp' bodyTy)
          msg' = msg ++ patMsg
      in LamF a (NodeAnn a' t msg') body
    LetF a a' (patTyp, expOf) (_, bodyOf) ->
      let (exp, _) = expOf patTyp
          (body, bodyTy) = bodyOf exTy
          (t, msg) = verdict bodyTy
      in LetF a (NodeAnn a' t msg) exp body
    PairF a a' lOf rOf ->
      let (exLTy, exRTy) = case exTy of
            PairType _ l r -> (l, r)
            _ -> (anyTypeEmp, anyTypeEmp)
          (l, lTy) = lOf exLTy
          (r, rTy) = rOf exRTy
          (t, msg) = verdict $ PairType pairTypeAnnEmp lTy rTy
      in PairF a (NodeAnn a' t msg) l r
    IfF a a' pOf xOf yOf ->
      let (p, _) = pOf boolTypeEmp
          (x, xTy) = xOf exTy
          (y, yTy) = yOf exTy
          (bodyTy, errs) = resolveTy xTy yTy
          (t, msg) = verdict bodyTy
          msg' = msg ++ errs
      in IfF a (NodeAnn a' t msg') p x y
    ConstF a a' c ->
      let cTy = case c of
            ConstInt _ -> intTypeEmp
            ConstStr _ -> strTypeEmp
            ConstBool _ -> boolTypeEmp
          (t, msg) = verdict cTy
      in ConstF a (NodeAnn a' t msg) c
    ErrorF a a' s -> ErrorF a (NodeAnn a' anyTypeEmp []) s
    where
      verdict = resolveTy exTy

instance TokenizableSig Sig where
  tokenize = \case
    VarF a (NodeAnn _ t _) _ str -> mkToken a (length str) (varOrFun t)
    AppF _ _ x y -> x ++ y
    LamF (LamAnn a a') _ (pt, bt) -> 
      (mkToken a 1 SemanticTokenTypes_Keyword) 
        ++ pt
        ++ (mkToken a' 2 SemanticTokenTypes_Keyword) 
        ++ bt
    LetF (LetAnn a a' a'') _ (ept, et) (_, bt) -> 
      (mkToken a 1 SemanticTokenTypes_Keyword) 
        ++ ept
        ++ (mkToken a' 1 SemanticTokenTypes_Keyword) 
        ++ et
        ++ (mkToken a'' 1 SemanticTokenTypes_Keyword) 
        ++ bt
    PairF (PairAnn a a' a'') _ lt rt -> 
      (mkToken a 1 SemanticTokenTypes_Keyword) 
        ++ lt
        ++ (mkToken a' 1 SemanticTokenTypes_Keyword) 
        ++ rt
        ++ (mkToken a'' 1 SemanticTokenTypes_Keyword) 
    IfF (IfAnn a a' a'') _ pt tt et -> 
      (mkToken a 2 SemanticTokenTypes_Keyword) 
        ++ pt
        ++ (mkToken a' 4 SemanticTokenTypes_Keyword) 
        ++ tt
        ++ (mkToken a'' 4 SemanticTokenTypes_Keyword) 
        ++ et
    ConstF a _ c -> mkToken a (length $ show c) (constSemanticType c)
    ErrorF a _ s -> mkToken a (length s) SemanticTokenTypes_Variable
    where
      constSemanticType = \case
        ConstInt _ -> SemanticTokenTypes_Number
        ConstStr _ -> SemanticTokenTypes_String
        ConstBool _ -> SemanticTokenTypes_Keyword

instance TokenizablePat Binder where
  tokenizePat = \case
    PatternWildcard a a' t -> (mkToken a 1 SemanticTokenTypes_Variable) 
      ++ (mkToken a' 1 SemanticTokenTypes_Keyword)
      ++ (tokenizeType t)
    PatternVar a a' t str _ -> (mkToken a (length str) (varOrFun t))
      ++ (mkToken a' 1 SemanticTokenTypes_Keyword)
      ++ (tokenizeType t)
    PatternPair (PatternPairAnn a a' a'') a''' t l r -> (mkToken a 1 SemanticTokenTypes_Keyword)
      ++ (tokenizePat l) 
      ++ (mkToken a' 1 SemanticTokenTypes_Keyword)
      ++ (tokenizePat r)
      ++ (mkToken a'' 1 SemanticTokenTypes_Keyword)
      ++ mkToken a''' 1 SemanticTokenTypes_Keyword
      ++ (tokenizeType t)

tokenizeType :: FlanType' -> [SemanticTokenAbsolute]
tokenizeType = \case
  IntType a -> mkToken a 3 SemanticTokenTypes_Type
  StrType a -> mkToken a 3 SemanticTokenTypes_Type
  BoolType a -> mkToken a 4 SemanticTokenTypes_Type
  FunType (FunTypeAnn _ a) l r -> (tokenizeType l) 
    ++ (mkToken a 2 SemanticTokenTypes_Keyword)
    ++ (tokenizeType r)
  PairType (PairTypeAnn a a' a'') l r -> (mkToken a 1 SemanticTokenTypes_Keyword)
    ++ (tokenizeType l)
    ++ (mkToken a' 1 SemanticTokenTypes_Keyword)
    ++ (tokenizeType r)
    ++ (mkToken a'' 1 SemanticTokenTypes_Keyword)
  UnknownType a str -> mkToken a (length str) SemanticTokenTypes_Type
  AnyType a -> mkToken a 1 SemanticTokenTypes_Type

varOrFun :: FlanType a -> SemanticTokenTypes
varOrFun = \case
  FunType _ _ _ -> SemanticTokenTypes_Function
  _ -> SemanticTokenTypes_Variable

mkToken :: R.HasPosition a 
  => a -> Int -> SemanticTokenTypes -> [SemanticTokenAbsolute]
mkToken ann len tokenTy = maybe [] ((\x -> [x]) . build) (R.hasPosition ann)
  where
    build (x, y) = SemanticTokenAbsolute
      { _tokenType = tokenTy
      , _tokenModifiers = []
      , _startChar = fromIntegral $ y - 1
      , _line = fromIntegral $ x - 1
      , _length = fromIntegral len
      }

diagnosticKey :: T.Text
diagnosticKey = T.pack "Flan Server Diagnostic"

combineMsgs :: [String] -> String
combineMsgs = concatMap (++ "\n\n")

flanDiagnostic :: Int -> Int -> Int -> String -> [LSP.Diagnostic]
flanDiagnostic x y len msg = [LSP.Diagnostic
  (toRangeLen (x, y) len)
  (Just LSP.DiagnosticSeverity_Error)
  Nothing
  Nothing
  (Just diagnosticKey)
  (T.pack msg)
  Nothing
  Nothing
  Nothing]

diagnoseType :: FlanType' -> [LSP.Diagnostic]
diagnoseType = \case
  UnknownType (Just (x, y)) str -> flanDiagnostic x y (length str) ("Unknown type: " ++ str ++ "\n\n")
  FunType _ l r -> diagnoseType l ++ diagnoseType r
  PairType _ l r -> diagnoseType l ++ diagnoseType r
  _ -> []

instance DiagnosablePat Binder where
  diagnosePat = \case
    PatternWildcard _ _ t -> diagnoseType t
    PatternVar _ _ t _ _ -> diagnoseType t
    PatternPair _ _ t l r -> diagnoseType t ++ diagnosePat l ++ diagnosePat r

instance DiagnosableSig Sig where
  diagnose = \case
    ErrorF (Just (x, y)) (NodeAnn _ _ errs) str ->
      flanDiagnostic x y (length str) $ combineMsgs $ "Unbound symbol" : errs
    VarF (Just (x, y)) (NodeAnn _ _ errs) _ str ->
      if null errs then [] else
        flanDiagnostic x y (length str) $ combineMsgs errs
    ConstF (Just (x, y)) (NodeAnn _ _ errs) c ->
      if null errs then [] else
        flanDiagnostic x y (length $ show c) $ combineMsgs errs
    AppF (Just (x, y)) (NodeAnn _ _ errs) _ _ ->
      if null errs then [] else
        flanDiagnostic x y 1 $ combineMsgs errs
    LamF (LamAnn (Just (x, y)) _) (NodeAnn _ _ errs) _ ->
      if null errs then [] else
        flanDiagnostic x y 1 $ combineMsgs errs
    LetF (LetAnn (Just (x, y)) _ _) (NodeAnn _ _ errs) _ _ ->
      if null errs then [] else
        flanDiagnostic x y 1 $ combineMsgs errs
    PairF (PairAnn (Just (x, y)) _ _) (NodeAnn _ _ errs) _ _ ->
      if null errs then [] else
        flanDiagnostic x y 1 $ combineMsgs errs
    IfF (IfAnn (Just (x, y)) _ _) (NodeAnn _ _ errs) _ _ _ ->
      if null errs then [] else
        flanDiagnostic x y 1 $ combineMsgs errs
    _ -> []

instance HoverableSig Sig where
  hoverData = \case
    VarF _ a _ _ -> fromAnn a
    ConstF _ a _ -> fromAnn a
    AppF _ a _ _ -> fromAnn a
    LamF _ a _ -> fromAnn a
    LetF _ a _ _ -> fromAnn a
    PairF _ a _ _ -> fromAnn a
    IfF _ a _ _ _ -> fromAnn a
    ErrorF _ a _ -> fromAnn a
    where
      fromAnn (NodeAnn _ t e) = [show t] ++ e

instance HoverablePat Binder where
  hoverDataPat = \case
    PatternWildcard _ _ t -> [show t]
    PatternVar _ _ t _ _ -> [show t]
    PatternPair _ _ t _ _ -> [show t]

runFlanLS :: IO ()
runFlanLS = runLanguageServer LSConfiguration
  { fileExtension = "flan"
  , buildAsts = buildAsts'
  -- , printTerm = printTerm'
  }
