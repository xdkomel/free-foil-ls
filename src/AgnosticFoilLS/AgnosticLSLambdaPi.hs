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

module AgnosticFoilLS.AgnosticLSLambdaPi
  ( runAgnosticLambdaPiLS ) where

import Common.LambdaPi
import qualified AgnosticFoilLS.AgnosticLS as LS
import qualified Lampi.AbsLampi as Raw
import qualified Lampi.LayoutLampi as Raw
import qualified Lampi.LexLampi as Raw
import qualified Lampi.ParLampi as Raw
import Control.Monad.Foil
import Control.Monad.Free.Foil
import qualified Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)
import Language.LSP.Protocol.Types 

type ASTAnn = Raw.BNFC'Position

buildAsts :: String -> [LambdaPi ASTAnn VoidS]
buildAsts = 
  maybe [] terms
  . toAst
  . Raw.resolveLayout True 
  . Raw.tokens
  where
    toAst :: [Raw.Token] -> Maybe Raw.Program
    toAst ts = either (\_ -> Nothing) Just (Raw.pProgram ts)
    terms :: Raw.Program -> [LambdaPi ASTAnn VoidS]
    terms = \case
      Raw.AProgram _ ts -> concatMap toLambdaPiCommand ts
    toLambdaPiCommand :: Raw.Command -> [LambdaPi ASTAnn VoidS]
    toLambdaPiCommand = \case
      Raw.CommandCheck _ l r -> map (toLambdaPi scope env) [l, r]
      Raw.CommandCompute _ l r -> map (toLambdaPi scope env) [l, r]
      where
        scope = emptyScope
        env = Map.empty

findName :: (Int, Int) -> LambdaPi ASTAnn n -> Maybe (Name m, String)
findName position@(x, y) = \case
  (AVar varPos (Var name) nameStr) -> 
    varPos >>= (unsafeCoerce . nameInterval name nameStr)
  (App _ fun arg) -> 
    firstJust $ map (findName position) [fun, arg]
  (Lam _ _ _ body) -> findName position body
  (Pi _ _ _ pat body) -> 
    firstJust [findName position pat, findName position body]
  (Pair _ l r) -> 
    firstJust $ map (findName position) [l, r]
  (First _ a) -> findName position a
  (Second _ a) -> findName position a
  (Product _ l r) -> 
    firstJust $ map (findName position) [l, r]
  _ -> Nothing
  -- (CommandCompute _ l r) -> 
  --   firstJust $ map (findName position) [l, r]
  -- (CommandCheck _ l r) -> 
  --   firstJust $ map (findName position) [l, r]
  -- (LampiProgram _ ts) -> firstJust $ map (findName position) ts
  -- _ -> Nothing
  where
    nameInterval name nameStr (line, col) = 
      if x == line && y >= col && y < col + length nameStr
        then Just (name, nameStr)
        else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust (j@(Just _):_) = j
firstJust (_:t) = firstJust t
firstJust [] = Nothing

buildTelescopes :: Distinct n => LambdaPi a n -> [LS.TermTelescope NameBinder (LambdaPiSig a) n]
buildTelescopes t@AVar{} = [LS.LeafTerm t]
buildTelescopes (App _ fun arg) = concatMap buildTelescopes [fun, arg]
buildTelescopes t@(Lam _ binder _ body) =
  case (assertDistinct binder, assertExt binder) of
    (Distinct, Ext) -> (map (LS.NodeTerm t binder) $ buildTelescopes body)
      ++ [LS.LeafTerm t] 
buildTelescopes t@(Pi _ binder _ fun body) = 
  case (assertDistinct binder, assertExt binder) of
    (Distinct, Ext) -> (map (LS.NodeTerm t binder) $ buildTelescopes body)
      ++ [LS.LeafTerm t] 
      ++ buildTelescopes fun
buildTelescopes (Pair _ l r) = concatMap buildTelescopes [l, r]
buildTelescopes (First _ body) = buildTelescopes body
buildTelescopes (Second _ body) = buildTelescopes body
buildTelescopes (Product _ l r) = concatMap buildTelescopes [l, r]
-- buildTelescopes (CommandCheck _ l r) = concatMap buildTelescopes [l, r]
-- buildTelescopes (CommandCompute _ l r) = concatMap buildTelescopes [l, r]
-- buildTelescopes (LampiProgram _ ts) = concatMap buildTelescopes ts
buildTelescopes _ = []

termRange :: LS.SomeTerm NameBinder (LambdaPiSig ASTAnn) -> Maybe Range
termRange (LS.SomeTerm n) = case n of
  Lam (Just (x, y)) _ pat _ -> toRange x y pat
  Pi (Just (x, y)) _ pat _ _ -> toRange x y pat
  _ -> Nothing
  where
    patternLength = \case
      PatternWildcard _ -> 1
      PatternVar _ x -> length x
      _ -> 1
    toRange x y pat = Just 
      $ Range (Position (pos x) (pos y)) (Position (pos x) (pos $ y + (patternLength pat)))
    pos i = fromIntegral (i + 1)

printTerm :: LambdaPi ASTAnn n -> String
printTerm = showLambdaPi

fileExtension :: String
fileExtension = "lampi"

runAgnosticLambdaPiLS :: IO ()
runAgnosticLambdaPiLS = LS.runAgnosticLanguageServer 
  LS.LSConfiguration
    { LS.fileExtension = fileExtension
    , LS.buildAsts = buildAsts
    , LS.findName = findName
    , LS.buildTelescopes = buildTelescopes
    , LS.termRange = termRange
    , LS.printTerm = printTerm
    }
