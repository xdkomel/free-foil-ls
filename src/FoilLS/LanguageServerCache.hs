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

module FoilLS.LanguageServerCache where

import Common.LambdaPi
import qualified Control.Monad.Foil              as Foil
import qualified Data.Map                        as Map
import qualified Lampi.AbsLampi as Raw
import Control.Concurrent.STM
import Control.Monad.Reader
import Language.LSP.Server as LSP
-- import Data.IntervalMap (Interval(..))
-- import qualified Data.IntervalMap.Generic.Strict as IM


data LampiProgramStore ann n = LampiProgramStore
  { lampiAsts :: [LambdaPi ann n]
  -- , lampiNames :: Map.Map Int (IM.IntervalMap (Interval Int) (Foil.Name n))
  -- Map.Map Int (IM.IntervalMap (Interval Int) ann)
  } 

newtype LampiStore ann n
  = LampiStore (Map.Map FilePath (LampiProgramStore ann n)) 

newtype LampiEnv ann n = LampiEnv
  { store :: TVar (LampiStore ann n)
  } --deriving (Eq)

defaultLampiEnv :: IO (LampiEnv ASTAnn Foil.VoidS)
defaultLampiEnv = do
  emptyCache <- newTVarIO $ LampiStore Map.empty
  return LampiEnv { store = emptyCache }

type ASTAnn = Raw.BNFC'Position

-- data ASTAnn = ASTAnn
--   { maybeNamePosition :: Maybe (Int, Int)
--   , maybeNameDef :: NameDefinition
--   } deriving (Show, Eq)

type LSP = LspT () (ReaderT (LampiEnv ASTAnn Foil.VoidS) IO)

getCachedStore :: LSP (LampiStore ASTAnn Foil.VoidS)
getCachedStore = lift $ do
  cache <- asks store
  liftIO $ readTVarIO cache

cacheStore :: (LampiStore ASTAnn Foil.VoidS) -> LSP ()
cacheStore cache = lift $ do
  astCache <- asks store
  liftIO $ atomically $ do
    writeTVar astCache cache
