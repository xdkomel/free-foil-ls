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

module Common.LanguageServerCache where

import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Monad.Reader
import Language.LSP.Server as LSP

newtype LangProgramStore ast = LangProgramStore 
  { langAst :: [ast] 
  }

newtype LangStore ast
  = LangStore (Map.Map FilePath (LangProgramStore ast)) 

newtype LangEnv ast = LangEnv
  { store :: TVar (LangStore ast)
  }

defaultLangEnv :: IO (LangEnv ast)
defaultLangEnv = do
  emptyCache <- newTVarIO $ LangStore Map.empty
  return LangEnv { store = emptyCache }

type LSP ast = LspT () (ReaderT (LangEnv ast) IO)

getCachedStore :: LSP ast (LangStore ast)
getCachedStore = lift $ do
  cache <- asks store
  liftIO $ readTVarIO cache

cacheStore :: (LangStore ast) -> LSP ast ()
cacheStore cache = lift $ do
  astCache <- asks store
  liftIO $ atomically $ do
    writeTVar astCache cache
