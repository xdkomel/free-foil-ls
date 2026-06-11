module AgnosticLanguageServer.Cache
  ( LangProgramStore(..)
  , LangStore(..)
  , LangEnv(..)
  , defaultLangEnv
  , LSP
  , getCachedStore
  , cacheStore
  ) where

import qualified Data.Map as Map
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IORef (IORef, newIORef)
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server

newtype LangProgramStore ast = LangProgramStore
  { langAst :: [ast]
  }

newtype LangStore ast
  = LangStore (Map.Map FilePath (LangProgramStore ast))

data LangEnv ast = LangEnv
  { store :: TVar (LangStore ast)
  , debounceTimers :: TVar (Map.Map Uri ThreadId)
  , lspEnvRef :: IORef (Maybe (LanguageContextEnv ()))
  }

defaultLangEnv :: IO (LangEnv ast)
defaultLangEnv = do
  emptyCache <- newTVarIO $ LangStore Map.empty
  emptyTimers <- newTVarIO Map.empty
  emptyEnvRef <- newIORef Nothing
  return LangEnv
    { store = emptyCache
    , debounceTimers = emptyTimers
    , lspEnvRef = emptyEnvRef
    }

type LSP ast = LspT () (ReaderT (LangEnv ast) IO)

getCachedStore :: LSP ast (LangStore ast)
getCachedStore = lift $ do
  cache <- asks store
  liftIO $ readTVarIO cache

cacheStore :: LangStore ast -> LSP ast ()
cacheStore cache = lift $ do
  astCache <- asks store
  liftIO $ atomically $ writeTVar astCache cache
