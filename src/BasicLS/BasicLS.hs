{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module BasicLS.BasicLS
    ( runBasicLanguageServer
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Lens ( ( ^. ) )
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Functor (void)
import Data.Bifunctor
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Protocol.Lens ( textDocument, uri, params )
import System.FilePath.Glob (compile, globDir)
import Hsls.AbsHsls qualified as Ast
import Hsls.ParHsls ( pProgram, myLexer )
import Hsls.PrintHsls ( printTree )
import Data.IntervalMap (Interval(..))
import qualified Data.IntervalMap.Generic.Strict as IM

type ScopedPos = Map.Map String (Int, Int)

data HslsProgramStore a = HslsProgramStore
  { namesPositions :: Map.Map Int (IM.IntervalMap (Interval Int) (Ast.Name' a))
  } deriving (Show, Eq)

newtype HslsStore a = HslsStore (Map.Map FilePath (HslsProgramStore a)) deriving (Show, Eq)

newtype HslsEnv a = HslsEnv
  { hslsStore :: TVar (HslsStore a)
  } deriving (Eq)

defaultHslsEnv :: IO (HslsEnv ScopedPos)
defaultHslsEnv = do
  emptyCache <- newTVarIO $ HslsStore Map.empty
  return HslsEnv { hslsStore = emptyCache }

type LSP = LspT () (ReaderT (HslsEnv ScopedPos) IO)

getCachedStore :: LSP (HslsStore ScopedPos)
getCachedStore = lift $ do
  cache <- asks hslsStore
  liftIO $ readTVarIO cache

cacheStore :: HslsStore ScopedPos -> LSP ()
cacheStore cache = lift $ do
  astCache <- asks hslsStore
  liftIO $ atomically $ do
    writeTVar astCache cache

buildAsts :: LSP ()
buildAsts = do
  root <- getRootPath
  case root of
    Nothing ->
      sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Warning "Cannot find the workspace root")
    Just rootPath -> do
      rawPaths <- liftIO $ globDir [compile "*.hsls"] rootPath
      let paths = concat rawPaths
      maybeAsts <- liftIO $ mapM filePathToAst paths
      let programs = foldl collectSomePrograms [] maybeAsts
          cache = Map.fromList $ map (second programToCache) programs
      cacheStore $ HslsStore cache
      -- let asts = intercalate "\n" $ map (parseAndPrettyPrint . doc . showString) sources
      let msg = "Initialized\n" ++ show cache
      sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info $ T.pack msg)
  where
    collectSomePrograms a (f, p) = case p of
      Right pr -> (f, pr) : a
      Left _ -> a
    filePathToAst f = do
      contents <- liftIO $ readFile f
      return (f, (pProgram . myLexer) contents)
    programToCache p =
      let ranges = buildCache p
          namesPositions = Map.fromListWith IM.union 
            [ (x, IM.fromList [(i, n)]) | (x, i, n) <- ranges ]
      in HslsProgramStore
        { namesPositions = namesPositions }

buildCache :: Ast.Program' Ast.BNFC'Position -> [(Int, Interval Int, Ast.Name' ScopedPos)]
buildCache = \case
  Ast.AProgram _ decls -> 
    let tuples = buildCacheDecls $ map (mapAnnotation (, Map.empty)) decls
    in [ (a, b, mapAnnotation snd c) | (a, b, c) <- tuples ]
    where 
      namesMap :: [Ast.Name' (Ast.BNFC'Position, ScopedPos)] -> Map.Map String (Int, Int)
      namesMap = Map.fromList 
        . concatMap (\x -> 
          maybe [] (\p -> [(nameString x, p)]) (fst $ annotation x))

      scopedName :: Ast.Name' (Ast.BNFC'Position, ScopedPos) -> [(Int, Interval Int, Ast.Name' (Ast.BNFC'Position, ScopedPos))]
      scopedName node@(Ast.HslsName (maybePos, _) (Ast.LangIdent s)) = 
        maybe [] (\(x, y) -> [(x, IntervalCO y (y + length s), node)]) maybePos

      buildCacheDecls :: [Ast.Decl' (Ast.BNFC'Position, ScopedPos)] -> [(Int, Interval Int, Ast.Name' (Ast.BNFC'Position, ScopedPos))]
      buildCacheDecls (d:ds) = case d of
        Ast.DeclFun _ name args localDecls -> 
          let scopeArgs m = Map.unions [namesMap args, m]
              scopeDecls m = Map.unions [namesMap [name], m]
          in concatMap scopedName (name : args) 
            ++ buildCacheLocalDecls (map (mapAnnotation (second scopeArgs)) localDecls)
            ++ buildCacheDecls (map (mapAnnotation (second scopeDecls)) ds)
      buildCacheDecls [] = []
      
      buildCacheLocalDecls :: [Ast.Statement' (Ast.BNFC'Position, ScopedPos)] -> [(Int, Interval Int, Ast.Name' (Ast.BNFC'Position, ScopedPos))]
      buildCacheLocalDecls (d:ds) = case d of
        Ast.ReturnStatement _ e -> buildCacheExprs [e] ++ buildCacheLocalDecls ds
        Ast.ExprStatement _ e -> buildCacheExprs [e] ++ buildCacheLocalDecls ds
        Ast.AssignStatement _ lhs rhs -> 
          let scopeDecls m = Map.unions [namesMap [lhs], m]
          in scopedName lhs 
            ++ buildCacheExprs [rhs] 
            ++ buildCacheLocalDecls (map (mapAnnotation (second scopeDecls)) ds)
      buildCacheLocalDecls [] = []

      buildCacheExprs :: [Ast.Expr' (Ast.BNFC'Position, ScopedPos)] -> [(Int, Interval Int, Ast.Name' (Ast.BNFC'Position, ScopedPos))]
      buildCacheExprs (e:es) = case e of
        Ast.FnCall _ name exps -> scopedName name ++ buildCacheExprs (exps ++ es)
        Ast.Var _ name -> scopedName name ++ buildCacheExprs es
        Ast.Sum _ e1 e2 -> buildCacheExprs (e1 : e2 : es)
        Ast.Sub _ e1 e2 -> buildCacheExprs (e1 : e2 : es)
        Ast.Mul _ e1 e2 -> buildCacheExprs (e1 : e2 : es)
        Ast.Div _ e1 e2 -> buildCacheExprs (e1 : e2 : es)
        _ -> buildCacheExprs es
      buildCacheExprs [] = []

nameString :: Ast.Name' a -> String
nameString (Ast.HslsName _ (Ast.LangIdent s)) = s

class AnnotatedNode node where
  mapAnnotation :: (a -> b) -> node a -> node b
  annotation :: node a -> a

instance AnnotatedNode Ast.Program' where
  mapAnnotation f (Ast.AProgram ann decls) = 
    Ast.AProgram (f ann) (map (mapAnnotation f) decls)
  annotation (Ast.AProgram a _) = a

instance AnnotatedNode Ast.Decl' where
  mapAnnotation f (Ast.DeclFun ann name args body) = 
    let g = mapAnnotation f 
    in Ast.DeclFun (f ann) (g name) (map g args) (map (mapAnnotation f) body)
  annotation (Ast.DeclFun a _ _ _) = a

instance AnnotatedNode Ast.Statement' where
  mapAnnotation :: (a -> b) -> Ast.Statement' a -> Ast.Statement' b
  mapAnnotation f = \case
    (Ast.ReturnStatement ann e) -> 
      Ast.ReturnStatement (f ann) (mapAnnotation f e)
    (Ast.AssignStatement ann name exp2) -> 
      Ast.AssignStatement (f ann) (mapAnnotation f name) (mapAnnotation f exp2)
    (Ast.ExprStatement ann e) -> 
      Ast.ExprStatement (f ann) (mapAnnotation f e)
  annotation = \case
    (Ast.ReturnStatement a _) -> a
    (Ast.AssignStatement a _ _) -> a
    (Ast.ExprStatement a _) -> a

instance AnnotatedNode Ast.Expr' where
  mapAnnotation f = \case
    (Ast.FnCall ann name exps) -> 
      Ast.FnCall (f ann) (mapAnnotation f name) (map (mapAnnotation f) exps)
    (Ast.ConstTrue ann) -> Ast.ConstTrue (f ann)
    (Ast.ConstFalse ann) -> Ast.ConstFalse (f ann)
    (Ast.ConstInt ann n) -> Ast.ConstInt (f ann) n
    (Ast.ConstDouble ann n) -> Ast.ConstDouble (f ann) n
    (Ast.Var ann name) -> Ast.Var (f ann) (mapAnnotation f name)
    (Ast.Sum ann exp1 exp2) -> 
      let g = mapAnnotation f
      in Ast.Sum (f ann) (g exp1) (g exp2)
    (Ast.Sub ann exp1 exp2) -> 
      let g = mapAnnotation f
      in Ast.Sub (f ann) (g exp1) (g exp2)
    (Ast.Mul ann exp1 exp2) -> 
      let g = mapAnnotation f
      in Ast.Mul (f ann) (g exp1) (g exp2)
    (Ast.Div ann exp1 exp2) -> 
      let g = mapAnnotation f
      in Ast.Div (f ann) (g exp1) (g exp2)
  annotation = \case
    (Ast.FnCall a _ _) -> a
    (Ast.ConstTrue a) -> a
    (Ast.ConstFalse a) -> a
    (Ast.ConstInt a _) -> a
    (Ast.ConstDouble a _) -> a
    (Ast.Var a _) -> a
    (Ast.Sum a _ _) -> a
    (Ast.Sub a _ _) -> a
    (Ast.Mul a _ _) -> a
    (Ast.Div a _ _) -> a

instance AnnotatedNode Ast.Name' where
  mapAnnotation f (Ast.HslsName ann n) = Ast.HslsName (f ann) n
  annotation (Ast.HslsName a _) = a


showAst :: (Int, Int) -> HslsStore ScopedPos -> FilePath -> Maybe String
showAst (x, y) (HslsStore cache) filePath = do
  fileCache <- Map.lookup filePath cache
  let row = namesPositions fileCache Map.! x
      candidates = IM.elems $ IM.containing row y
  case candidates of
    [n] -> Just $ printTree n ++ "\nWith scope at " ++ show (annotation n)
    _ -> Nothing


handlers :: Handlers LSP
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ const buildAsts
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const buildAsts
  , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        cache <- getCachedStore
        let TRequestMessage _ _ _ (HoverParams _ position _) = req
            maybeCurrentFile = uriToFilePath $ req ^. params . textDocument . uri
            (Position l r) = position
            intPos = (fromIntegral l + 1, fromIntegral r + 1)
            maybeAst = maybeCurrentFile >>= showAst intPos cache
            atLine = "\n at " ++ show intPos
            ms = mkMarkdown $ T.pack $ maybe
              ("Nothing found" ++ atLine)
              (++ atLine)
              maybeAst
            rsp = Hover (InL ms) (Just $ Range position position)
        responder (Right $ InL rsp)
  ]

runBasicLanguageServer :: IO ()
runBasicLanguageServer = do
  hslsEnv <- defaultHslsEnv
  void $ runServer
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (flip runReaderT hslsEnv . runLspT env) liftIO
      , options = defaultOptions
      }

