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

module FoilLS.FoilLS
    ( runFoilLanguageServer
    ) where

import FoilLS.LanguageServerCache
import Common.LambdaPi
import qualified Control.Monad.Foil              as Foil
import           Control.Monad.Free.Foil
import Data.Bifunctor (second)
import qualified Data.Map                        as Map
import qualified Lampi.AbsLampi as Raw
import qualified Lampi.LayoutLampi as Raw
import qualified Lampi.LexLampi as Raw
import qualified Lampi.ParLampi as Raw
-- import Data.IntervalMap (Interval(..))
-- import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server as LSP
import Language.LSP.Protocol.Lens ( textDocument, uri, params )
import Control.Monad.Reader
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ) )
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (catMaybes)

buildCache :: LSP ()
buildCache = do
  root <- getRootPath
  case root of
    Nothing ->
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Warning 
        $ T.pack "Cannot find the workspace root"
    Just rootPath -> do
      rawPaths <- liftIO $ globDir [compile "*.lampi"] rootPath
      let paths = concat rawPaths
      maybeAsts <- liftIO $ mapM filePathToAst paths
      let programs = concatMap 
            (\(f, p) -> maybe [] (\x -> [(f, x)]) p) 
            maybeAsts
          cache = Map.fromList $ map (second programToCache) programs
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Info 
        $ T.pack 
        $ "Initialized"
      cacheStore $ LampiStore cache
  where
    filePathToAst f = do
      input <- liftIO $ readFile f
      let tokens = Raw.resolveLayout True $ Raw.tokens input 
          maybeProgram = either (\_ -> Nothing) Just (Raw.pProgram tokens)
      return (f, maybeProgram)
    programToCache p = 
      let asts = buildAsts p
      in LampiProgramStore
        { lampiAsts = asts
        -- , lampiNames = buildNamesIM asts
        }

buildAsts :: Raw.Program -> [LambdaPi ASTAnn Foil.VoidS]
buildAsts = \case
  Raw.AProgram _ cmds -> concatMap cmdToLambdaPi cmds

-- buildCache :: Raw.Program -> [(Int, Interval Int, ASTAnn)]
-- buildCache = \case
--   Raw.AProgram _ cmds -> 
--     concatMap buildIntervals $ concatMap cmdToLambdaPi cmds

cmdToLambdaPi :: Raw.Command -> [LambdaPi ASTAnn Foil.VoidS]
cmdToLambdaPi = \case
  (Raw.CommandCompute _ t1 t2) -> 
    map (toLambdaPi Foil.emptyScope Map.empty) [t1, t2]
  (Raw.CommandCheck _ t1 t2) -> 
    map (toLambdaPi Foil.emptyScope Map.empty) [t1, t2]

-- buildNamesIM 
--   :: [LambdaPi ASTAnn Foil.VoidS] 
--   -> Map.Map Int (IM.IntervalMap (Interval Int) (Foil.Name Foil.VoidS))
-- buildNamesIM nodes = 
--   let positionedNames = concatMap (foldLambdaPi namesForNode []) nodes
--       imEntry (x, i, n) = (x, IM.fromList [(i, n)])
--   in Map.fromListWith IM.union $ map imEntry positionedNames
--   where
--     namesForNode 
--       :: [(Int, Interval Int, Foil.Name Foil.VoidS)] 
--       -> LambdaPi ASTAnn Foil.VoidS 
--       -> [(Int, Interval Int, Foil.Name Foil.VoidS)]
--     namesForNode list = \case
--       AVar position (Var name) nameStr -> list 
--         ++ maybe [] (nameInterval name nameStr) position
--       _ -> list
--     nameInterval :: Foil.Name Foil.VoidS -> String -> (Int, Int) -> [(Int, Interval Int, (Foil.Name Foil.VoidS))]
--     nameInterval name nameStr (line, col) = [(line, (IntervalCO col (col + length nameStr)), name)]

-- cmdToLambdaPi :: Raw.Command -> [LambdaPi (ExtendedAnnotation Raw.BNFC'Position NameDefinition) Foil.VoidS]
-- cmdToLambdaPi = \case
  -- (Raw.CommandCompute _ t1 t2) -> 
  --   map (toLambdaPi Foil.emptyScope buildAnn Map.empty) [t1, t2]
  -- (Raw.CommandCheck _ t1 t2) -> 
  --   map (toLambdaPi Foil.emptyScope buildAnn Map.empty) [t1, t2]
--   where
--     buildAnn :: Raw.BNFC'Position -> String -> NameDefinition
--     buildAnn maybePos name = 
--       fmap (\(line, col) -> (line, (IntervalCO col (col + length name)))) maybePos

-- buildIntervals 
--   :: LambdaPi (ExtendedAnnotation Raw.BNFC'Position NameDefinition) n
--   -> [(Int, Interval Int, ASTAnn)]
-- buildIntervals = \case
--   AVar (ExtendedAnnotation maybePos nameDef) (Var name) -> case maybePos of
--     Nothing -> []
--     Just (line, col) -> 
--       let ann = ASTAnn 
--             { maybeNamePosition = maybePos
--             , maybeNameDef = nameDef
--             }
--           endCol = col + (length $ ppName name)
--       in [(line, IntervalCO col endCol, ann)]
--   App _ fun arg -> buildIntervals fun ++ buildIntervals arg
--   Lam _ _ t -> buildIntervals t
--   Pi _ _ a b -> buildIntervals a ++ buildIntervals b
--   Pair _ l r -> buildIntervals l ++ buildIntervals r
--   First _ t -> buildIntervals t
--   Second _ t -> buildIntervals t
--   Product _ l r -> buildIntervals l ++ buildIntervals r
--   Universe{} -> []
--   ErrorUnbound (ExtendedAnnotation maybePos _) name -> 
--     case maybePos of
--       Nothing -> []
--       Just (line, col) -> 
--         let ann = ASTAnn 
--               { maybeNamePosition = maybePos
--               , maybeNameDef = Nothing
--               }
--             endCol = col + length name
--         in [(line, IntervalCO col endCol, ann)]
--   ErrorUnsupported _ -> []
--   _ -> []
--   where
--     ppName :: Foil.Name n -> String
--     ppName name = "x" ++ show (Foil.nameId name)

showAst :: (Int, Int) -> LampiStore ASTAnn Foil.VoidS -> FilePath -> Maybe String
showAst position (LampiStore cache) filePath = do
  fileCache <- Map.lookup filePath cache
  let asts = lampiAsts fileCache
      astsStr = concatMap ((++ "\n;") . ppLambdaPi) asts
  name <- firstJust $ map (findName position) asts
  let telescopes = concatMap buildTelescopes asts
      allUsages = map (definingTerm Foil.emptyScope name) telescopes
      usages = catMaybes allUsages
  Just $ "ASTS: " ++ astsStr 
    ++ "\n\nDefinitions: " ++ (concatMap ((++ ";\n") . printDefinition) usages)
  where
    printDefinition :: SomeTerm ASTAnn -> String
    printDefinition (SomeTerm lampi) = showLambdaPi lampi

  -- let row = lampiNames fileCache Map.! x
  --     candidates = IM.elems $ IM.containing row y
  -- if null candidates
  --   then Nothing
  --   else Just $ show $ map ((++ "\n") . show) candidates

  -- let asts = lampiAsts fileCache
  --     telescopes = concatMap (buildIntervals id) asts
  -- if null telescopes
  --   then Nothing
  --   else Just $ show $ concatMap ((++ "\n") . show) telescopes

findName :: (Int, Int) -> LambdaPi ASTAnn n -> Maybe (Foil.Name m)
findName position@(x, y) = \case
  (AVar varPos (Var name) nameStr) -> 
    varPos >>= (nameInterval (unsafeCoerce name) nameStr)
  (App _ fun arg) -> 
    firstJust (map (findName position) [fun, arg])
  (Lam _ _ _ body) -> findName position body
  (Pi _ _ _ pat body) -> 
    firstJust [findName position pat, findName position body]
  (Pair _ l r) -> 
    firstJust (map (findName position) [l, r])
  (First _ a) -> findName position a
  (Second _ a) -> findName position a
  (Product _ l r) -> 
    firstJust (map (findName position) [l, r]) 
  _ -> Nothing
  where
    nameInterval name nameStr (line, col) = 
      if x == line && y >= col && y < col + length nameStr
        then Just name
        else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust (j@(Just _):_) = j
firstJust (_:t) = firstJust t
firstJust [] = Nothing

data SomeTerm a where
  SomeTerm :: Foil.Distinct n => LambdaPi a n -> SomeTerm a

instance Show (SomeTerm a) where
  show (SomeTerm n) = ppLambdaPi n

data TermTelescope a n where
  LeafTerm :: LambdaPi a n -> TermTelescope a n
  NodeTerm :: Foil.Distinct l => LambdaPi a n -> Foil.NameBinder n l -> TermTelescope a l -> TermTelescope a n

instance Show (TermTelescope a n) where
  show (LeafTerm n) = ppLambdaPi n
  show (NodeTerm n _ tele) = ppLambdaPi n ++ " =>= " ++ show tele

-- data SomeTelescope a where
--   SomeTelescope :: Foil.Distinct n => TermTelescope a n -> SomeTelescope a

-- instance Show (SomeTelescope a) where
--   show (SomeTelescope n) = show n

definingTerm :: (Foil.Distinct n) => Foil.Scope n -> Foil.Name m -> TermTelescope a n -> Maybe (SomeTerm a)
definingTerm scope n _ | n `Foil.member` scope = Nothing
definingTerm _ _ (LeafTerm{}) = Nothing
definingTerm scope n (NodeTerm term binder scopedTele) = 
  let scope' = Foil.extendScopePattern binder scope
  in if n `Foil.member` scope'
        then Just (SomeTerm term)
        else definingTerm scope' n scopedTele

buildTelescopes :: Foil.Distinct n => LambdaPi a n -> [TermTelescope a n]
buildTelescopes t@AVar{} = [LeafTerm t]
buildTelescopes (App _ fun arg) = concatMap buildTelescopes [fun, arg]
buildTelescopes t@(Lam _ binder _ body) =
  case Foil.assertDistinct binder of
    Foil.Distinct -> [LeafTerm t] 
      ++ (map (NodeTerm t binder) $ buildTelescopes body)
buildTelescopes t@(Pi _ binder _ _ body) = 
  case Foil.assertDistinct binder of
    Foil.Distinct -> [LeafTerm t] 
      ++ (map (NodeTerm t binder) $ buildTelescopes body)
buildTelescopes (Pair _ l r) = concatMap buildTelescopes [l, r]
buildTelescopes (First _ body) = buildTelescopes body
buildTelescopes (Second _ body) = buildTelescopes body
buildTelescopes (Product _ l r) = concatMap buildTelescopes [l, r]
buildTelescopes _ = []



-- maybeDefinition :: (Int, Int) -> 

-- -- | Interpret a \(\lambda\Pi\) command.
-- interpretCommand :: Raw.Command -> IO ()
-- interpretCommand (Raw.CommandCompute _loc term _type) =
--       putStrLn ("  ↦ " ++ ppLambdaPi (whnf Foil.emptyScope (toLambdaPi Foil.emptyScope Map.empty term)))
-- -- #TODO: add typeCheck
-- interpretCommand (Raw.CommandCheck _loc _term _type) = putStrLn "check is not yet implemented"

-- -- | Interpret a \(\lambda\Pi\) program.
-- interpretProgram :: Raw.Program -> IO ()
-- interpretProgram (Raw.AProgram _loc typedTerms) = mapM_ interpretCommand typedTerms


handlers :: Handlers LSP
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ const buildCache
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const buildCache
  -- , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
  --     cache <- getCachedStore
  --     let TRequestMessage _ _ _ (DefinitionParams _ position _ _) = req
  --         maybeCurrentFile = uriToFilePath $ req ^. params . textDocument . uri
  --         (Position l r) = position
  --         intPos = (fromIntegral l + 1, fromIntegral r + 1)
  --         maybeAst = maybeCurrentFile >>= showAst intPos cache
  --         atLine = "\n at " ++ show intPos
  --         ms = mkMarkdown $ T.pack $ maybe
  --           ("Nothing found" ++ atLine)
  --           (++ atLine)
  --           maybeAst
  --         -- location = Location
  --         --   { uri = ""
  --         --   , range = Range () 
  --         --   }
  --         -- rsp = Definition location
  --          (InL ms) (Just $ Range position position)
  --     responder (Right $ InL rsp)
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
  -- do
  -- input <- readFile "/Users/kf/Documents/projects/uni/free-foil-ls/src/FoilLS/testlampi"
  -- case Raw.pProgram (Raw.resolveLayout True (Raw.tokens input)) of
  --   Left err -> do
  --     putStrLn err
  --     exitFailure
  --   Right program -> interpretProgram program

runFoilLanguageServer :: IO ()
runFoilLanguageServer = do
  lampiEnv <- defaultLampiEnv
  void $ runServer
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = T.pack "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (flip runReaderT lampiEnv . runLspT env) liftIO
      , options = LSP.defaultOptions
      }
