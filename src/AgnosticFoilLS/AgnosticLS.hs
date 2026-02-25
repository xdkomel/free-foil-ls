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
{-# LANGUAGE NamedFieldPuns                 #-}

module AgnosticFoilLS.AgnosticLS where

import Common.LanguageServerCache
import Control.Monad.Foil
import Control.Monad.Free.Foil
import Control.Arrow 
-- import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server as LSP
import Language.LSP.Protocol.Lens ( textDocument, uri, params, newName, position )
import Control.Monad.Reader
import Data.Functor ( void )
import System.FilePath.Glob ( compile, globDir )
import Control.Lens ( ( ^. ), ( # ) )
import Data.Maybe (catMaybes, isJust)
import Unsafe.Coerce (unsafeCoerce)

data SomeTerm binder sig where
  SomeTerm :: Distinct n => AST binder sig n -> SomeTerm binder sig

data TermTelescope binder sig n where
  LeafTerm :: Distinct n => AST binder sig n -> TermTelescope binder sig n
  NodeTerm :: (Distinct n, DExt n l, CoSinkable binder) => AST binder sig n -> binder n l -> TermTelescope binder sig l -> TermTelescope binder sig n

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

firstJust :: [Maybe a] -> Maybe a
firstJust (j@(Just _):_) = j
firstJust (_:t) = firstJust t
firstJust [] = Nothing

buildCache 
  :: String 
  -> (String -> [AST binder sig n]) 
  -> LSP (AST binder sig n) ()
buildCache extension toAsts = do
  root <- getRootPath
  case root of
    Nothing ->
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Warning 
        $ T.pack "Cannot find the workspace root"
    Just rootPath -> do
      rawPaths <- liftIO $ globDir [compile ("*." ++ extension)] rootPath
      let paths = concat rawPaths
      asts <- liftIO $ mapM filePathToAst paths
      let 
        -- asts = concatMap unpackFilePathAstPair maybeAsts
          cache = Map.fromList $ map (second astToCache) asts
          -- cacheStr = show $ Map.map (printNode . SomeTerm . langAst) cache
      sendNotification SMethod_WindowShowMessage 
        $ ShowMessageParams MessageType_Info 
        $ T.pack 
        $ "Language server for '." 
          ++ extension 
          ++ "' is initialized"
      cacheStore $ LangStore cache
  where
    filePathToAst f = do
      input <- liftIO $ readFile f
      return (f, toAsts input)
    astToCache n = LangProgramStore { langAst = n }

nameString :: MethodFindName binder sig n -> (Int, Int) -> LangProgramStore (AST binder sig n) -> Maybe String
nameString findName pos = firstJust . map (fmap snd . findName pos) . langAst

-- symbolRange ::  MethodFindName binder sig n -> (Int, Int) -> LangProgramStore (AST binder sig n) -> Maybe Range
-- symbolRange findName pos = 

definitionRange
  :: MethodFindName binder sig n 
  -> MethodBuildTelescopes binder sig n
  -> MethodTermRange binder sig
  -> (Int, Int)
  -> LangProgramStore (AST binder sig n) 
  -> Maybe Range
definitionRange findName buildTelescopes termRange pos cache = do
  (name, ast) <- firstJust 
    $ map maybeFindName 
    $ langAst cache
  usage <- firstJust 
    $ map (definingTerm termIsValid emptyScope name) 
    $ buildTelescopes ast
  termRange usage
  where
    maybeFindName ast = fmap (\(n, _) -> (n, ast)) $ findName pos ast
    termIsValid = isJust . (findName pos) . unpackSomeTerm

showAST 
  :: MethodFindName binder sig n 
  -> MethodBuildTelescopes binder sig n
  -> (AST binder sig n -> String) 
  -> (Int, Int) 
  -> LangStore (AST binder sig n) 
  -> FilePath 
  -> Maybe String
showAST findName buildTelescopes printTerm position (LangStore cache) filePath = do
  fileCache <- Map.lookup filePath cache
  -- (name, ast) <- firstJust 
  --   $ map maybeFindName 
  --   $ langAst fileCache
  Just $ show $ map (("\n\n" ++) . printTerm) $ langAst fileCache
  -- Just $ "Found names: " ++ (show $ map (findName position) (langAst fileCache))
  -- usage <- firstJust 
  --   $ map (definingTerm termIsValid emptyScope name) 
  --   $ buildTelescopes ast
  -- Just $ "\n\n" ++ (printTerm usage) 
  -- where
  --   maybeFindName ast = fmap (\x -> (x, ast)) $ findName position ast
  --   termIsValid = isJust . (findName position) . unpackSomeTerm
  -- let asts = langAst fileCache
  --     names = concatMap maybeFindName asts
  -- case names of
  --   ((name, ast):_) ->
  --     let telescopes = buildTelescopes ast
  --         allUsages = map (definingTerm (isJust . (findName position) . unpackSomeTerm) emptyScope name) telescopes
  --         usages = catMaybes allUsages
      -- in Just $ "Definitions:\n\n" 
      --   ++ (concatMap ((++ "\n\n") . printTerm) usages)
      --   ++ "Telescopes:\n\n"
      --   ++ (concatMap ((++ "\n\n") . (printTelescope printTerm)) telescopes)
  --   _ -> Nothing
  

unpackSomeTerm :: SomeTerm binder sig -> AST binder sig m
unpackSomeTerm (SomeTerm a) = unsafeCoerce a

-- printTelescope :: (SomeTerm binder sig -> String) -> TermTelescope binder sig n -> String
-- printTelescope printTerm (LeafTerm n) = printTerm $ SomeTerm n
-- printTelescope printTerm (NodeTerm n binder next) = (printTerm $ SomeTerm n)
--   ++ " binding ["
--   ++ show binder
--   ++ "] ➡️ " 
--   ++ printTelescope printTerm next

definingTerm :: Distinct n 
  => (SomeTerm binder sig -> Bool) 
  -> Scope n 
  -> Name m 
  -> TermTelescope binder sig n 
  -> Maybe (SomeTerm binder sig)
definingTerm _ scope n _ | n `member` scope = Nothing
definingTerm _ _ _ (LeafTerm{}) = Nothing
definingTerm leafIsValid scope n (NodeTerm term binder scopedTele) = 
  let scope' = extendScopePattern binder scope
      isValid = case scopedTele of
        LeafTerm a -> leafIsValid $ SomeTerm a
        _ -> False
  in if isValid && n `member` scope'
    then Just (SomeTerm term)
    else definingTerm leafIsValid scope' n scopedTele
  
handlers :: LSConfiguration binder sig n -> Handlers (LSP (AST binder sig n))
handlers (LSConfiguration fileExtension buildAsts findName buildTelescopes termRange printTerm) = 
  let cacheBuilder = buildCache fileExtension buildAsts 
      astShower = showAST findName buildTelescopes printTerm
      -- tuplesToRange ((x1, y1), (x2, y2)) = Range 
      --   (Position (fromIntegral x1 - 1) (fromIntegral y1 - 1)) 
      --   (Position (fromIntegral x2 - 1) (fromIntegral y2 - 1))
      locateDefinition = definitionRange findName buildTelescopes termRange
      stringSym = nameString findName
  in mconcat
    [ notificationHandler SMethod_Initialized $ const $ cacheBuilder
    , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ const $ cacheBuilder
    , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        LangStore cache <- getCachedStore
        let parameters = req ^. params
            Position l r = parameters ^. position
            fileUri = parameters ^. textDocument . uri
            bnfcPosition = (fromIntegral l + 1, fromIntegral r + 1)
            maybeCurrentFile = uriToFilePath fileUri
            maybeRange = maybeCurrentFile 
              >>= lookupFile cache
              >>= locateDefinition bnfcPosition
        sendNotification SMethod_WindowShowMessage 
          $ ShowMessageParams MessageType_Info 
          $ T.pack 
          $ "Range identified: " ++ show maybeRange
        let maybeLocation = fmap (Location fileUri) maybeRange
            -- location = maybe (Location fileUri (Range (Position 1 1) (Position 1 3))) id maybeLocation
        -- currentFile <- uriToFilePath fileUri
        -- range <- liftIO $ locateDefinition intPos cache currentFile
        -- maybeCurrentFile >>= locateDefinition intPos cache 
        -- $ Right $ InL $ Definition $ InL location
        responder 
          $ maybeToEither (responseError "Did not find the definition") 
          $ fmap (InL . Definition . InL) maybeLocation
        -- case maybeRange of
        --   Just range -> responder $ Right $ InL $ Definition $ InL $ Location fileUri range
        --   Nothing -> responder $ Left (TResponseError (InL LSPErrorCodes_RequestFailed) (T.pack "") Nothing)
        -- responder $ fmap (InL . Definition . InL . Location fileUri) maybeRange
        -- responder $ Right $ InL $ Definition $ InL $ Location fileUri range
        -- Location (Uri { getUri = T.pack "" }) (Range (Position 0 0) (Position 0 0))
        -- DefinitionLink $ LocationLink Nothing (Uri { getUri = T.pack "" }) (Range (Position 0 0) (Position 0 0)) (Range (Position 0 0) (Position 0 0))
        --  DefinitionParams (TextDocumentIdentifier "") (Position 0 0) Nothing Nothing
    -- , requestHandler SMethod_TextDocumentRename $ \req responser -> do
    --     cache <- getCachedStore
    --     let TRequestMessage _ _ _ (RenameParams) = req
    , requestHandler SMethod_TextDocumentRename $ \req responder -> do
        let parameters = req ^. params
            Position l r = parameters ^. position
            newSymName = parameters ^. newName
            fileUri = parameters ^. textDocument . uri
        vdoc <- getVersionedTextDoc $ parameters ^. textDocument
        LangStore cache <- getCachedStore
        let bnfcPosition = (fromIntegral l + 1, fromIntegral r + 1)
            maybeCache = uriToFilePath fileUri >>= lookupFile cache
            maybeRange = maybeCache >>= locateDefinition bnfcPosition
            maybeName = maybeCache >>= stringSym bnfcPosition
        sendNotification SMethod_WindowShowMessage 
          $ ShowMessageParams MessageType_Info 
          $ T.pack 
          $ "Range identified: " ++ show maybeRange
        let selectionLen = length maybeName
            toTextEdit x y = InL . TextEdit (mkRange x y x (y + (fromIntegral selectionLen)))
            originalEdit = toTextEdit l r newSymName
            definitionEdit = maybe [] (\a -> [a]) 
              $ fmap (\(Range (Position x y) _) -> toTextEdit x y newSymName) maybeRange
            tde = TextDocumentEdit (_versionedTextDocumentIdentifier # vdoc) 
              $ [originalEdit] ++ definitionEdit
            rsp = WorkspaceEdit Nothing (Just [InL tde]) Nothing
        responder $ Right $ InL rsp
    , requestHandler SMethod_TextDocumentPrepareRename $ \req responder -> do
        LangStore cache <- getCachedStore
        let parameters = req ^. params
            Position l r = parameters ^. position
            fileUri = parameters ^. textDocument . uri
            bnfcPosition = (fromIntegral l + 1, fromIntegral r + 1)
            maybeCurrentFile = uriToFilePath fileUri
            maybeRange = maybeCurrentFile 
              >>= lookupFile cache
              >>= locateDefinition bnfcPosition
        sendNotification SMethod_WindowShowMessage 
          $ ShowMessageParams MessageType_Info 
          $ T.pack 
          $ "Range identified: " ++ show maybeRange
        responder 
          $ maybeToEither (responseError "Did not find the definition") 
          $ fmap (InL . PrepareRenameResult . InR . InL . (\r -> PrepareRenamePlaceholder r (T.pack "placeholder"))) maybeRange
    , requestHandler SMethod_TextDocumentLinkedEditingRange $ \req responder -> do
        LangStore cache <- getCachedStore
        let parameters = req ^. params
            Position l r = parameters ^. position
            fileUri = parameters ^. textDocument . uri
            bnfcPosition = (fromIntegral l + 1, fromIntegral r + 1)
            maybeCurrentFile = uriToFilePath fileUri
            maybeRange = maybeCurrentFile 
              >>= lookupFile cache
              >>= locateDefinition bnfcPosition
        sendNotification SMethod_WindowShowMessage 
          $ ShowMessageParams MessageType_Info 
          $ T.pack 
          $ "Range identified: " ++ show maybeRange
        responder
          $ maybeToEither (responseError "Did not find the definition") 
          $ fmap (InL . (\r -> LinkedEditingRanges [r] Nothing)) maybeRange
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        cache <- getCachedStore
        let TRequestMessage _ _ _ (HoverParams _ position _) = req
            maybeCurrentFile = uriToFilePath $ req ^. params . textDocument . uri
            (Position l r) = position
            intPos = (fromIntegral l + 1, fromIntegral r + 1)
            maybeAst = maybeCurrentFile >>= astShower intPos cache
            atLine = "\n at " ++ show intPos
            ms = mkMarkdown $ T.pack $ maybe
              ("Nothing found" ++ atLine)
              (++ atLine)
              maybeAst
            rsp = Hover (InL ms) (Just $ Range position position)
        responder $ Right $ InL rsp
    ]
  where
    lookupFile cache x = Map.lookup x cache
    responseError comment = TResponseError (InL LSPErrorCodes_RequestFailed) (T.pack comment) Nothing

type MethodBuildAsts binder sig n = String -> [AST binder sig n]
type MethodFindName binder sig n = (Int, Int) -> AST binder sig n -> forall m. Maybe (Name m, String)
type MethodBuildTelescopes binder sig n = AST binder sig n -> [TermTelescope binder sig VoidS]
type MethodTermRange binder sig = SomeTerm binder sig -> Maybe Range

data LSConfiguration binder sig n = LSConfiguration
  { fileExtension :: String
  , buildAsts :: MethodBuildAsts binder sig n
  , findName :: MethodFindName binder sig n
  , buildTelescopes :: MethodBuildTelescopes binder sig n
  , termRange :: MethodTermRange binder sig
  -- TODO: Remove
  , printTerm :: AST binder sig n -> String
  }

runAgnosticLanguageServer :: LSConfiguration binder sig n -> IO ()
runAgnosticLanguageServer config = do
  langEnv <- defaultLangEnv
  void $ runServer
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = T.pack "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers config
      , interpretHandler = \env -> Iso (flip runReaderT langEnv . runLspT env) liftIO
      , options = LSP.defaultOptions
      }
