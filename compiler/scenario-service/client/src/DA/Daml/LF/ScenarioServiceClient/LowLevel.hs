-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module DA.Daml.LF.ScenarioServiceClient.LowLevel
  ( Options(..)
  , TimeoutSeconds
  , findServerJar
  , Handle
  , BackendError(..)
  , Error(..)
  , withScenarioService
  , ContextId
  , newCtx
  , cloneCtx
  , deleteCtx
  , gcCtxs
  , ContextUpdate(..)
  , LightValidation(..)
  , updateCtx
  , runScenario
  , ScenarioResult
  , encodeModule
  , ScenarioServiceException(..)
  ) where

import Conduit (runConduit, (.|), MonadUnliftIO(..))
import Control.Lens
import Data.Maybe
import Data.IORef
import GHC.Generics
import Text.Read
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified DA.Daml.LF.Proto3.EncodeV1 as EncodeV1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import Data.Conduit.Process
import qualified Data.Conduit.Text as C.T
import Data.Int (Int64)
import Data.List.Split (splitOn)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Message (defMessage)
-- import Data.ProtoLens.Service.Types (Service(..), HasMethod, HasMethodImpl(..))
import qualified Data.Text as T
import Network.HTTP2
import Network.HTTP2.Client
import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import qualified Proto3.Suite as Proto
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.IO
import System.Process (proc, CreateProcess, readCreateProcessWithExitCode)

import DA.Bazel.Runfiles
import qualified DA.Daml.LF.Ast as LF
import Proto.Compiler.ScenarioService.Protos.ScenarioService as Proto

-- import qualified Network.HTTP2 as HTTP2

data Options = Options
  { optServerJar :: FilePath
  , optRequestTimeout :: TimeoutSeconds
  , optGrpcMaxMessageSize :: Maybe Int
  , optLogInfo :: String -> IO ()
  , optLogError :: String -> IO ()
  }

type TimeoutSeconds = Int

data Handle = Handle
  { hClient :: GrpcClient
  , hOptions :: Options
  }

newtype ContextId = ContextId { getContextId :: Int64 }
  deriving (NFData, Eq, Show)

-- | If true, the scenario service server only runs a subset of validations.
newtype LightValidation = LightValidation { getLightValidation :: Bool }

data ContextUpdate = ContextUpdate
  { updLoadModules :: ![(LF.ModuleName, BS.ByteString)]
  , updUnloadModules :: ![LF.ModuleName]
  , updLoadPackages :: ![(LF.PackageId, BS.ByteString)]
  , updUnloadPackages :: ![LF.PackageId]
  , updDamlLfVersion :: LF.Version
  , updLightValidation :: LightValidation
  }

encodeModule :: LF.Version -> LF.Module -> BS.ByteString
encodeModule version m = case version of
    LF.V1{} -> BSL.toStrict (Proto.toLazyByteString (EncodeV1.encodeModuleWithLargePackageIds version m))

data BackendError
  = BErrorFail ErrorCode
  | BErrorClient ClientError
  | BTooMuchConcurrency TooMuchConcurrency
  | BErrorOther String
  deriving Show

data Error
  = ScenarioError ScenarioError
  | BackendError BackendError
  | ExceptionError SomeException
  deriving (Generic, Show)

instance NFData Error where
    rnf = rwhnf

findServerJar :: IO FilePath
findServerJar = do
  runfilesDir <- locateRunfiles (mainWorkspace </> "compiler/scenario-service/server")
  pure (runfilesDir </> "scenario-service.jar")

-- | Return the 'CreateProcess' for running java.
-- Uses 'java' from JAVA_HOME if set, otherwise calls java via
-- /usr/bin/env. This is needed when running under "bazel run" where
-- JAVA_HOME is correctly set, but 'java' is not in PATH.
javaProc :: [String] -> IO CreateProcess
javaProc args =
  lookupEnv "JAVA_HOME" >>= return . \case
    Nothing ->
      proc "java" args
    Just javaHome ->
      let javaExe = javaHome </> "bin" </> "java"
      in proc javaExe args

data ScenarioServiceException = ScenarioServiceException String deriving Show

instance Exception ScenarioServiceException

validateJava :: Options -> IO ()
validateJava Options{..} = do
    getJavaVersion <- liftIO $ javaProc ["-version"]
    -- We could validate the Java version here but Java version strings are annoyingly
    -- inconsistent, e.g. you might get
    -- java version "11.0.2" 2019-01-15 LTS
    -- or
    -- openjdk version "1.8.0_181"
    -- so for now we only verify that "java -version" runs successfully.
    (exitCode, _stdout, stderr) <- readCreateProcessWithExitCode getJavaVersion "" `catch`
      (\(e :: IOException) -> throwIO (ScenarioServiceException ("Failed to run java: " <> show e)))
    case exitCode of
        ExitFailure _ -> throwIO (ScenarioServiceException ("Failed to start `java -version`: " <> stderr))
        ExitSuccess -> pure ()

-- | This is sadly not exposed by Data.Conduit.Process.
terminateStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ()
terminateStreamingProcess = liftIO . terminateProcess . streamingProcessHandleRaw

-- | Variant of withCheckedProcessCleanup that gives access to the
-- StreamingProcessHandle.
withCheckedProcessCleanup'
    :: ( InputSource stdin
       , OutputSink stderr
       , OutputSink stdout
       , MonadUnliftIO m
       )
    => CreateProcess
    -> (StreamingProcessHandle -> stdin -> stdout -> stderr -> m b)
    -> m b
withCheckedProcessCleanup' cp f = withRunInIO $ \run -> bracket
    (streamingProcess cp)
    (\(_, _, _, sph) -> closeStreamingProcessHandle sph)
    $ \(x, y, z, sph) -> do
        res <- run (f sph x y z) `onException` terminateStreamingProcess sph
        ec <- waitForStreamingProcess sph
        if ec == ExitSuccess
            then return res
            else throwIO $ ProcessExitedUnsuccessfully cp ec

handleCrashingScenarioService :: IORef Bool -> StreamingProcessHandle -> IO a -> IO a
handleCrashingScenarioService exitExpected h act =
    -- `race` doesn’t quite work here since we might end up
    -- declaring an expected exit at the very end as a failure.
    -- In particular, once we close stdin of the scenario service
    -- `waitForStreamingProcess` can return before `act` returns.
    -- See https://github.com/digital-asset/daml/pull/1974.
    withAsync (waitForStreamingProcess h) $ \scenarioProcess ->
    withAsync act $ \act' -> do
        r <- waitEither scenarioProcess act'
        case r of
            Right a -> pure a
            Left _ -> do
                expected <- readIORef exitExpected
                if expected
                   then wait act'
                   else fail "Scenario service exited unexpectedly"

withScenarioService :: Options -> (Handle -> IO a) -> IO a
withScenarioService opts@Options{..} f = do
  optLogInfo "Starting scenario service..."
  serverJarExists <- doesFileExist optServerJar
  unless serverJarExists $
      throwIO (ScenarioServiceException (optServerJar <> " does not exist."))
  validateJava opts
  cp <- javaProc (["-jar" , optServerJar] <> maybeToList (show <$> optGrpcMaxMessageSize))
  exitExpected <- newIORef False
  let closeStdin hdl = do
          atomicWriteIORef exitExpected True
          System.IO.hClose hdl
  withCheckedProcessCleanup' cp $ \processHdl (stdinHdl :: System.IO.Handle) stdoutSrc stderrSrc ->
          flip finally (closeStdin stdinHdl) $ handleCrashingScenarioService exitExpected processHdl $ do
    let splitOutput = C.T.decode C.T.utf8 .| C.T.lines
    let printStderr line
            -- The last line should not be treated as an error.
            | T.strip line == "ScenarioService: stdin closed, terminating server." =
              liftIO (optLogInfo (T.unpack ("SCENARIO SERVICE STDERR: " <> line)))
            | otherwise =
              liftIO (optLogError (T.unpack ("SCENARIO SERVICE STDERR: " <> line)))
    let printStdout line = liftIO (optLogInfo (T.unpack ("SCENARIO SERVICE STDOUT: " <> line)))
    -- stick the error in the mvar so that we know we won't get an BlockedIndefinitedlyOnMvar exception
    portMVar <- newEmptyMVar
    let handleStdout = do
          mbLine <- C.await
          case mbLine of
            Nothing ->
              liftIO (putMVar portMVar (Left "Stdout of scenario service terminated before we got the PORT=<port> message"))
            Just (T.unpack -> line) ->
              case splitOn "=" line of
                ["PORT", ps] | Just (p :: Int) <- readMaybe ps ->
                  liftIO (putMVar portMVar (Right p)) >> C.awaitForever printStdout
                _ -> do
                  liftIO (optLogError ("Expected PORT=<port> from scenario service, but got '" <> line <> "'. Ignoring it."))
                  handleStdout
    withAsync (runConduit (stderrSrc .| splitOutput .| C.awaitForever printStderr)) $ \_ ->
        withAsync (runConduit (stdoutSrc .| splitOutput .| handleStdout)) $ \_ ->
        -- The scenario service will shut down cleanly when stdin is closed so we do this at the end of
        -- the callback. Note that on Windows, killThread will not be able to kill the conduits
        -- if they are blocked in hGetNonBlocking so it is crucial that we close stdin in the
        -- callback or withAsync will block forever.
        flip finally (closeStdin stdinHdl) $ do
            System.IO.hFlush System.IO.stdout
            port <- either fail pure =<< takeMVar portMVar
            liftIO $ optLogInfo $ "Scenario service backend running on port " <> show port
            -- Using 127.0.0.1 instead of localhost helps when our packaging logic falls over
            -- and DNS lookups break, e.g., on Alpine linux.
            let grpcConfig =
                    (grpcClientConfigSimple "127.0.0.1" (fromIntegral port) False) { _grpcClientConfigTimeout = Timeout 5 }
            -- putStrLn "setting up grpc client"
            -- conn <- asException $ newHttp2FrameConnection "127.0.0.1" (fromIntegral port) Nothing
            -- let goAwayHandler m = liftIO $ do
            --         putStrLn "~~~goAway~~~"
            --         print m
            -- asException $ runHttp2Client (wrapConn conn) 8192 8192 [] goAwayHandler ignoreFallbackHandler $ \client -> do
            bracket (asException $ setupGrpcClient grpcConfig) (asException . close) $ \client -> do
                liftIO $ f Handle
                    { hClient = client
                    , hOptions = opts
                    }

-- wrapConn :: Http2FrameConnection -> Http2FrameConnection
-- wrapConn conn = conn {
--       _makeFrameClientStream = \sid ->
--           let frameClient = (_makeFrameClientStream conn) sid
--           in frameClient {
--                  _sendFrames = \mkFrames -> do
--                      xs <- mkFrames
--                      liftIO . print $ (">>> "::String, _getStreamId frameClient, map snd xs)
--                      _sendFrames frameClient (pure xs)
--              }
--     , _serverStream =
--         let
--           currentServerStrean = _serverStream conn
--         in
--           currentServerStrean {
--             _nextHeaderAndFrame = do
--                 hdrFrame@(hdr,_) <- _nextHeaderAndFrame currentServerStrean
--                 liftIO . print $ ("<<< "::String, HTTP2.streamId hdr, hdrFrame)
--                 return hdrFrame
--           }
--     }

asException :: ClientIO a -> IO a
asException a = do
    r <- runExceptT a
    case r of
        Left e -> throwIO e
        Right r -> pure r

fromRightA :: Applicative f => (a -> f b) -> Either a b -> f b
fromRightA f = either f pure

wrapResult :: ClientIO (Either TooMuchConcurrency (RawReply a)) -> ExceptT BackendError IO a
wrapResult a = do
    r <- liftIO $ runExceptT a
    r <- fromRightA (throwError . BErrorClient) r
    r <- fromRightA (throwError . BTooMuchConcurrency) r
    (_, _, r) <- fromRightA (throwError . BErrorFail) r
    fromRightA (throwError . BErrorOther) r

newCtx :: Handle -> IO (Either BackendError ContextId)
newCtx Handle{..} = runExceptT $ do
    liftIO $ putStrLn "newContext"
    r <- wrapResult $ rawUnary (RPC :: RPC ScenarioService "newContext") hClient
        defMessage
    liftIO $ putStrLn "oldContext"
    pure $ ContextId $ r ^. #contextId

cloneCtx :: Handle -> ContextId -> IO (Either BackendError ContextId)
cloneCtx Handle{..} (ContextId ctxId) = runExceptT $ do
    liftIO $ putStrLn "cloneContext"
    r <- wrapResult $ rawUnary (RPC :: RPC ScenarioService "cloneContext") hClient $
        defMessage & #contextId .~ ctxId
    liftIO $ putStrLn "clonedContext"
    pure $ ContextId $ r ^. #contextId

deleteCtx :: Handle -> ContextId -> IO (Either BackendError ())
deleteCtx Handle{..} (ContextId ctxId) = runExceptT $ do
    _ <- wrapResult $ rawUnary (RPC :: RPC ScenarioService "deleteContext") hClient $
        defMessage & #contextId .~ ctxId
    pure ()

gcCtxs :: Handle -> [ContextId] -> IO (Either BackendError ())
gcCtxs Handle{..} ctxIds = runExceptT $ do
    _ <- wrapResult $ rawUnary (RPC :: RPC ScenarioService "gccontexts") hClient $
        defMessage & #contextIds .~ map getContextId ctxIds
    pure ()

updateCtx :: Handle -> ContextId -> ContextUpdate -> IO (Either BackendError ())
updateCtx Handle{..} (ContextId ctxId) ContextUpdate{..} = runExceptT $ do
    liftIO $ putStrLn "updateCtx"
    _ <- liftIO $ evaluate $ force updModules
    liftIO $ putStrLn "evaluated 0"
    _ <- liftIO $ evaluate $ force updPackages
    liftIO $ putStrLn "evaluated 1"
    let msg = defMessage & #contextId .~ ctxId
                   & #updateModules .~ updModules
                   & #updatePackages .~ updPackages
                   & #lightValidation .~ getLightValidation updLightValidation
    _ <- liftIO $ evaluate $ force msg
    liftIO $ putStrLn "evaluated 2"
    _ <- wrapResult $ rawUnary (RPC :: RPC ScenarioService "updateContext") hClient msg
    liftIO $ putStrLn "updatedCtx"
    pure ()
  where
      updModules = defMessage
          & #loadModules .~ map convModule updLoadModules
          & #unloadModules .~ map encodeName updUnloadModules
      updPackages = defMessage
          & #loadPackages .~ map snd updLoadPackages
          & #unloadPackages .~ map LF.unPackageId updUnloadPackages
      encodeName = T.intercalate "." . LF.unModuleName
      convModule :: (LF.ModuleName, BS.ByteString) -> Module
      convModule (_, bytes) =
          case updDamlLfVersion of
              LF.V1 minor -> defMessage
                  & #minor .~ T.pack (LF.renderMinorVersion minor)
                  & #damlLf1 .~ bytes

runScenario :: Handle -> ContextId -> LF.ValueRef -> IO (Either Error ScenarioResult)
runScenario Handle{..} (ContextId ctxId) name = do
  putStrLn "runScenario"
  res <- runExceptT $ wrapResult $ rawUnary (RPC :: RPC ScenarioService "runScenario") hClient $
      defMessage & #contextId .~ ctxId
                 & #scenarioId .~ toIdentifier name
  putStrLn "ranScenario"
  pure $ case res of
    Left err -> Left (BackendError err)
    Right r -> case r ^. #maybe'response of
        Nothing -> fail "IMPOSSIBLE: missing payload in RunScenarioResponse"
        Just (RunScenarioResponse'Error err) -> Left (ScenarioError err)
        Just (RunScenarioResponse'Result r) -> Right r
  where
    toIdentifier :: LF.ValueRef -> Identifier
    toIdentifier (LF.Qualified pkgId modName defn) =
      let ssPkgId = case pkgId of
            LF.PRSelf     -> defMessage & #self .~ defMessage
            LF.PRImport x -> defMessage & #packageId .~ LF.unPackageId x
      in defMessage & #package .~ ssPkgId & #name .~ (T.intercalate "." (LF.unModuleName modName) <> ":" <> LF.unExprValName defn)
