{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , LambdaCase
           , ViewPatterns
           , FlexibleContexts
           , FlexibleInstances
           , BlockArguments
           , ImportQualifiedPost
           , NamedFieldPuns
           , TemplateHaskell
           , UndecidableInstances
           , RankNTypes
           , TypeApplications
#-}

module Main where

import RIO

import Options.Applicative as Opt hiding (action)
import Lens.Micro.Platform

import Graphics.UI.GLFW qualified as GLFW
-- import Vulkan

data AppException
  = GLFWInitError
  | GLFWWindowError

instance Exception AppException

instance Show AppException where
  show = \case
    GLFWInitError -> "Failed to initialize GLFW."
    GLFWWindowError -> "Failed to create window."

data WindowSize = MkWindowSize { _width  :: !Natural
                               , _height :: !Natural
                               }

-- Not using makeClassy since its windowSize method would clash with HasConfig
makeLenses ''WindowSize

data Options = MkOptions { optWidth   :: !Natural
                         , optHeight  :: !Natural
                         , optVerbose :: !Bool
                         }

data Config = MkConfig { _windowSize :: !WindowSize }

makeClassy ''Config

data App = MkApp { appLogFunc :: !LogFunc
                 , appConfig  :: !Config
                 }
class (HasLogFunc env, HasConfig env) => HasApp env where
  env :: Lens' env App
instance HasLogFunc App where
  logFuncL = lens appLogFunc \x y -> x {appLogFunc = y}
instance HasConfig App where
  config = lens appConfig \x y -> x {appConfig = y}
instance HasApp App where
  env = id

mkConfig :: Options -> Config
mkConfig (MkOptions w h _) = MkConfig (MkWindowSize w h)

options :: Parser Options
options = MkOptions
  <$> option auto
      ( long "width"
     <> help "The width of the window"
     <> showDefault
     <> Opt.value 800
     <> metavar "WIDTH")
  <*> option auto
      ( long "height"
     <> help "The height of the window"
     <> showDefault
     <> Opt.value 600
     <> metavar "HEIGHT")
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print verbose log messages")

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) fullDesc)
  logOptions <- setLogUseTime True <$> logOptionsHandle stdout (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let app = MkApp { appLogFunc = logFunc
                    , appConfig = mkConfig opts
                    }
    runRIO app $ catch run \e -> do
      logError $ displayShow @SomeException e
      exitFailure

withGLFW :: RIO env a -> RIO env a
withGLFW action = bracket
  do liftIO GLFW.init
  do const $ liftIO GLFW.terminate
  \case success | success   -> action
                | otherwise -> throwIO GLFWInitError

withWindow :: HasConfig env => (GLFW.Window -> RIO env a) -> RIO env a
withWindow action = do
  size <- view $ config.windowSize
  bracket
    do liftIO do
        mapM_ GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                              , GLFW.WindowHint'Resizable False
                              ]
        let asInt l = fromIntegral $ size^.l
        GLFW.createWindow (asInt width) (asInt height) "Vulkan" Nothing Nothing
    do liftIO . mapM_ GLFW.destroyWindow
    do maybe (throwIO GLFWWindowError) action

mainLoop :: HasLogFunc env => GLFW.Window -> RIO env ()
mainLoop window = do
  liftIO GLFW.pollEvents
  threadDelay 1000
  unlessM (liftIO $ GLFW.windowShouldClose window) $ mainLoop window

run :: HasApp env => RIO env ()
run = do
  logInfo "Started app"

  withGLFW $ withWindow mainLoop

  logInfo "Goodbye!"
