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
           , RankNTypes
           , TypeApplications
           , DuplicateRecordFields
#-}

module Main where

import RIO

-- import Options.Applicative as Opt hiding (action)
-- import System.Environment (getProgName)

-- import Graphics.UI.GLFW qualified as GLFW
-- -- import Vulkan

import TH

data WindowSize = MkWindowSize { windowWidth  :: !Natural
                               , windowHeight :: !Natural
                               }
makeRioClassy ''WindowSize

newtype Email = MkEmail { emailString :: String }
makeRioClassy ''Email

data Config = MkConfig { windowSize   :: !WindowSize
                       , verbose      :: !Bool
                       , studentEmail :: !Email
                       , teacherEmail :: !Email
                       }
makeRioClassy ''Config

data App = MkApp { config  :: !Config
                 , logFunc :: !LogFunc
                 }
makeRioClassy ''App

newtype OtherStuff = MkOtherStuff { stuffContent :: String }
makeRioClassy ''OtherStuff

data SuperApp = MkSuperApp { saApp        :: !App
                           , saOtherStuff :: !OtherStuff
                           }
makeRioClassy ''SuperApp

main :: IO ()
main = pure ()

-- data AppException
--   = GLFWInitError
--   | GLFWWindowError

-- instance Exception AppException

-- instance Show AppException where
--   show = \case
--     GLFWInitError -> "Failed to initialize GLFW."
--     GLFWWindowError -> "Failed to create window."

-- data WindowSize = MkWindowSize { width  :: !Natural
--                                , height :: !Natural
--                                }
-- class HasWindowSize env where
--   windowSizeL :: Lens' env WindowSize
--   RIO_DATA_FIELD(windowSizeL, Natural, widthL, width)
--   RIO_DATA_FIELD(windowSizeL, Natural, heightL, height)

-- instance HasWindowSize WindowSize where
--   windowSizeL = id

-- data Options = MkOptions { optWidth   :: !Natural
--                          , optHeight  :: !Natural
--                          , optVerbose :: !Bool
--                          }

-- data Config = MkConfig { windowSize :: !WindowSize }
-- RIO_BASIC_CLASS(HasWindowSize env, HasConfig, Config, configL)
-- RIO_BASIC_INSTANCE(HasWindowSize, Config, windowSizeL, windowSize)

-- data App = MkApp { appLogFunc :: !LogFunc
--                  , appConfig  :: !Config
--                  }
-- RIO_BASIC_CLASS((HasLogFunc env, HasConfig env), HasApp, App, appL)
-- RIO_TRANS_INSTANCE(HasWindowSize, App, windowSizeL, configL)
-- RIO_BASIC_INSTANCE(HasLogFunc, App, logFuncL, appLogFunc)
-- RIO_BASIC_INSTANCE(HasConfig, App, configL, appConfig)

-- mkConfig :: Options -> Config
-- mkConfig (MkOptions w h _) = MkConfig (MkWindowSize w h)

-- options :: Parser Options
-- options = MkOptions
--   <$> option auto
--       ( long "width"
--      <> help "The width of the window"
--      <> showDefault
--      <> Opt.value 800
--      <> metavar "WIDTH")
--   <*> option auto
--       ( long "height"
--      <> help "The height of the window"
--      <> showDefault
--      <> Opt.value 600
--      <> metavar "HEIGHT")
--   <*> switch
--       ( long "verbose"
--      <> short 'v'
--      <> help "Print verbose log messages")

-- main :: IO ()
-- main = do
--   opts <- execParser (info (options <**> helper) fullDesc)
--   logOptions <- logOptionsHandle stdout (optVerbose opts)
--   withLogFunc logOptions \logFunc -> do
--     let app = MkApp { appLogFunc = logFunc
--                     , appConfig = mkConfig opts
--                     }
--     runRIO app $ catch run \e -> do
--       logError $ displayShow @SomeException e
--       exitFailure

-- withGLFW :: RIO env a -> RIO env a
-- withGLFW action = bracket
--   do liftIO GLFW.init
--   do const $ liftIO GLFW.terminate
--   \case success | success   -> action
--                 | otherwise -> throwIO GLFWInitError

-- withWindow :: HasConfig env => (GLFW.Window -> RIO env a) -> RIO env a
-- withWindow action = do
--   size <- view windowSizeL
--   bracket
--     do liftIO do
--         mapM_ GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
--                               , GLFW.WindowHint'Resizable False
--                               ]
--         let asInt = fromIntegral . ($ size)
--         progName <- getProgName
--         GLFW.createWindow (asInt width) (asInt height) progName Nothing Nothing
--     do liftIO . mapM_ GLFW.destroyWindow
--     do maybe (throwIO GLFWWindowError) action

-- mainLoop :: HasLogFunc env => GLFW.Window -> RIO env ()
-- mainLoop window = do
--   liftIO GLFW.pollEvents
--   threadDelay 1000
--   unlessM (liftIO $ GLFW.windowShouldClose window) $ mainLoop window

-- run :: HasApp env => RIO env ()
-- run = do
--   logInfo "Started app"

--   withGLFW $ withWindow mainLoop

--   logInfo "Goodbye!"
