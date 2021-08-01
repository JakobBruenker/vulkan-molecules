{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , OverloadedLists
           , LambdaCase
           , MultiWayIf
           , ViewPatterns
           , FlexibleContexts
           , FlexibleInstances
           , BlockArguments
           , ImportQualifiedPost
           , TemplateHaskell
           , RankNTypes
           , TypeApplications
           , DerivingVia
           , DuplicateRecordFields
           , NamedFieldPuns
           , ScopedTypeVariables
           , DataKinds
#-}

module Main where

import RIO
import RIO.List qualified as L
import RIO.NonEmpty qualified as NE

import Control.Lens (views, (??))
import Control.Monad.Extra
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B.C
import Data.Vector qualified as V
import Options.Applicative hiding (action, info)
import Options.Applicative qualified as Opt
import System.Environment (getProgName)

import Graphics.UI.GLFW qualified as GLFW
import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     )
import Vulkan.Zero

import TH

data AppException
  = GLFWInitError
  | GLFWWindowError
  | VkGenericError Result
  | VkValidationLayersNotSupported (NonEmpty ByteString)

instance Exception AppException

instance Show AppException where
  show = \case
    GLFWInitError -> "Failed to initialize GLFW."
    GLFWWindowError -> "Failed to create window."
    VkGenericError r -> "Vulkan returned error: " <> show r
    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (L.intercalate ", " . map B.C.unpack . toList) ls
      where num = case ls of
              _ :| [] -> " is"
              _       -> "s are"

data Options = MkOptions { optWidth            :: !Natural
                         , optHeight           :: !Natural
                         , optVerbose          :: !Bool
                         , optValidationLayers :: !Bool
                         }

data WindowSize = MkWindowSize { windowWidth  :: !Natural
                               , windowHeight :: !Natural
                               }
makeRioClassy ''WindowSize

data Config = MkConfig { windowSize             :: !WindowSize
                       , enableValidationLayers :: !Bool
                       }
makeRioClassy ''Config

data App = MkApp { logFunc :: !LogFunc
                 , config  :: !Config
                 }
makeRioClassy ''App

data VulkanApp = MkVulkanApp { app    :: !App
                             , window :: !GLFW.Window
                             , inst   :: !Instance
                             -- , device :: !Device
                             }
makeRioClassy ''VulkanApp

mkConfig :: Options -> Config
mkConfig (MkOptions w h _ l) = MkConfig (MkWindowSize w h) l

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
  <*> switch
      ( long "val-layers"
     <> short 'l'
     <> help "Enable Vulkan validation layers")

main :: IO ()
main = do
  opts <- execParser (Opt.info (options <**> helper) fullDesc)
  logOptions <- logOptionsHandle stdout (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let app = MkApp { logFunc = logFunc
                    , config = mkConfig opts
                    }
    runRIO app $ catch runApp \e -> do
      logError $ displayShow @SomeException e
      exitFailure

withGLFW :: RIO env a -> RIO env a
withGLFW action = bracket
  do liftIO GLFW.init
  do const $ liftIO GLFW.terminate
  \case success | success   -> action
                | otherwise -> throwIO GLFWInitError

withWindow :: HasWindowSize env => (GLFW.Window -> RIO env a) -> RIO env a
withWindow action = do
  width <- views windowWidthL fromIntegral
  height <- views windowHeightL fromIntegral
  bracket
    do liftIO do
         mapM_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                   , GLFW.WindowHint'Resizable False
                                   ]
         progName <- getProgName
         GLFW.createWindow width height progName Nothing Nothing
    do liftIO . mapM_ GLFW.destroyWindow
    do maybe (throwIO GLFWWindowError) action

mainLoop :: HasVulkanApp env => RIO env ()
mainLoop = whileM do
  liftIO GLFW.waitEvents -- XXX might need pollEvents instead
  liftIO . fmap not . GLFW.windowShouldClose =<< view windowL

validationLayers :: HasEnableValidationLayers env => RIO env [ByteString]
validationLayers = view enableValidationLayersL >>= \case
    False -> pure []
    True  -> do
      enumerateInstanceLayerProperties >>= \case
        (SUCCESS, properties) -> do
          case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
            Nothing          -> pure layers
            Just unsupported -> throwIO (VkValidationLayersNotSupported unsupported)
        (err, _) -> throwIO (VkGenericError err)
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

runApp :: HasApp env => RIO env ()
runApp = do
  logDebug "Started app"

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  enabledLayerNames <- V.fromList <$> validationLayers
  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo :: InstanceCreateInfo '[] =
        zero{applicationInfo, enabledExtensionNames, enabledLayerNames}

  withInstance instanceCreateInfo Nothing bracket \inst ->
    withGLFW $ withWindow \window -> do
      logDebug "Successfully initialized GLFW and created window, instance, and device"
      mapRIO (\env -> MkVulkanApp (env^.appL) window inst) mainLoop

  logInfo "Goodbye!"
