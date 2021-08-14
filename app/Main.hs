{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , OverloadedLists
           , LambdaCase
           , MultiWayIf
           , ViewPatterns
           , TupleSections
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
           , TypeOperators
           , DataKinds
           , OverloadedLabels
           , UndecidableInstances
           , TypeFamilies
#-}

module Main where

import RIO
import RIO.ByteString qualified as B
import RIO.NonEmpty qualified as NE
import RIO.Text qualified as T
import RIO.Vector qualified as V
import Data.List.NonEmpty.Extra qualified as NE

import Control.Lens (views, (??))
import Control.Monad.Extra (whileM)
import Data.Bits
import Options.Applicative hiding (action, info)
import Options.Applicative qualified as Opt
import Relude.Extra.Tuple
import System.Environment (getProgName)

import Graphics.UI.GLFW qualified as GLFW
import Vulkan hiding ( MacOSSurfaceCreateInfoMVK(view)
                     , IOSSurfaceCreateInfoMVK(view)
                     , Display
                     , WaylandSurfaceCreateInfoKHR(display)
                     , DisplayPropertiesKHR(display)
                     , (:::)
                     )
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Has

data AppException
  = GLFWInitError
  | GLFWWindowError
  | VkGenericError Result
  | VkValidationLayersNotSupported (NonEmpty ByteString)
  | VkNoPhysicalDevicesError
  | VkNoSuitableDevicesError

instance Exception AppException

instance Show AppException where
  show = T.unpack . textDisplay

instance Display AppException where
  display = \case
    GLFWInitError -> "Failed to initialize GLFW."
    GLFWWindowError -> "Failed to create window."
    VkGenericError r -> "Vulkan returned error: " <> displayShow r
    VkValidationLayersNotSupported ls ->
      "Requested validation layer" <> num <> " not supported: " <>
        (displayBytesUtf8 . B.intercalate ", " . toList) ls <> "."
      where num = case ls of
              _ :| [] -> " is"
              _       -> "s are"
    VkNoPhysicalDevicesError -> "No physical devices found."
    VkNoSuitableDevicesError -> "No suitable devices found."

data Options = MkOptions { optWidth            :: !Natural
                         , optHeight           :: !Natural
                         , optVerbose          :: !Bool
                         , optValidationLayers :: !Bool
                         }

-- To keep track of whether GLFW is initialized
data GLFWToken s = UnsafeMkGLFWToken

type WindowSize = "windowWidth" ::: Natural >< "windowHeight" ::: Natural

type Config = WindowSize >< "enableValidationLayers" ::: Bool

type App = LogFunc >< Config

type VulkanApp = App >< GLFW.Window >< Instance >< Device

mkConfig :: Options -> Caps Config
mkConfig (MkOptions w h _ l) = #windowWidth .:= w
                            >< #windowHeight .:= h
                            >< #enableValidationLayers .:= l

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
  logOptions <- logOptionsHandle stderr (optVerbose opts)
  withLogFunc logOptions \logFunc -> do
    let app = logFunc >< mkConfig opts
    runRIO app $ catch runApp \e -> do
      mapRIO (view $ the @LogFunc) . logError $ display @SomeException e
      exitFailure

withGLFW :: (forall s . GLFWToken s -> RIO env a) -> RIO env a
withGLFW action = bracket
  do liftIO GLFW.init
  do const $ liftIO GLFW.terminate
  \case success | success   -> action UnsafeMkGLFWToken
                | otherwise -> throwIO GLFWInitError

withWindow :: Has WindowSize env => GLFWToken s -> (GLFW.Window -> RIO env a) -> RIO env a
withWindow _ action = do
  width <- views #windowWidth fromIntegral
  height <- views #windowHeight fromIntegral
  bracket
    do liftIO do
         mapM_ @[] GLFW.windowHint [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                                   , GLFW.WindowHint'Resizable False
                                   ]
         progName <- getProgName
         GLFW.createWindow width height progName Nothing Nothing
    do liftIO . mapM_ GLFW.destroyWindow
    do maybe (throwIO GLFWWindowError) action

mainLoop :: Has VulkanApp env => RIO env ()
mainLoop = whileM do
  liftIO GLFW.waitEvents -- XXX might need pollEvents instead
  liftIO . fmap not . GLFW.windowShouldClose =<< view it

validationLayers :: Has ("enableValidationLayers" ::: Bool) env => RIO env (Vector ByteString)
validationLayers = fmap V.fromList $ view #enableValidationLayers >>= bool (pure []) do
   properties <- catchVk enumerateInstanceLayerProperties
   case NE.nonEmpty $ filter (not . (properties `hasLayer`)) layers of
     Nothing          -> pure layers
     Just unsupported -> throwIO $ VkValidationLayersNotSupported unsupported
  where
    hasLayer props = V.elem ?? V.map layerName props
    layers = ["VK_LAYER_KHRONOS_validation"]

pickPhysicalDevice :: Has LogFunc env
                   -- => Instance -> RIO env (PhysicalDevice, "queueFamilyIndex" ::: Word32)
                   => Instance -> RIO env (PhysicalDevice, Word32) -- XXX return Tagged
pickPhysicalDevice inst = do
  catchVk (enumeratePhysicalDevices inst) <&> toList <&> NE.nonEmpty >>= maybe
    do throwIO VkNoPhysicalDevicesError
    \physDevs -> do
      let lds = length physDevs
      mapRIO (view $ the @LogFunc) .
        logDebug $ "Found " <> display lds <> " physical device" <> bool "s" "" (lds == 1)
      mapMaybeM (fmap sequence . traverseToSnd findQueueFamily) (toList physDevs) <&>
        NE.nonEmpty >>= maybe
          do throwIO VkNoSuitableDevicesError
          do (fst . NE.maximumOn1 snd <$>) . mapM (traverseToSnd $ score . fst)
  where
    score :: PhysicalDevice -> RIO env Integer
    score = (bool 1000 0 . (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU ==) . deviceType <$>) .
      getPhysicalDeviceProperties

    findQueueFamily :: PhysicalDevice -> RIO env (Maybe Word32)
    findQueueFamily physDev =
      fmap fromIntegral . V.findIndex (\p -> queueFlags p .&. QUEUE_GRAPHICS_BIT > zero) <$>
        getPhysicalDeviceQueueFamilyProperties physDev

catchVk :: MonadIO m => m (Result, a) -> m a
catchVk = (=<<) \case
  (SUCCESS, x) -> pure x
  (err    , _) -> throwIO $ VkGenericError err

-- runApp :: (Has App env) => RIO env () -- XXX I'd really like to keep this simple one
                                         --     Maybe with could replace args
                                         --     instead of requiring they don't
                                         --     already exist
runApp :: ( env ~ Caps caps
          , caps' ~ Caps (GLFW.Window >< Instance >< Device >< caps)
          , HasTag "enableValidationLayers" caps' Bool
          , HasTag "windowHeight" caps' Natural
          , HasTag "windowWidth" caps' Natural
          , Has App env
          , (Device >< Instance >< GLFW.Window) `NoOverlap` caps)
       => RIO env ()
runApp = do
  mapRIO (view $ the @LogFunc) $ logDebug "Started app"

  enabledExtensionNames <- V.fromList <$>
    do liftIO GLFW.getRequiredInstanceExtensions >>= liftIO . mapM B.packCString
  enabledLayerNames <- validationLayers
  let applicationInfo = Just (zero{apiVersion = MAKE_API_VERSION 1 0 0} :: ApplicationInfo)
      instanceCreateInfo =
        zero{applicationInfo, enabledExtensionNames, enabledLayerNames}

  withInstance instanceCreateInfo Nothing bracket \inst ->
    withGLFW $ withWindow ?? \window -> do
      (physDev, queueFamilyIndex) <- pickPhysicalDevice inst
      let queueCreateInfos = [SomeStruct zero{queueFamilyIndex, queuePriorities = [1]}]
          deviceCreateInfo = zero{queueCreateInfos, enabledLayerNames}
      withDevice physDev deviceCreateInfo Nothing bracket \device -> do
        mapRIO (view $ the @LogFunc) $
          logDebug "Successfully initialized GLFW and created window, instance, and device"
        mapRIO (with $ window >< inst >< device) mainLoop

  mapRIO (view $ the @LogFunc) $ logInfo "Goodbye!"
