module Options where

import RIO

import Options.Applicative

import VulkanSetup.Types

data Options = MkOptions { optWidth            :: Natural
                         , optHeight           :: Natural
                         , optFullscreen       :: Bool
                         , optMonitorIndex     :: Natural
                         , optVerbose          :: Bool
                         , optValidationLayers :: Bool
                         , optDebug            :: Bool
                         }


mkConfig :: Options -> Dict HasConfig
mkConfig (MkOptions{..}) = Dict
  where ?windowWidth            = optWidth
        ?windowHeight           = optHeight
        ?fullscreen             = optFullscreen
        ?monitorIndex           = optMonitorIndex
        ?enableValidationLayers = optValidationLayers
        ?enableDebug            = optDebug

options :: Parser Options
options = MkOptions
  <$> option auto
      ( long "width"
     <> help "The width of the window in windowed mode"
     <> showDefault
     <> value 800
     <> metavar "WIDTH")
  <*> option auto
      ( long "height"
     <> help "The height of the window in windowed mode"
     <> showDefault
     <> value 600
     <> metavar "HEIGHT")
  <*> switch
      ( long "fullscreen"
     <> short 'f'
     <> help "Start in fullscreen (borderless windowed) mode")
  <*> option auto
      ( long "monitor"
     <> short 'm'
     <> help "Which monitor to use for fullscreen mode"
     <> showDefault
     <> value 0
     <> metavar "MONITOR_INDEX")
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print verbose log messages")
  <*> switch
      ( long "val-layers"
     <> short 'l'
     <> help "Enable Vulkan validation layers")
  <*> switch
      ( long "debug"
      <> short 'd'
      <> help "Enable debugging functionality for shaders")
