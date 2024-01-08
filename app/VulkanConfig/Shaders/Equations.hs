{-# LANGUAGE QuasiQuotes #-}

module VulkanConfig.Shaders.Equations where

import RIO
import VulkanConfig.Shaders.ADiff

-- The equations have to live in a separate module due to GHC's stage restriction

fbo' :: Expr Scalar Float
fbo' = [expr| exp (pa * (r / ro)**pb) |]
