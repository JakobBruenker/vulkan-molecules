{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

type VertexData =
  '[ Slot 0 0 ':-> V 2 Float
   , Slot 1 0 ':-> V 3 Float
   ]

type VertexDefs =
  '[ "position"  ':-> Input      '[Location 0] (V 2 Float)
   , "color"     ':-> Input      '[Location 1] (V 3 Float)
   , "pointSize" ':-> Output     '[Location 0] Float
   , "vertColor" ':-> Output     '[Location 1] (V 4 Float)
   , "main"      ':-> EntryPoint '[          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  position <- #position
  #gl_Position .= Vec4 (view @(Swizzle "x") position) (view @(Swizzle "y") position) 0 0
  color <- #color
  #vertColor .=
    Vec4 (view @(Swizzle "x") color) (view @(Swizzle "y") color) (view @(Swizzle "z") color) 1
  #pointSize .= 40
  #gl_PointSize .= 40

type FragmentDefs =
  '[ "pointSize" ':-> Input      '[Location 0     ] Float
   , "vertColor" ':-> Input      '[Location 1     ] (V 4 Float)
   , "color"     ':-> Output     '[Location 0     ] (V 4 Float)
   , "main"      ':-> EntryPoint '[OriginUpperLeft] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  pointSize <- #pointSize
  pCoord <- #gl_PointCoord
  let col = Vec4 (1 - pointSize / 50) (pointSize / 50) 0.5 1

  -- Limit alpha to a disk with darkened limb
  #color .= set @(Index 3) (1 - squaredNorm (pCoord ^* 2 ^-^ Vec2 1 1)) col

-- Currently unused. Still points out useful type errors though. You could use
-- this to interface with the vulkan library to catch even more.
shaderPipeline :: ShaderPipeline FilePath
shaderPipeline = ShaderPipeline $ StructInput @VertexData @Points
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)

shadersPath, vertPath, fragPath :: FilePath
shadersPath = "shaders"
vertPath = shadersPath </> "vert.spv"
fragPath = shadersPath </> "frag.spv"

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileTo vertPath spirv vertex
  , compileTo fragPath spirv fragment
  ]
  where spirv = [SPIRV $ Version 1 0]
