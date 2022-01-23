{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

import Graphics.Types

type VertexData =
  '[ Slot 0 0 ':-> V 2 Float
   , Slot 1 0 ':-> V 3 Float
   ]

type UniformData = Struct
  '[ "time" ':-> Float
   ]

type VertexDefs =
  '[ "position"  ':-> Input      '[Location 0                ] (V 2 Float)
   , "color"     ':-> Input      '[Location 1                ] (V 3 Float)
   , "vertColor" ':-> Output     '[Location 0                ] (V 4 Float)
   , "ubo"       ':-> Uniform    '[Binding 0, DescriptorSet 0] UniformData
   , "main"      ':-> EntryPoint '[                          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  -- ubo <- #ubo
  -- time <- let' $ view @(Name "time") ubo
  position <- #position
  -- #gl_Position .= Vec4 (view @(Swizzle "x") position - time / 100) (view @(Swizzle "y") position) 0 1
  #gl_Position .= Vec4 (view @(Swizzle "x") position) (view @(Swizzle "y") position) 0 1
  color <- #color
  #vertColor .=
    Vec4 (view @(Swizzle "x") color) (view @(Swizzle "y") color) (view @(Swizzle "z") color) 1
  #gl_PointSize .= 40

type FragmentDefs =
  '[ "vertColor" ':-> Input      '[Location 0     ] (V 4 Float)
   , "color"     ':-> Output     '[Location 0     ] (V 4 Float)
   , "main"      ':-> EntryPoint '[OriginUpperLeft] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  pCoord <- #gl_PointCoord
  col <- #vertColor

  -- Limit alpha to a disk with darkened limb
  #color .= set @(Index 3) (1 - squaredNorm (pCoord ^* 2 ^-^ Vec2 1 1)) col

-- Currently unused. Still points out useful type errors though. You could use
-- this to interface with the vulkan library to catch even more.
shaderPipeline :: ShaderPipeline FilePath
shaderPipeline = ShaderPipeline $ StructInput @VertexData @Points
  :>-> (vertex  , vertexShaderPath  )
  :>-> (fragment, fragmentShaderPath)

shadersPath, vertexShaderPath, fragmentShaderPath :: FilePath
shadersPath = "shaders"
vertexShaderPath   = shadersPath </> "vert.spv"
fragmentShaderPath = shadersPath </> "frag.spv"

shaderPaths :: Dict HasShaderPaths
shaderPaths = Dict
  where ?vertexShaderPath = vertexShaderPath
        ?fragmentShaderPath = fragmentShaderPath

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileTo vertexShaderPath spirv vertex
  , compileTo fragmentShaderPath spirv fragment
  ]
  where spirv = [SPIRV $ Version 1 0]
