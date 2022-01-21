{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import Data.Vector.Sized qualified as Vector
import RIO.FilePath ((</>))

type VertexInput =
  '[
   ]

type VertexDefs =
  '[ "pointSize" ':-> Output     '[Location 0] Float
   , "main"      ':-> EntryPoint '[          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  positions :: Code (Array 3 (V 4 Float)) <- let' $ Lit . MkArray . Vector.fromTuple $
    ( V4 0.1 0.1 0 1
    , V4 0.1 0.8 0 1
    , V4 0.7 0.8 0 1
    )
  sizes :: Code (Array 3 Float) <- let' $ Lit . MkArray . Vector.fromTuple $ (10, 20, 30)
  vertexID <- #gl_VertexID
  #gl_Position .= view @(AnIndex _) vertexID positions
  #pointSize .= view @(AnIndex _) vertexID sizes
  #gl_PointSize .= view @(AnIndex _) vertexID sizes

type FragmentDefs =
  '[ "pointSize" ':-> Input      '[Location 0     ] Float
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

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline = ShaderPipeline $ StructInput @VertexInput @Points
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
