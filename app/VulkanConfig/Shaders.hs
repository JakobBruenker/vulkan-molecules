{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

import VulkanSetup.Types

type UpdatePosLocalSize = 256


type UpdatePosDefs =
  '[ "positions" ':-> StorageBuffer [DescriptorSet 1, Binding 0      ]
                      (Struct '["posArray" ':-> RuntimeArray (Struct '[ "pos" ':-> V 2 Float
                                                                      , "col" ':-> V 3 Float
                                                                      ])])
   , "ubo"       ':-> Uniform      '[DescriptorSet 0, Binding 0      ] UniformInput
   , "main"      ':-> EntryPoint   '[LocalSize UpdatePosLocalSize 1 1] Compute
   ]

updatePos :: Module UpdatePosDefs
updatePos = Module $ entryPoint @"main" @Compute do
  gid <- use @(Name "gl_GlobalInvocationID" :.: Swizzle "x")
  positions <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) gid
  assign @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) gid $
    over @(Name "col" :.: Swizzle "g") (+ 0.01) positions

type VertexInput =
  '[ Slot 0 0 ':-> V 2 Float
   , Slot 1 0 ':-> V 3 Float
   ]

type UniformInput = Struct
  '[ "time"         ':-> Float
   , "windowWidth"  ':-> Int32
   , "windowHeight" ':-> Int32
   ]

type VertexDefs =
  '[ "position"  ':-> Input      '[Location 0                ] (V 2 Float)
   , "color"     ':-> Input      '[Location 1                ] (V 3 Float)
   , "vertColor" ':-> Output     '[Location 0                ] (V 4 Float)
   , "ubo"       ':-> Uniform    '[DescriptorSet 0, Binding 0] UniformInput
   , "main"      ':-> EntryPoint '[                          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  time <- use @(Name "ubo" :.: Name "time")
  windowWidth <- use @(Name "ubo" :.: Name "windowWidth")
  windowHeight <- use @(Name "ubo" :.: Name "windowHeight")

  phi <- let' $ time / 100
  position <- #position
  scl <- let' if windowHeight < windowWidth
              then Mat22 (fromIntegral windowHeight / fromIntegral windowWidth) 0 0 1
              else Mat22 1 0 0 (fromIntegral windowWidth / fromIntegral windowHeight)
  rot <- let' $ Mat22 (cos phi) (sin phi) (-(sin phi)) (cos phi)
  pos' <- let' $ (scl !*! rot) !*^ position
  #gl_Position .= Vec4 (view @(Swizzle "x") pos') (view @(Swizzle "y") pos') 0 1
  color <- #color
  #vertColor .=
    Vec4 (view @(Swizzle "x") color) (view @(Swizzle "y") color) (view @(Swizzle "z") color) 0.3
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
  #color .= over @(Index 3) (* (1 - squaredNorm (pCoord ^* 2 ^-^ Vec2 1 1))) col

-- Currently unused. Still points out useful type errors though. You could use
-- this to interface with the vulkan library to catch even more.
shaderPipeline :: ShaderPipeline FilePath
shaderPipeline = ShaderPipeline $ StructInput @VertexInput @Points
  :>-> (vertex  , vertexShaderPath  )
  :>-> (fragment, fragmentShaderPath)

shadersPath, vertexShaderPath, fragmentShaderPath, updatePosPath :: FilePath
shadersPath = "shaders"
vertexShaderPath   = shadersPath </> "vert.spv"
fragmentShaderPath = shadersPath </> "frag.spv"
updatePosPath      = shadersPath </> "updPos.spv"

shaderPaths :: Dict HasShaderPaths
shaderPaths = Dict
  where ?vertexShaderPath = vertexShaderPath
        ?fragmentShaderPath = fragmentShaderPath
        ?updatePosPath = updatePosPath

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileTo vertexShaderPath spirv vertex
  , compileTo fragmentShaderPath spirv fragment
  , compileTo updatePosPath spirv updatePos
  ]
  where spirv = [SPIRV $ Version 1 0]
