{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import GHC.Records

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

import VulkanSetup.Types
import VulkanConfig.FIRUtils ()
import VulkanConfig.Pipeline (numVertices, numVertexEntries)

type UpdatePosLocalSize = 256

type UpdatePosDefs =
  -- TODO
  -- this produces a validation error. fixed by adding %positions at the end of the OpEntryPoint line.
  -- might have to add NonWritable or NonReadable though, and split the buffer in two, idk
  -- EDIT: this is probably wrong, delete it if so, nothing to worry about I think
  '[ "ubo"       ':-> Uniform      '[DescriptorSet 0, Binding 0      ] UniformInput
   , "positions" ':-> StorageBuffer [DescriptorSet 1, Binding 0      ]
                      (Struct '["posArray" ':-> RuntimeArray Float])
                      -- really, this should be a RuntimeArray with
                      -- Struct ["pos" ':-> V 2 Float, "col" ':-> V 3 Float]
                      -- But the alignment doesn't work out since this is also
                      -- the vertex buffer
   , "main"      ':-> EntryPoint   '[LocalSize UpdatePosLocalSize 1 1] Compute
   ]

updatePos :: Module UpdatePosDefs
updatePos = Module $ entryPoint @"main" @Compute do
  gid <- #gl_GlobalInvocationID <<&>> \i -> i.x
  when (gid < Lit numVertices) do
    index <- let' $ gid * Lit numVertexEntries + 2
    modifying @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) index $ (+ 0.001)

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
  ubo <- #ubo
  floatWidth <- let' $ fromIntegral ubo.windowWidth
  floatHeight <- let' $ fromIntegral ubo.windowHeight

  phi <- let' $ ubo.time / 100
  position <- #position
  scl <- let' if floatHeight < floatWidth
              then Mat22 (floatHeight / floatWidth) 0 0 1
              else Mat22 1 0 0 (floatWidth / floatHeight)
  rot <- let' $ Mat22 (cos phi) (sin phi) (-(sin phi)) (cos phi)
  #gl_Position .= (scl !*! rot) !*^ position <!> Vec2 0 1
  color <- #color
  #vertColor .= Vec4 color.r color.g color.b 0.7
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
        ?computeShaderPath = updatePosPath

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileTo vertexShaderPath spirv vertex
  , compileTo fragmentShaderPath spirv fragment
  -- for some reason this shader isn't being compiled right now.
  -- replacing updatePos with fragment makes it compile, but it used to compile as is...
  , compileTo updatePosPath spirv updatePos
  ]
  where spirv = [SPIRV $ Version 1 3]
