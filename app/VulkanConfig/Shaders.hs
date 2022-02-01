{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module VulkanConfig.Shaders where

import GHC.Records
import Data.Function
import Data.Vector.Sized qualified as Sized

import FIR
import FIR.Syntax.Labels
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

import VulkanSetup.Types
import VulkanConfig.FIRUtils ()
import VulkanConfig.Pipeline (numVertices, numVertexEntries)

type instance ComputeShaderCount = 2

type ComputeUniformInput = Struct
  '[ "sign" ':-> Word32 ]

type UpdatePosLocalSize = 256

type UpdatePosDefs =
  '[ "ubo"       ':-> Uniform      '[DescriptorSet 0, Binding 0      ] ComputeUniformInput
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
  when (gid < Lit numVertices) $ locally do
    ubo <- #ubo
    sign :: Code Float <- let' $ if ubo.sign == 0 then -1 else 1

    realGid <- let' $ Lit numVertexEntries * gid
    selfX <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) realGid
    selfY <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) (realGid + 1)


    -- subtract own position to find center of others
    #sum #= Vec2 -selfX -selfY
    #index #= 0
    while (#index <<&>> (< Lit numVertices)) do
      #index %= (+ 1)
      realIndex <- (Lit numVertexEntries *) <<$>> #index
      x <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) realIndex
      y <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) (realIndex + 1)
      #sum %= (^+^ Vec2 x y)

    center <- #sum <<&>> (^/ Lit (fromIntegral numVertices))
    modifying @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) realGid \x ->
      x - 0.00001 * (x - center.x) * sign
    modifying @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) (realGid + 1) \y ->
      y - 0.00001 * (y - center.y) * sign
    pure (Lit ())

type UpdateAccLocalSize = 256

type UpdateAccDefs =
  '[ "ubo"       ':-> Uniform      '[DescriptorSet 0, Binding 0      ] ComputeUniformInput
   , "positions" ':-> StorageBuffer [DescriptorSet 1, Binding 0      ]
                      (Struct '["posArray" ':-> RuntimeArray Float])
                      -- really, this should be a RuntimeArray with
                      -- Struct ["pos" ':-> V 2 Float, "col" ':-> V 3 Float]
                      -- But the alignment doesn't work out since this is also
                      -- the vertex buffer
   , "main"      ':-> EntryPoint   '[LocalSize UpdatePosLocalSize 1 1] Compute
   ]

updateAcc :: Module UpdatePosDefs
updateAcc = Module $ entryPoint @"main" @Compute do
  gid <- #gl_GlobalInvocationID <<&>> \i -> i.x
  when (gid < Lit numVertices) $ locally do
    realGid <- let' $ Lit numVertexEntries * gid

    sign <- let' $ if gid `mod` 2 == 0 then 1 else -1

    -- subtract own position to find center of others
    let usePos n = use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) (realGid + n)
    let assignPos n = assign @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) (realGid + n)
    pos <- Vec2 <<$>> usePos 0 <<*>> usePos 1

    phi <- let' $ sign * 0.00001
    rot <- let' $ Mat22 (cos phi) (sin phi) -(sin phi) (cos phi)
    pos' <- let' $ rot !*^ pos
    assignPos 0 pos'.x
    assignPos 1 pos'.y

type VertexInput =
  '[ Slot 0 0 ':-> V 2 Float
   , Slot 1 0 ':-> V 3 Float
   ]

type GraphicsUniformInput = Struct
  '[ "time"         ':-> Float
   , "windowWidth"  ':-> Int32
   , "windowHeight" ':-> Int32
   ]

type VertexDefs =
  '[ "position"  ':-> Input      '[Location 0                ] (V 2 Float)
   , "color"     ':-> Input      '[Location 1                ] (V 3 Float)
   , "vertColor" ':-> Output     '[Location 0                ] (V 4 Float)
   , "ubo"       ':-> Uniform    '[DescriptorSet 0, Binding 0] GraphicsUniformInput
   , "main"      ':-> EntryPoint '[                          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  ubo <- #ubo
  floatWidth  <- let' $ fromIntegral ubo.windowWidth
  floatHeight <- let' $ fromIntegral ubo.windowHeight

  position <- #position
  scl :: Code (M 2 2 Float) <- let' if floatHeight < floatWidth
                                    then Mat22 (floatHeight / floatWidth) 0 0 1
                                    else Mat22 1 0 0 (floatWidth / floatHeight)
  #gl_Position .= scl !*^ position <!> Vec2 0 1
  color <- #color
  #vertColor .= Vec4 (color.r * ubo.time * 0.01) color.g color.b 0.3
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

shadersPath, vertexShaderPath, fragmentShaderPath, updatePosPath, updateAccPath :: FilePath
shadersPath = "shaders"
vertexShaderPath   = shadersPath </> "vert.spv"
fragmentShaderPath = shadersPath </> "frag.spv"
updatePosPath      = shadersPath </> "updPos.spv"
updateAccPath      = shadersPath </> "updAcc.spv"

shaderPaths :: Dict HasShaderPaths
shaderPaths = Dict
  where ?vertexShaderPath = vertexShaderPath
        ?fragmentShaderPath = fragmentShaderPath
        ?computeShaderPaths = Sized.fromTuple (updatePosPath, updateAccPath)

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileTo vertexShaderPath spirv vertex
  , compileTo fragmentShaderPath spirv fragment
  , compileTo updatePosPath spirv updatePos
  , compileTo updateAccPath spirv updateAcc
  ]
  where spirv = [SPIRV $ Version 1 3]
