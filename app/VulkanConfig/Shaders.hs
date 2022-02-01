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
import VulkanConfig.Pipeline (numVertices)

type instance ComputeShaderCount = 2

type ComputeUniformInput = Struct
  '[ "dt" ':-> Float ]

type UpdatePosLocalSize = 64

type Ar = Name "array"
type Ix = AnIndex Word32

type StorageArray deco a = StorageBuffer deco (Struct '["array" ':-> RuntimeArray a])

type Ubo    = "ubo"    ':-> Uniform      '[DescriptorSet 0, Binding 0      ] ComputeUniformInput
type Posit  = "posit"  ':-> StorageArray '[DescriptorSet 1, Binding 0      ] (V 4 Float)
type Veloc  = "veloc"  ':-> StorageArray '[DescriptorSet 1, Binding 1      ] (V 4 Float)
type Accel  = "accel"  ':-> StorageArray '[DescriptorSet 1, Binding 2      ] (V 4 Float)
type Accel' = "accel'" ':-> StorageArray '[DescriptorSet 1, Binding 3      ] (V 4 Float)
type Main size = "main"   ':-> EntryPoint   '[size] Compute

updatePos :: Module '[Ubo, Posit, Veloc, Accel, Accel', Main (LocalSize UpdatePosLocalSize 1 1)]
updatePos = Module $ entryPoint @"main" @Compute do
  gid <- #gl_GlobalInvocationID <<&>> \i -> i.x
  when (gid < Lit numVertices) do
    ubo <- #ubo
    dt <- let' $ ubo.dt

    pos  <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc  <- use @(Name "accel"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc' <- use @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel  <- use @(Name "veloc"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel' <- let' $ vel ^+^ (dt * 0.5) *^ (acc ^+^ acc')
    -- friction
    let f x = 0.95 * abs x
    velx' <- let' $ if pos.x < -1 then f vel'.x else if pos.x > 1 then -(f vel'.x) else vel'.x
    vely' <- let' $ if pos.y < -1 then f vel'.y else if pos.y > 1 then -(f vel'.y) else vel'.y
    -- velx' <- let' $ if pos.x < -1 then abs vel'.x else if pos.x > 1 then -(abs vel'.x) else vel'.x
    -- vely' <- let' $ if pos.y < -1 then abs vel'.y else if pos.y > 1 then -(abs vel'.y) else vel'.y
    -- vel'' <- let' $ Vec2 if pos.x < -1 then abs vel'.x else if pos.x > 1 then -(abs vel'.x) else vel'.x
    --                      if pos.y < -1 then abs vel'.y else if pos.y > 1 then -(abs vel'.y) else vel'.y
    -- assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid vel'
    assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 velx' vely')

    modifying @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid
      (^+^ (dt *^ vel' ^+^ (dt * dt * 0.5) *^ acc'))

    assign @(Name "accel" :.: Ar :.: Ix :.: Swizzle "xy") gid acc'
    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 0 0)


-- type UpdateAccLocalSize = 8

-- -- TODO: this is not really synced correctly when there are more vertices than local group size I think
-- updateAcc :: Module '[Posit, Accel', Main (LocalSize UpdateAccLocalSize UpdateAccLocalSize 1)]
-- updateAcc = Module $ entryPoint @"main" @Compute do
--   gid <- use @(Name "gl_GlobalInvocationID" :.: Swizzle "xy")
--   when (gid.x /= gid.y && gid.x < Lit numVertices && gid.y < Lit numVertices) do
--     pos  <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid.x
--     other <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid.y
--     s <- let' $ 0.04
--     e <- let' $ 0.1
--     r <- let' $ distance pos other
--     s6 <- let' $ s ** 6
--     r6 <- let' $ r ** 6
--     modifying @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid.x $
--       (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ normalise (other ^-^ pos))

type UpdateAccLocalSize = 64

updateAcc :: Module '[Posit, Accel', Main (LocalSize UpdateAccLocalSize 1 1)]
updateAcc = Module $ entryPoint @"main" @Compute do
  gid <- use @(Name "gl_GlobalInvocationID" :.: Swizzle "x")
  when (gid < Lit numVertices) $ locally do
    pos  <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid

    #acc #= Vec2 0 0
    #i #= 0
    while (#i <<&>> (< Lit numVertices)) do
      i <- #i
      #i %= (+ 1)
      when (i /= gid) do
        other <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") i
        s <- let' $ 0.04
        e <- let' $ 0.1
        r <- let' $ distance pos other
        s6 <- let' $ s ** 6
        r6 <- let' $ r ** 6
        #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ normalise (other ^-^ pos))

    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid =<< #acc

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
   , "color"     ':-> Input      '[Location 1                ] (V 2 Float)
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
  #vertColor .= Vec4 (color.r * ubo.time * 0.01) color.g 0.5 0.3
  #gl_PointSize .= 25

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
