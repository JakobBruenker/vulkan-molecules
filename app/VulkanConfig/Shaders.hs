{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

-- Boltzmann constant in eV*K^-1
kB :: Float
kB = 8.617333262145e-5

jPereV :: Float
jPereV = 1.602176634 -- * 10^-19, omitted

avogadro :: Float
avogadro = 6.02214076 -- * 10^23, omitted

type instance ComputeShaderCount = 2

type ComputeUniformInput = Struct
  '[ "dt"          ':-> Float
   , "worldWidth"  ':-> Float
   , "worldHeight" ':-> Float
   ]

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

    pos  <- use @(Name "posit"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc  <- use @(Name "accel"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc' <- use @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel  <- use @(Name "veloc"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel' <- let' $ vel ^+^ (dt * 0.5) *^ (acc ^+^ acc')
    -- friction
    let f v isMin = (if (if isMin then (<) else (>)) v 0 then 0.5 else 1) * abs v
    r <- let' $ ubo.worldWidth
    l <- let' 0
    b <- let' $ ubo.worldHeight
    t <- let' 0
    velx' <- let' $ if pos.x < l
                    then f vel'.x True
                    else if pos.x > r then -(f vel'.x False) else vel'.x
    vely' <- let' $ if pos.y < t
                    then f vel'.y True
                    else if pos.y > b then -(f vel'.y False) else vel'.y
    assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 velx' vely')

    -- -- lazy friction
    -- vel'' <- let' $ vel' ^* 0.999999
    -- assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid vel''

    modifying @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid
      (^+^ (dt *^ vel' ^+^ (dt * dt * 0.5) *^ acc'))

    assign @(Name "accel" :.: Ar :.: Ix :.: Swizzle "xy") gid acc'
    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 0 0)

    -- speed visualization
    speed <- let' $ norm vel'
    assign @(Name "posit" :.: Ar :.: Ix :.: Swizzle "z") gid speed

type UpdateAccLocalSize = 64

updateAcc :: Module '[Ubo, Posit, Accel', Main (LocalSize UpdateAccLocalSize 1 1)]
updateAcc = Module $ entryPoint @"main" @Compute do
  -- ubo <- #ubo
  gid <- use @(Name "gl_GlobalInvocationID" :.: Swizzle "x")
  when (gid < Lit numVertices) $ locally do
    pos  <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid

    #acc #= Vec2 0 0
    #i #= 0
    s <- let' $ 3.4
    s6 <- let' $ s ** 6
    e <- let' $ 120 * Lit kB
    while (#i <<&>> (< Lit numVertices)) do
      i <- #i
      #i %= (+ 1)
      when (i /= gid) do
        other <- use @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") i
        r <- let' $ distance pos other
        r6 <- let' $ r ** 6
        #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ normalise (other ^-^ pos))

    -- -- boundaries
    -- do r <- let' $ pos.x + s / 2
    --    r6 <- let' $ r ** 6
    --    #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ Vec2 -1 0)
    --    -- without attractive part
    --    -- #acc %= (^+^ (e * 24 * s6 * (-2 * s6) / (r6 * r6 * r)) *^ Vec2 -1 0)

    -- do r <- let' $ ubo.worldWidth - pos.x + s / 2
    --    r6 <- let' $ r ** 6
    --    #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ Vec2 1 0)
    --    -- without attractive part
    --    -- #acc %= (^+^ (e * 24 * s6 * (-2 * s6) / (r6 * r6 * r)) *^ Vec2 1 0)

    -- do r <- let' $ pos.y + s / 2
    --    r6 <- let' $ r ** 6
    --    #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ Vec2 0 -1)
    --    -- without attractive part
    --    -- #acc %= (^+^ (e * 24 * s6 * (-2 * s6) / (r6 * r6 * r)) *^ Vec2 0 -1)

    -- do r <- let' $ ubo.worldHeight - pos.y + s / 2
    --    r6 <- let' $ r ** 6
    --    #acc %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ Vec2 0 1)
    --    -- without attractive part
    --    -- #acc %= (^+^ (e * 24 * s6 * (-2 * s6) / (r6 * r6 * r)) *^ Vec2 0 1)

    -- mass in Daltons
    m <- let' 40
    -- apply unit conversion factors
    convertedAcc <- #acc <<&>> (^/ (m * Lit jPereV * Lit avogadro * 1e-3))

    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid convertedAcc

type VertexInput =
  '[ Slot 0 0 ':-> V 2 Float
   , Slot 1 0 ':-> V 3 Float
   ]

type GraphicsUniformInput = Struct
  '[ "time"         ':-> Float
   , "windowWidth"  ':-> Int32
   , "windowHeight" ':-> Int32
   , "worldWidth"   ':-> Float
   , "worldHeight"  ':-> Float
   ]

type VertexDefs =
  '[ "position"  ':-> Input      '[Location 0                ] (V 3 Float)
   , "type"      ':-> Input      '[Location 1                ] Word32
   , "vertColor" ':-> Output     '[Location 0                ] (V 4 Float)
   , "ubo"       ':-> Uniform    '[DescriptorSet 0, Binding 0] GraphicsUniformInput
   , "main"      ':-> EntryPoint '[                          ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  ubo <- #ubo
  floatWidth  <- let' $ fromIntegral ubo.windowWidth
  floatHeight <- let' $ fromIntegral ubo.windowHeight
  angstromPerPixel <- let' if ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight
                            then ubo.worldWidth / floatWidth
                            else ubo.worldHeight / floatHeight

  position <- #position
  pos <- let' $ view @(Swizzle "xy") position ^-^ Vec2 (ubo.worldWidth / 2) (ubo.worldHeight / 2)
  scaleWorld :: Code (M 2 2 Float) <- let' $ Mat22 (2 / ubo.worldWidth) 0 0 (2 / ubo.worldHeight)
  scaleWin :: Code (M 2 2 Float) <- let'
    if ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight
    then Mat22 1 0 0 (1/((ubo.worldWidth / ubo.worldHeight) * (floatHeight / floatWidth)))
    else Mat22 (1/((ubo.worldHeight / ubo.worldWidth) * (floatWidth / floatHeight))) 0 0 1
  #gl_Position .= (scaleWin !*! scaleWorld) !*^ pos <!> Vec2 0 1
  speed <- let' $ log (position.z * 30)
  #vertColor .= Vec4 speed 0.5 0.5 0.9
  -- 3.76 is the diameter of Argon in Angstrom
  #gl_PointSize .= 3.76 / angstromPerPixel

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
