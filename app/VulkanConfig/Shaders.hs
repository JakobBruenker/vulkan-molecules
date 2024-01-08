{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- Optimization takes a long time but doesn't seem to help much. Could consider making this configurable.
{-# OPTIONS_GHC -O0 #-}
{- HLINT ignore "Use camelCase" "Use guards" -}

module VulkanConfig.Shaders where

import GHC.Records
import Data.Function
import Data.Vector.Sized qualified as Sized

import FIR
import FIR.Syntax.Labels ( (#=), (%=), (.=), IsLabel(fromLabel) )
-- Can use this for debugging
-- import FIR.Syntax.DebugPrintf
import Math.Linear

import Data.Foldable (sequence_)
import RIO.FilePath ((</>))

import VulkanSetup.Types
import VulkanConfig.FIRUtils ()
import VulkanConfig.Pipeline (numVertices)
import VulkanConfig.Shaders.ADiff hiding (Vec2)
import VulkanConfig.Shaders.Equations qualified as E

-- Boltzmann constant in eV*K^-1
kB :: Float
kB = 8.617333262145e-5

jPereV :: Float
jPereV = 1.602176634 -- * 10^-19, omitted

jPerkcal :: Float
jPerkcal = 4.184

avogadro :: Float
avogadro = 6.02214076 -- * 10^23, omitted

-- Coulomb's constant in eV angstrom / e^2
coulomb :: Float
coulomb = 14.3996

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
-- The fourth value of "posit" is actually the type encoded as an unsigned integer
type Posit  = "posit"  ':-> StorageArray '[DescriptorSet 1, Binding 0      ] (V 4 Float)
type Veloc  = "veloc"  ':-> StorageArray '[DescriptorSet 1, Binding 1      ] (V 4 Float)
type Accel  = "accel"  ':-> StorageArray '[DescriptorSet 1, Binding 2      ] (V 4 Float)
type Accel' = "accel'" ':-> StorageArray '[DescriptorSet 1, Binding 3      ] (V 4 Float)
type Main size = "main"   ':-> EntryPoint   '[size] Compute

updatePos :: Module '[Ubo, Posit, Veloc, Accel, Accel', Main (LocalSize UpdatePosLocalSize 1 1)]
updatePos = Module $ entryPoint @"main" @Compute do
  gid <- #gl_GlobalInvocationID <<&>> (.x)
  when (gid < Lit numVertices) do
    ubo <- #ubo
    dt <- let' ubo.dt

    pos  <- use @(Name "posit"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc  <- use @(Name "accel"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    acc' <- use @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel  <- use @(Name "veloc"  :.: Ar :.: Ix :.: Swizzle "xy") gid
    vel' <- let' $ vel ^+^ (dt * 0.5) *^ (acc ^+^ acc')
    -- friction
    let f v isMin = (if (if isMin then (<) else (>)) v 0 then 0.9 else 1) * abs v
    r <- let' ubo.worldWidth
    l <- let' 0
    b <- let' ubo.worldHeight
    t <- let' 0
    velx' <- let' $ if pos.x < l
                    then f vel'.x True
                    else if pos.x > r then -(f vel'.x False) else vel'.x
    vely' <- let' $ if pos.y < t
                    then f vel'.y True
                    else if pos.y > b then -(f vel'.y False) else vel'.y
    assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 velx' vely')

    -- -- lazy friction
    -- vel'' <- let' $ Vec2 velx' vely' ^* 0.99998
    -- assign @(Name "veloc" :.: Ar :.: Ix :.: Swizzle "xy") gid vel''

    modifying @(Name "posit" :.: Ar :.: Ix :.: Swizzle "xy") gid
      (^+^ (dt *^ vel' ^+^ (dt * dt * 0.5) *^ acc'))

    assign @(Name "accel" :.: Ar :.: Ix :.: Swizzle "xy") gid acc'
    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid (Vec2 0 0)

type UpdateAccLocalSize = 64

type AtomType = Word32

-- an atom needs to have enough electrons to be able to form π and double-π
-- bonds
minπType, minππType :: AtomType
minπType = 3
minππType = 4

λ28 :: Code Float
λ28 = 1.69

p_bo1_lut, p_bo2_lut, p_bo3_lut, p_bo4_lut, p_bo5_lut, p_bo6_lut
  :: Code AtomType -> Code AtomType -> Code Float
p_bo1_lut i j = if i == 1 && j == 1 then -0.016 else
                if i == 1 && j == 6 then -0.013 else
                if i == 6 && j == 1 then -0.013 else
                if i == 6 && j == 6 then -0.097 else 0
p_bo2_lut i j = if i == 1 && j == 1 then 5.98 else
                if i == 1 && j == 6 then 7.65 else
                if i == 6 && j == 1 then 7.65 else
                if i == 6 && j == 6 then 6.38 else 0
p_bo3_lut i j = if i == 6 && j == 6 then -0.26 else 0
p_bo4_lut i j = if i == 6 && j == 6 then 9.37 else 0
p_bo5_lut i j = if i == 6 && j == 6 then -0.391 else 0
p_bo6_lut i j = if i == 6 && j == 6 then 16.87 else 0

-- TODO: possibly have different values for pi bonds
de_lut :: Code AtomType -> Code AtomType -> Code Float
de_lut i j = if i == 1 && j == 1 then 168.4 else
             if i == 1 && j == 6 then 183.8 else
             if i == 6 && j == 1 then 183.8 else
             if i == 6 && j == 6 then 145.2 else 0

-- TODO: We should package these up into a Vec4 for efficiency
ε_lut, r_vdw_lut, ɑ_lut, ɣw_lut :: Code AtomType -> Code Float
ε_lut i = if i == 1 then 0.0194 else if i == 6 then 0.0862 else 0
r_vdw_lut i = if i == 1 then 3.649 else if i == 6 then 3.912 else 1
ɑ_lut i = if i == 1 then 10.06 else if i == 6 then 10.71 else 10
ɣw_lut i = if i == 1 then 5.36 else if i == 6 then 1.41 else 1

mass, charge, lj_σ, lj_ε, rσ_lut, rπ_lut, rππ_lut :: Code AtomType -> Code Float
mass atomType = if atomType == 1 then 1.007825 else if atomType == 6 then 12 else 0
charge atomType = if atomType == 1 then 0 else if atomType == 6 then 0 else 0
lj_σ atomType = if atomType == 1 then 0.5523570 else if atomType == 6 then 1.3541700 else 1
lj_ε atomType = if atomType == 1 then 4.4778900 else if atomType == 6 then 6.3695300 else 1
rσ_lut atomType = if atomType == 1 then 0.656 else if atomType == 6 then 1.399 else 1
rπ_lut atomType = if atomType == 6 then 1.266 else 1
rππ_lut atomType = if atomType == 6 then 1.236 else 1

valence :: Code AtomType -> Code Int32
valence atomType = if atomType == 1 then 1 else if atomType == 6 then 4 else 0

lj_shield_power, lj_shield_start :: Code Float
lj_shield_power = 3
lj_shield_start = 1.2

atomColor :: Code AtomType -> Code (V 3 Float)
atomColor atomType =
  if atomType == 1 then Vec3 0.9 0.9 0.9 else if atomType == 6 then Vec3 0.3 0.3 0.3 else Vec3 1 0 1

updateAcc :: Module '[Ubo, Posit, Accel', Main (LocalSize UpdateAccLocalSize 1 1)]
updateAcc = Module $ entryPoint @"main" @Compute do
  -- ubo <- #ubo
  gid <- use @(Name "gl_GlobalInvocationID" :.: Swizzle "x")
  when (gid < Lit numVertices) $ locally do
    iprops <- use @(Name "posit" :.: Ar :.: Ix) gid
    ipos <- let' $ view @(Swizzle "xy") iprops
    itype <- let' $ bitcast iprops.w

    -- mass in Daltons
    m <- let' $ mass itype

    #force #= Vec2 0 0

    -- -- lennard jones
    -- #i #= 0

    -- while (#i <<&>> (< Lit numVertices)) do
    --   i <- #i
    --   #i %= (+ 1)
    --   when (i /= gid) do
    --     other <- use @(Name "posit" :.: Ar :.: Ix) i
    --     otherPos <- let' $ view @(Swizzle "xy") other
    --     otherType <- let' $ bitcast other.w
    --     s <- let' $ 1.8 * (radius atomType + radius otherType) / 2
    --     e <- let' $ Lit kB * sqrt (lj_potential atomType * lj_potential otherType)
    --     s6 <- let' $ s ** 6
    --     r <- let' $ distance pos otherPos
    --     r6 <- let' $ r ** 6
    --     #force %= (^+^ (e * 24 * s6 * (r6 - 2 * s6) / (r6 * r6 * r)) *^ normalise (otherPos ^-^ pos))
    --     -- only repulsive
    --     -- #force %= (^+^ (e * 24 * s6 * (-2 * s6) / (r6 * r6 * r)) *^ normalise (otherPos ^-^ pos))

    -- -- apply unit conversion factors
    -- forceLJ <- #force <<&>> (^/ (Lit jPereV * Lit avogadro * 1e-3))

    -- -- electric
    -- #i .= 0
    -- q <- let' $ charge atomType
    -- while (#i <<&>> (< Lit numVertices)) do
    --   i <- #i
    --   #i %= (+ 1)
    --   when (i /= gid) do
    --     other <- use @(Name "posit" :.: Ar :.: Ix) i
    --     otherPos <- let' $ view @(Swizzle "xy") other
    --     otherType <- let' $ bitcast other.w
    --     otherQ <- let' $ charge otherType
    --     r <- let' $ distance pos otherPos
    --     r2 <- let' $ r * r

    --     #force %= (^+^ -(Lit coulomb * q * otherQ / r2) *^ normalise (otherPos ^-^ pos))

    -- -- apply unit conversion factors
    -- -- we start with kg * m / s^2
    -- -- we want to end up with dalton * angstrom / fs^2
    -- forceE <- #force <<&>> (^/ (Lit jPereV * Lit avogadro * 1e-3))

    -- assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid ((forceLJ ^+^ forceE) ^/ m)

    let fbo' r pa pb ro = $$(simpleCode E.fbo')

    #j #= 0

    #_Δ' #= -(fromIntegral $ valence itype)
    #dΔ' #= Vec2 0 0

    while (#j <<&>> (< Lit numVertices)) do
      j <- #j
      #j %= (+ 1)
      when (j /= gid) do
        jprops <- use @(Name "posit" :.: Ar :.: Ix) j
        jpos <- let' $ view @(Swizzle "xy") jprops
        jtype <- let' $ bitcast jprops.w

        -- set minimum r of for numerical stability in edge cases that
        -- shouldn't occur
        -- XXX JB it actually does occur! very often, i.e. right in the first collision, where the dist goes down to less than 7.5e-3 i.e. 0.0075
        -- Weirdly making this value smaller doesn't seem to change the behavior very much
        r <- let' $ max 0.2 (distance ipos jpos)

        p_bo1 <- let' $ p_bo1_lut itype jtype
        p_bo2 <- let' $ p_bo2_lut itype jtype
        p_bo3 <- let' $ p_bo3_lut itype jtype
        p_bo4 <- let' $ p_bo4_lut itype jtype
        p_bo5 <- let' $ p_bo5_lut itype jtype
        p_bo6 <- let' $ p_bo6_lut itype jtype

        rσ <- let' $ (rσ_lut itype + rσ_lut jtype) / 2
        rπ <- let' $ (rπ_lut itype + rπ_lut jtype) / 2
        rππ <- let' $ (rππ_lut itype + rππ_lut jtype) / 2

        let bo'σ = fbo' r p_bo1 p_bo2 rσ
            bo'π = if itype >= Lit minπType && jtype >= Lit minπType then fbo' r p_bo3 p_bo4 rπ else 0
            bo'ππ = if itype >= Lit minππType && jtype >= Lit minππType then fbo' r p_bo5 p_bo6 rππ else 0
        bo' <- let' $ bo'σ + bo'π + bo'ππ

        dr <- let' $ Vec2 (ipos.x - jpos.x) (ipos.y - jpos.y) ^/ r

        -- Have to insert these parentheses to prevent infinity/NaN. Maybe there's a more robust solution?
        let fdbo' pa pb ro = pa * pb * ((r / ro)**pb * fbo' r pa pb ro)
            dbo'σ = fdbo' p_bo1 p_bo2 rσ
            dbo'π = if itype >= Lit minπType && jtype >= Lit minπType then fdbo' p_bo3 p_bo4 rπ else 0
            dbo'ππ = if itype >= Lit minππType && jtype >= Lit minππType then fdbo' p_bo5 p_bo6 rππ else 0
        dbo' <- let' $ ((dbo'σ + dbo'π + dbo'ππ) / r) *^ dr

        #_Δ' %= (+ bo')
        #dΔ' %= (^+^ dbo')

    #j .= 0

    while (#j <<&>> (< Lit numVertices)) do
      j <- #j
      #j %= (+ 1)
      when (j /= gid) do
        jprops <- use @(Name "posit" :.: Ar :.: Ix) j
        jpos <- let' $ view @(Swizzle "xy") jprops
        jtype <- let' $ bitcast jprops.w

        -- set minimum r of for numerical stability in edge cases that
        -- shouldn't occur, but see above
        r <- let' $ max 0.2 (distance ipos jpos)

        p_bo1 <- let' $ p_bo1_lut itype jtype
        p_bo2 <- let' $ p_bo2_lut itype jtype
        p_bo3 <- let' $ p_bo3_lut itype jtype
        p_bo4 <- let' $ p_bo4_lut itype jtype
        p_bo5 <- let' $ p_bo5_lut itype jtype
        p_bo6 <- let' $ p_bo6_lut itype jtype

        rσ <- let' $ (rσ_lut itype + rσ_lut jtype) / 2
        rπ <- let' $ (rπ_lut itype + rπ_lut jtype) / 2
        rππ <- let' $ (rππ_lut itype + rππ_lut jtype) / 2

        let bo'σ = fbo' r p_bo1 p_bo2 rσ
            bo'π = if itype >= Lit minπType && jtype >= Lit minπType then fbo' r p_bo3 p_bo4 rπ else 0
            bo'ππ = if itype >= Lit minππType && jtype >= Lit minππType then fbo' r p_bo5 p_bo6 rππ else 0
        bo' <- let' $ bo'σ + bo'π + bo'ππ

        -- Have to insert these parentheses to prevent infinity/NaN. Maybe there's a more robust solution?
        let fdbo' pa pb ro = pa * pb * ((r / ro)**pb * fbo' r pa pb ro)
            dbo'σ = fdbo' p_bo1 p_bo2 rσ
            dbo'π = if itype >= Lit minπType && jtype >= Lit minπType then fdbo' p_bo3 p_bo4 rπ else 0
            dbo'ππ = if itype >= Lit minππType && jtype >= Lit minππType then fdbo' p_bo5 p_bo6 rππ else 0
        dbo' <- let' $ (dbo'σ + dbo'π + dbo'ππ) / r
        -- dbo'v <- let' $ dbo' *^ dr

        -- converting kcal/mol into eV
        de <- let' $ de_lut itype jtype * 0.0434
        -- TODO figure out proper conversion factor
        -- conv <- let' 0.1

        -- #force %= (^+^ (conv * -de) *^ dbo')

        -- -- This doesn't quite do what I want, using LJ for now
        -- r_vdw <- let' $ (r_vdw_lut itype + r_vdw_lut jtype) / 2
        -- ε <- let' $ (ε_lut itype + ε_lut jtype) / 2
        -- ɑ <- let' $ (ɑ_lut itype + ɑ_lut jtype) / 2
        -- ɣw <- let' $ (ɣw_lut itype + ɣw_lut jtype) / 2

        -- f13' <- let' $ r**λ28 + ɣw**(-λ28)
        -- f13 <- let' $ f13'**(1/λ28)
        -- df13 <- let' $ (f13'**(1/λ28 - 1) * r**(λ28 - 1)) *^ dr
        -- df14' <- let' $ exp (ɑ * (1 - f13 / r_vdw))
        -- df14a <- let' $ (-ɑ * df14' / r_vdw) *^ df13
        -- df14b <- let' $ (-ɑ * 0.5 * df14'**0.5 / r_vdw) *^ df13
        -- dEvdW <- let' $ ε *^ df14a ^-^ 2 *^ df14b

        -- -- apply unit conversion factors
        -- -- we start with (kcal / mole) / Angstrom
        -- -- we want to end up with dalton * angstrom / fs^2
        -- -- TODO: Not sure this is the right conversion
        -- #force %= (^-^ (Lit jPerkcal *^ dEvdW) ^/ (Lit jPereV * Lit avogadro * 1e-3))

        -- potential valley gets closer to nucleus as bond order increases
        -- unchanged at BO = 0, reduced by ~50% at BO = 1, and reduced by ~60% at BO = 3
        σ0 <- let' $ (lj_σ itype + lj_σ jtype) / 2
        σ <- let' $ σ0 * (0.6 * 0.17**bo' + 0.4)
        ε0 <- let' $ sqrt (lj_ε itype * lj_ε jtype)
        ε <- let' $ ε0 + de * bo'

        dσ <- let' $ σ0 * 0.6 * -0.177196 * 0.17**bo' * dbo'
        dε <- let' $ de * dbo'

        -- σ6 <- let' $ σ ** 6
        -- r6 <- let' $ r ** 6
        -- force <- let' $ (ε * 24 * σ6 * (r6 - 2 * σ6) / (r6 * r6 * r)) *^ normalise (jpos ^-^ ipos)
        -- XXX not sure the sign is right, but it seems to work
        force <- let' $ normalise (jpos ^-^ ipos) ^*
          -((4 * σ**5 / r**13) *
            (r * σ * (r**6 - σ**6) * dε + 6 * ε * (r**6 - 2 * σ**6) * (r * dσ - σ)))

        -- Unit conversion:
        -- We have eV/Angstrom, we want Dalton * Angstrom * fs^-2
        #force %= (^+^ 0.00964853322 *^ force)

    force <- #force
    assign @(Name "accel'" :.: Ar :.: Ix :.: Swizzle "xy") gid (force ^/ m)

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
   , "vertColor" ':-> Output     '[Location 0                ] (V 3 Float)
   , "pointSize" ':-> Output     '[Location 1                ] Float
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
  atomType <- #type
  pos <- let' $ view @(Swizzle "xy") position ^-^ Vec2 (ubo.worldWidth / 2) (ubo.worldHeight / 2)
  scaleWorld :: Code (M 2 2 Float) <- let' $ Mat22 (2 / ubo.worldWidth) 0 0 (2 / ubo.worldHeight)
  scaleWin :: Code (M 2 2 Float) <- let'
    if ubo.worldWidth / ubo.worldHeight > floatWidth / floatHeight
    then Mat22 1 0 0 (1/((ubo.worldWidth / ubo.worldHeight) * (floatHeight / floatWidth)))
    else Mat22 (1/((ubo.worldHeight / ubo.worldWidth) * (floatWidth / floatHeight))) 0 0 1
  #gl_Position .= (scaleWin !*! scaleWorld) !*^ pos <!> Vec2 0 1
  color <- let' $ atomColor atomType
  #vertColor .= color
  pointSize <- let' $ 2**(1/6) * lj_σ atomType / angstromPerPixel
  #pointSize .= pointSize
  #gl_PointSize .= pointSize

type FragmentDefs =
  '[ "vertColor" ':-> Input      '[Location 0                     ] (V 3 Float)
   , "pointSize" ':-> Input      '[Location 1                     ] Float
   , "color"     ':-> Output     '[Location 0                     ] (V 4 Float)
   , "main"      ':-> EntryPoint '[OriginUpperLeft, DepthReplacing] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  pointSize <- #pointSize
  samplePos <- #gl_SamplePosition
  pCoord <- #gl_PointCoord <<&>> (^+^ ((samplePos ^-^ Vec2 0.5 0.5) ^/ pointSize))
  depth <- let' $ squaredNorm (pCoord ^* 2 ^-^ Vec2 1 1)
  -- Limit color to a disk with darkened limb
  col <- #vertColor <<&>> (^* (1 - depth))

  #color .= Vec4 col.r col.g col.b 1
  #gl_FragDepth .= depth

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

compileAllShaders :: HasDebug => IO ()
compileAllShaders = sequence_
  [ compileTo vertexShaderPath spirv vertex
  , compileTo fragmentShaderPath spirv fragment
  , compileTo updatePosPath spirv updatePos
  , compileTo updateAccPath spirv updateAcc
  ]
  where spirv = (if ?enableDebug then [Debug] else []) <> [SPIRV $ Version 1 3]
