import RayTracer.Geometry.Ray
import RayTracer.Graphics.PPM
import RayTracer.Graphics.Entity

structure CameraConfig where
  aspectRatio : Float
  imageWidth : UInt64
  samplesPerPixel : Nat
  maxRayDepth : Nat
  vfov : Float
  lookFrom : Point3
  lookAt : Point3
  vUp : Vec3
  defocusAngle : Float
  focusDistance : Float
  logging : Bool
  deriving BEq, Repr

instance : Inhabited CameraConfig where
  default := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 10,
    samplesPerPixel := 10,
    vfov := 90,
    lookFrom := 0,
    lookAt := ⟨0, 0, -1⟩,
    vUp := ⟨0, 1, 0⟩,
    defocusAngle := 0,
    focusDistance := 10,
    logging := false,
  }

def CameraConfig.std : CameraConfig := default

structure Camera where
  aspectRatio : Float
  imageWidth : UInt64
  imageHeight : UInt64
  samplesPerPixel : Nat
  pixelScaleFactor : Float
  center : Point3
  pixel00Loc : Vec3
  pixelDeltaU : Vec3
  pixelDeltaV : Vec3
  maxRayDepth : Nat
  logging : Bool
  vfov : Float
  lookFrom : Point3
  lookAt : Point3
  defocusAngle : Float
  focusDistance : Float
  defocusDiskU : Vec3
  defocusDiskV : Vec3
  vUp : Vec3

namespace Camera

def init (config : CameraConfig := default) : IO Camera := do
  if config.logging then
    IO.eprintln s!"Config: {repr config}"

  let aspectRatio : Float := config.aspectRatio
  let imageWidth : UInt64 := config.imageWidth
  let imageHeight : UInt64 := max (imageWidth.toFloat / aspectRatio).toUInt64 1

  let vfov := config.vfov
  let lookFrom := config.lookFrom
  let lookAt := config.lookAt
  let vUp := config.vUp
  let defocusAngle := config.defocusAngle
  let focusDistance := config.focusDistance

  let theta := Float.degreesToRadians vfov
  let h := Float.tan (theta / 2.0)
  let viewportHeight : Float := 2 * h * focusDistance
  let viewportWidth : Float :=
    viewportHeight * (imageWidth.toFloat / imageHeight.toFloat)

  let w := Vec3.normalize (lookFrom - lookAt)
  let u := Vec3.normalize (vUp.cross w)
  let v := w.cross u

  let cameraCenter : Point3 := lookFrom

  let viewportU : Vec3 := viewportWidth * u
  let viewportV : Vec3 := viewportHeight * -v
  let pixelDeltaU : Vec3 := viewportU / imageWidth.toFloat
  let pixelDeltaV : Vec3 := viewportV / imageHeight.toFloat

  let viewportUpperLeft : Vec3 :=
    cameraCenter -
      (focusDistance * w) -
      (viewportU / 2.0) -
      (viewportV / 2.0)
  let pixel00Loc : Vec3 :=
    viewportUpperLeft + (0.5 : Float) * (pixelDeltaU + pixelDeltaV)

  let defocusRadius :=
    focusDistance * Float.tan (Float.degreesToRadians (defocusAngle / 2.0))
  let defocusDiskU := u * defocusRadius
  let defocusDiskV := v * defocusRadius

  return {
    aspectRatio,
    imageWidth,
    imageHeight,
    samplesPerPixel := config.samplesPerPixel,
    pixelScaleFactor := 1 / config.samplesPerPixel.toFloat,
    center := cameraCenter,
    pixel00Loc,
    pixelDeltaU,
    pixelDeltaV,
    vfov,
    lookFrom,
    lookAt,
    vUp,
    defocusAngle,
    focusDistance,
    defocusDiskU,
    defocusDiskV,
    maxRayDepth := config.maxRayDepth,
    logging := config.logging,
  }

private def sampleSquare : IO Vec3 := do
  pure ⟨(← IO.randFloat) - 0.5, (← IO.randFloat) - 0.5, 0⟩

private def sampleDefocusDisk (camera : Camera) : IO Vec3 := do
  let p ← Vec3.randomInUnitDisk
  return camera.center +
    (p.x * camera.defocusDiskU) +
    (p.y * camera.defocusDiskV)

private def getRay (camera : Camera) (i j : Float) : IO Ray := do
  let offset ←
    if camera.samplesPerPixel > 1 then sampleSquare else pure 0
  let pixelSample : Point3 :=
    camera.pixel00Loc +
    ((i + offset.x) * camera.pixelDeltaU) +
    ((j + offset.y) * camera.pixelDeltaV)
  let origin ← do
    if camera.defocusAngle <= 0 then
      pure camera.center
    else do
      sampleDefocusDisk camera
  let direction : Vec3 := pixelSample - origin
  pure {origin, direction}

private def rayColor
    (r : Ray)
    (world : Entity)
    (fuel : Nat) :
    IO Vec3 := do
  match fuel with
  | 0 => pure 0
  | fuel' + 1 =>
    if let some ⟨collision, material⟩ :=
        world.collide r ⟨0.001, Float.infinity⟩ then
      if let some scatter ← material.scatter r collision then do
        let color ← rayColor scatter.scattered world fuel'
        return scatter.attenuation * color
      else
        return 0

    let a : Float := 0.5 * (r.direction.normalize.y + 1)
    return (1.0 - a) * (⟨1, 1, 1⟩ : Vec3) + a * (⟨0.5, 0.7, 1.0⟩ : Vec3)

def render
    (camera : Camera)
    (world : Entity) :
    IO PPM := do
  let mut image := PPM.init camera.imageWidth camera.imageHeight
  for j in List.range camera.imageHeight.toNat do
    for i in List.range camera.imageWidth.toNat do
      let mut color : Vec3 := ⟨0, 0, 0⟩
      for _ in List.range camera.samplesPerPixel do
        let ray ← camera.getRay i.toInt32.toFloat j.toInt32.toFloat
        color := color + (← rayColor ray world camera.maxRayDepth)
      image := image.addPixel <| RGB.ofVec3 (color * camera.pixelScaleFactor)

    if camera.logging then
      let bar := progressBar j (camera.imageHeight.toNat - 1) (ticks := 30)
      IO.eprint s!"Rendering: {bar}\r"

  if camera.logging then IO.eprintln s!"\nDone."
  return image

end Camera
