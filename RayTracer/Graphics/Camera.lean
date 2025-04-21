import RayTracer.Geometry.Ray
import RayTracer.Graphics.PPM
import RayTracer.Entities.Entity

open PPM RGB Vec3 Ray

structure CameraConfig where
  aspectRatio : Float
  imageWidth : UInt64
  samplesPerPixel : Nat
  maxRayDepth : Nat
  logging : Bool
  deriving BEq, Repr

instance : Inhabited CameraConfig where
  default := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 10,
    samplesPerPixel := 10,
    logging := false
  }

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

namespace Camera

def init (config : CameraConfig := default) : IO Camera := do
  if config.logging then
    IO.eprintln s!"Config: {repr config}"

  let aspectRatio : Float := config.aspectRatio
  let imageWidth : UInt64 := config.imageWidth
  let imageHeight : UInt64 := max (imageWidth.toFloat / aspectRatio).toUInt64 1

  let focalLength : Float := 1.0
  let viewportHeight : Float := 2
  let viewportWidth : Float :=
    viewportHeight * (imageWidth.toFloat / imageHeight.toFloat)
  let cameraCenter : Point3 := 0

  let viewportU : Vec3 := ⟨viewportWidth, 0, 0⟩
  let viewportV : Vec3 := ⟨0, -viewportHeight, 0⟩
  let pixelDeltaU : Vec3 := viewportU / imageWidth.toFloat
  let pixelDeltaV : Vec3 := viewportV / imageHeight.toFloat

  let viewportUpperLeft : Vec3 :=
    cameraCenter -
      ⟨0, 0, focalLength⟩ -
      (viewportU / 2.0) -
      (viewportV / 2.0)
  let pixel00Loc : Vec3 :=
    viewportUpperLeft + (0.5 : Float) * (pixelDeltaU + pixelDeltaV)

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
    maxRayDepth := config.maxRayDepth,
    logging := config.logging,
  }

private def sampleSquare : IO Vec3 := do
  pure ⟨(← IO.randFloat) - 0.5, (← IO.randFloat) - 0.5, 0⟩

private def getRay (camera : Camera) (i j : Float) : IO Ray := do
  let offset ←
    if camera.samplesPerPixel > 1 then sampleSquare else pure 0
  let pixelSample : Point3 :=
    camera.pixel00Loc +
    ((i + offset.x) * camera.pixelDeltaU) +
    ((j + offset.y) * camera.pixelDeltaV)
  let direction : Vec3 := pixelSample - camera.center
  pure {origin := camera.center, direction}

private partial def rayColor (r : Ray) (world : Entity) (fuel : Nat) : IO Vec3 := do
  if fuel == 0 then
    return 0

  if let some ⟨collision, material⟩ := world r ⟨0.001, Float.infinity⟩ then
    if let some scatter ← material r collision then do
      let color ← rayColor scatter.scattered world (fuel - 1)
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
