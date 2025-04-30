import RayTracer.Geometry.Ray
import RayTracer.Geometry.OnlineMean
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
  cores : Nat
  logging : Bool
  deriving BEq, Repr

instance : Inhabited CameraConfig where
  default := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 50,
    samplesPerPixel := 100,
    vfov := 90,
    lookFrom := 0,
    lookAt := ⟨0, 0, -1⟩,
    vUp := ⟨0, 1, 0⟩,
    defocusAngle := 0,
    focusDistance := 10,
    cores := 8,
    logging := false,
  }

def CameraConfig.std : CameraConfig := default

structure Camera extends CameraConfig where
  imageHeight : UInt64
  center : Point3
  pixel00Loc : Vec3
  pixelDeltaU : Vec3
  pixelDeltaV : Vec3
  defocusDiskU : Vec3
  defocusDiskV : Vec3

namespace Camera

def init (cfg : CameraConfig := default) : IO Camera := do
  if cfg.logging then
    IO.eprintln s!"Config: {repr cfg}"

  let imageHeight : UInt64 := max (cfg.imageWidth.toFloat / cfg.aspectRatio).toUInt64 1

  let theta := Float.degreesToRadians cfg.vfov
  let h := Float.tan (theta / 2.0)
  let viewportHeight : Float := 2 * h * cfg.focusDistance
  let viewportWidth : Float :=
    viewportHeight * (cfg.imageWidth.toFloat / imageHeight.toFloat)

  let w := Vec3.normalize (cfg.lookFrom - cfg.lookAt)
  let u := Vec3.normalize (cfg.vUp.cross w)
  let v := w.cross u

  let center : Point3 := cfg.lookFrom

  let viewportU : Vec3 := viewportWidth * u
  let viewportV : Vec3 := viewportHeight * -v
  let pixelDeltaU : Vec3 := viewportU / cfg.imageWidth.toFloat
  let pixelDeltaV : Vec3 := viewportV / imageHeight.toFloat

  let viewportUpperLeft : Vec3 :=
    center -
      (cfg.focusDistance * w) -
      (viewportU / 2.0) -
      (viewportV / 2.0)
  let pixel00Loc : Vec3 :=
    viewportUpperLeft + (0.5 : Float) * (pixelDeltaU + pixelDeltaV)

  let defocusRadius :=
    cfg.focusDistance * Float.tan (Float.degreesToRadians (cfg.defocusAngle / 2.0))
  let defocusDiskU := u * defocusRadius
  let defocusDiskV := v * defocusRadius

  return {
    toCameraConfig := cfg,
    imageHeight,
    center,
    pixel00Loc,
    pixelDeltaU,
    pixelDeltaV,
    defocusDiskU,
    defocusDiskV,
  }

private def sampleSquare : CameraM Vec3 := do
  pure ⟨(← .randFloat) - 0.5, (← .randFloat) - 0.5, 0⟩

private def sampleDefocusDisk (camera : Camera) : CameraM Vec3 := do
  let p ← Vec3.randomInUnitDisk
  return camera.center +
    (p.x * camera.defocusDiskU) +
    (p.y * camera.defocusDiskV)

private def rayThrough (camera : Camera) (i j : Float) : CameraM Ray := do
  let offset ← sampleSquare
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

private def trace
    (r : Ray)
    (world : Entity)
    (fuel : Nat) :
    CameraM Vec3 := do
  match fuel with
  | 0 => pure 0
  | fuel' + 1 =>
    if let some ⟨collision, material⟩ :=
        world.collide r ⟨0.001, Float.infinity⟩ then
      if let some scatter ← material.scatter r collision then do
        let color ← trace scatter.scattered world fuel'
        return scatter.attenuation * color
      else
        return 0

    let a : Float := 0.5 * (r.direction.normalize.y + 1)
    return (1.0 - a) * (⟨1, 1, 1⟩ : Vec3) + a * (⟨0.5, 0.7, 1.0⟩ : Vec3)

def averageAll (pxs : List (Array Vec3)) : Array Vec3 := Option.get! do
  let n ← (·.size) <$> pxs[0]?
  return Array.ofFn (n := n) (λ i => Option.get! do
    let row ← pxs.mapM (λ row => row[i]?)
    return row.sum / row.length.toUInt32.toFloat)

def render
    (camera : Camera)
    (world : Entity) :
    IO PPM := do
  let samplesPerTask := camera.samplesPerPixel / camera.cores

  let computePixels : UInt32 → Array Vec3 := λ seed => CameraM.run seed do
    let mut pixels := Array.emptyWithCapacity (camera.imageWidth * camera.imageHeight).toNat
    for j in Array.range camera.imageHeight.toNat do
      for i in Array.range camera.imageWidth.toNat do
        let mut estimator := OnlineMean.init

        for _ in List.range samplesPerTask do
          let ray ← camera.rayThrough i.toInt32.toFloat j.toInt32.toFloat
          let color ← trace ray world camera.maxRayDepth
          estimator := estimator.addSample color

        pixels := pixels.push estimator.mean

    return pixels

  let pixelsTask :=
    Task.mapList (λ xs => (averageAll xs).map RGB.ofVec3) <|
      (List.range camera.cores).map λ i =>
        Task.spawn (prio := .dedicated) (λ () => computePixels i.toUInt32)

  return {
      width := camera.imageWidth,
      height := camera.imageHeight,
      pixels := pixelsTask.get,
    }

end Camera
