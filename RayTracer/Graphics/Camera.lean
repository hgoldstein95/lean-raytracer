import RayTracer.Graphics.PPM
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Hit

open PPM RGB Vec3 Ray

structure CameraConfig where
  aspectRatio : Float
  imageWidth : UInt64

instance : Inhabited CameraConfig where
  default := ⟨16.0 / 9.0, 400⟩

structure Camera where
  aspectRatio : Float
  imageWidth : UInt64
  imageHeight : UInt64
  center : Point3
  pixel00Loc : Vec3
  pixelDeltaU : Vec3
  pixelDeltaV : Vec3

namespace Camera

def init (config : CameraConfig := default) : Camera :=
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
    viewportUpperLeft + 0.5 * (pixelDeltaU + pixelDeltaV)

  {
    aspectRatio,
    imageWidth,
    imageHeight,
    center := cameraCenter,
    pixel00Loc,
    pixelDeltaU,
    pixelDeltaV
  }

private def getRay (camera : Camera) (i j : Nat) : Ray :=
  let pixelCenter : Point3 :=
    camera.pixel00Loc +
    (i.toFloat * camera.pixelDeltaU) +
    (j.toFloat * camera.pixelDeltaV)
  let rayDirection : Vec3 := pixelCenter - camera.center
  {origin := camera.center, direction := rayDirection}

private def rayColor [Hit World] (r : Ray) (world : World) : Id RGB := do
  if let some collision := Hit.hit world r ⟨0, Float.infinity⟩ then
    return RGB.ofVec3 (0.5 * (collision.normal + 1))

  let a : Float := 0.5 * (r.direction.normalize.y + 1)
  RGB.ofVec3 <| (1.0 - a) * (⟨1, 1, 1⟩ : Vec3) + a * (⟨0.5, 0.7, 1.0⟩ : Vec3)

def renderWorld
    [Hit World]
    (camera : Camera)
    (world : World)
    (logging : Bool := false) :
    IO PPM := do
  let mut image := PPM.empty camera.imageWidth camera.imageHeight
  for j in List.range image.height.toNat do
    for i in List.range image.width.toNat do
      let ray := camera.getRay i j
      let color := rayColor ray world
      image := image.addPixel color

    if logging then
      IO.eprint s!"Rendering: {progressBar j (image.height.toNat - 1)}\r"

  if logging then IO.eprintln s!"\nDone."
  return image

end Camera
