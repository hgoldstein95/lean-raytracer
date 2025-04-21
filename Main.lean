import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def rayColor (r : Ray) (world : List Hittable) : Id RGB := do
  -- Check for a collision with something in the world
  if let some collision := Hit.hit world r ⟨0, Float.infinity⟩ then
    return RGB.ofVec3 (0.5 * (collision.normal + 1))

  -- Otherwise just show the sky gradient
  let a : Float := 0.5 * (r.direction.normalize.y + 1)
  RGB.ofVec3 <| (1.0 - a) * (⟨1, 1, 1⟩ : Vec3) + a * (⟨0.5, 0.7, 1.0⟩ : Vec3)

def renderWorld (world : List Hittable) (logging : Bool := false) : IO PPM := do
  -- Configure the image
  let aspectRatio : Float := 16.0 / 9.0
  let imageWidth : UInt64 := 400
  let imageHeight : UInt64 := max (imageWidth.toFloat / aspectRatio).toUInt64 1

  -- Configure the camera
  let focalLength : Float := 1.0
  let viewportHeight : Float := 2
  let viewportWidth : Float :=
    viewportHeight * (imageWidth.toFloat / imageHeight.toFloat)
  let cameraCenter : Point3 := 0

  -- Set up conversions into viewport space
  let viewportU : Vec3 := ⟨viewportWidth, 0, 0⟩
  let viewportV : Vec3 := ⟨0, -viewportHeight, 0⟩
  let pixelDeltaU : Vec3 := viewportU / imageWidth.toFloat
  let pixelDeltaV : Vec3 := viewportV / imageHeight.toFloat

  -- Compute the location of ⟨0, 0⟩
  let viewportUpperLeft : Vec3 :=
    cameraCenter -
      ⟨0, 0, focalLength⟩ -
      (viewportU / 2.0) -
      (viewportV / 2.0)
  let pixel00Loc : Vec3 :=
    viewportUpperLeft + 0.5 * (pixelDeltaU + pixelDeltaV)

  -- Render pixels
  let mut image := PPM.empty imageWidth imageHeight
  for j in List.range image.height.toNat do
    for i in List.range image.width.toNat do
      -- Compute the ray through the given pixel
      let pixelCenter : Point3 :=
        pixel00Loc + (i.toFloat * pixelDeltaU) + (j.toFloat * pixelDeltaV)
      let rayDirection : Vec3 := pixelCenter - cameraCenter
      let ray := {origin := cameraCenter, direction := rayDirection}

      -- Cast the day, find the pixel color, and add it to the image
      image := image.addPixel (rayColor ray world)

    if logging then
      IO.eprint s!"Rendering: {progressBar j (image.height.toNat - 1)}\r"

  if logging then IO.eprintln s!"\nDone."
  return image

def spheres : List Hittable := [
    Hittable.mk (Sphere.mk ⟨0, 0, -1⟩ 0.5),
    Hittable.mk (Sphere.mk ⟨0, -100.5, -1⟩ 100)
  ]

#ppm renderWorld spheres

def main : IO Unit := do
  let image ← renderWorld spheres (logging := true)
  IO.print image.display
