import RayTracer
import RayTracer.Widget.PPM

open PPM RGB Vec3 Ray

structure Sphere where
  center : Point3
  radius : Float
  deriving BEq, Repr

def Ray.hitSphere (r : Ray) (s : Sphere) : Option Float :=
  let oc : Vec3 := s.center - r.origin
  let a := r.direction.lengthSquared
  let h := r.direction ⬝ oc
  let c := oc.lengthSquared - s.radius * s.radius
  let discriminant := h * h - a * c
  if discriminant >= 0 then (h - discriminant.sqrt) / a else none

def rayColor (r : Ray) : Id RGB := do
  if let some t := r.hitSphere (Sphere.mk ⟨0, 0, -1⟩ 0.5) then
    let norm := Vec3.normalize (r.at t - ⟨0, 0, -1⟩)
    return RGB.ofVec3 (0.5 * (norm + 1))
  let unitDirection := r.direction.normalize
  let a : Float := 0.5 * (unitDirection.y + 1)
  RGB.ofVec3 <| (1.0 - a) * (⟨1, 1, 1⟩ : Vec3) + a * (⟨0.5, 0.7, 1.0⟩ : Vec3)

def renderScene (logging : Bool := false) : IO PPM := do
  let aspectRatio : Float := 16.0 / 9.0

  let imageWidth : UInt64 := 400
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

  let mut image := PPM.empty imageWidth imageHeight
  for j in List.range image.height.toNat do
    if logging then
      IO.eprint s!"Rendering: {progressBar j (image.height.toNat - 1)}\r"
    for i in List.range image.width.toNat do
      let pixelCenter : Point3 :=
        pixel00Loc + (i.toFloat * pixelDeltaU) + (j.toFloat * pixelDeltaV)
      let rayDirection : Vec3 := pixelCenter - cameraCenter
      let r := {origin := cameraCenter, direction := rayDirection}
      image := image.addPixel (rayColor r)
  if logging then IO.eprintln s!"\nDone."
  return image

#ppm renderScene

def main : IO Unit := do
  let image ← renderScene (logging := true)
  IO.print image.display
