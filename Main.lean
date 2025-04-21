import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def spheres : List Hittable := [
    Hittable.mk (Sphere.mk ⟨0, 0, -1⟩ 0.5),
    Hittable.mk (Sphere.mk ⟨0, -100.5, -1⟩ 100)
  ]

#ppm
  let config := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    samplesPerPixel := 1
  }
  (Camera.init config).renderWorld spheres

def main : IO Unit := do
  let config := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    samplesPerPixel := 10
  }
  let image ← (Camera.init config).renderWorld spheres (logging := true)
  IO.print image.display
