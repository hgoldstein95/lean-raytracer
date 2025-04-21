import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def spheres : List Hittable := [
    Hittable.mk (Sphere.mk ⟨0, 0, -1⟩ 0.5 (lambertian 0.5)),
    Hittable.mk (Sphere.mk ⟨0, -100.5, -1⟩ 100 (lambertian 0.5))
  ]

def main : IO Unit := do
  let config := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 50,
    logging := true,
    samplesPerPixel := 100,
  }
  let camera ← Camera.init config
  IO.println <| PPM.display (← camera.render spheres)
