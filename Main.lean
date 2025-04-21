import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def spheres : List Hittable := [
    Hittable.mk (Sphere.mk ⟨0, 0, -1⟩ 0.5),
    Hittable.mk (Sphere.mk ⟨0, -100.5, -1⟩ 100)
  ]

#ppm Camera.renderWorld spheres

def main : IO Unit := do
  let image ← Camera.renderWorld spheres (logging := true)
  IO.print image.display
