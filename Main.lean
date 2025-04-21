import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def spheres : Entity := Entity.ofList [
    Sphere.mk {
      center := ⟨0, 0, -1⟩,
      radius := 0.5,
      material := Lambertian.mk 0.5
    },
    Sphere.mk {
      center := ⟨0, -100.5, -1⟩,
      radius := 100,
      material := Lambertian.mk 0.5,
    },
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
