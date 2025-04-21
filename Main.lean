import RayTracer.Widget.PPM
import RayTracer.Basic

open PPM RGB Vec3 Ray

def world :=
  Entity.ofList [
    -- Ground
    Sphere.mk {
      center := ⟨0, -100.5, -1⟩,
      radius := 100,
      material := Lambertian.mk ⟨0.8, 0.8, 0.0⟩
    },
    -- Center
    Sphere.mk {
      center := ⟨0, 0, -1.2⟩,
      radius := 0.5,
      material := Lambertian.mk ⟨0.1, 0.2, 0.5⟩
    },
    -- Left
    Sphere.mk {
      center := ⟨-1.0, 0.0, -1.0⟩,
      radius := 0.5,
      material := Metal.mk ⟨0.8, 0.8, 0.8⟩
    },
    -- Right
    Sphere.mk {
      center := ⟨1.0, 0.0, -1.0⟩,
      radius := 0.5,
      material := Metal.mk ⟨0.8, 0.6, 0.2⟩
    }
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
  IO.println <| PPM.display (← camera.render world)
