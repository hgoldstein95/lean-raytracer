import RayTracer.Widget.PPM
import RayTracer.Basic

def world :=
  Entity.ofList [
    -- Ground
    .sphere
      (center := ⟨0, -100.5, -1⟩)
      (radius := 100)
      (material := .lambertian ⟨0.8, 0.8, 0.0⟩),
    -- Center
    .sphere
      (center := ⟨0, 0, -1.2⟩)
      (radius := 0.5)
      (material := .lambertian ⟨0.1, 0.2, 0.5⟩),
    -- Left
    .sphere
      (center := ⟨-1.0, 0.0, -1.0⟩)
      (radius := 0.5)
      (material := .dialectric 1.5),
    -- Right
    .sphere
      (center := ⟨1.0, 0.0, -1.0⟩)
      (radius := 0.5)
      (material := .metal ⟨0.8, 0.6, 0.2⟩ 1.0),
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
