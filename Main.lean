import Lean
import RayTracer.Widget.PPM
import RayTracer.Basic

open Lean (ToJson FromJson Json)

def main : IO Unit := do
  let config := {
    CameraConfig.std with
    logging := true,
    samplesPerPixel := 100,
    lookFrom := ⟨-2, 2, 1⟩,
    lookAt := ⟨0, 0, -1⟩,
    vfov := 20,
  }

  let scene ← IO.FS.readFile "scenes/four-spheres.json"
  let .ok json := Json.parse scene
    | IO.throwServerError s!"Failed to parse"
  let .ok world := FromJson.fromJson? json
    | IO.throwServerError s!"Failed to deserialize"

  let camera ← Camera.init config
  IO.println <| PPM.display (← camera.render world)
