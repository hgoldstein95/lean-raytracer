import Lean
import RayTracer.Widget.PPM
import RayTracer.Basic

open Lean (ToJson FromJson Json)

def main : IO Unit := do
  let config := {
    CameraConfig.std with
    logging := true,

    maxRayDepth := 50,
    samplesPerPixel := 8 * 12,
    cores := 2,

    vfov := 20,
    lookFrom := ⟨-2, 2, 1⟩,
    lookAt := ⟨0, 0, -1⟩,
    vUp := ⟨0, 1, 0⟩,
    defocusAngle := 10,
    focusDistance := 3.4,
  }

  let scene ← IO.FS.readFile "scenes/four-spheres.json"
  let .ok json := Json.parse scene
    | IO.throwServerError s!"Failed to parse"
  let .ok world := FromJson.fromJson? json
    | IO.throwServerError s!"Failed to deserialize"

  let camera ← Camera.init config
  IO.println <| PPM.display (← camera.render world)
