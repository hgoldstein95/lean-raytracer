import Lean
import RayTracer.Widget.PPM
import RayTracer.Basic

open Lean (ToJson FromJson Json)

def main : IO Unit := do
  let config : CameraConfig := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 50,
    logging := true,
    samplesPerPixel := 100,
  }

  let scene := include_str "scenes" / "four-spheres.json"
  let .ok json := Json.parse scene
    | IO.throwServerError s!"Failed to parse"
  let .ok world := FromJson.fromJson? json
    | IO.throwServerError s!"Failed to deserialize"

  let camera ← Camera.init config
  IO.println <| PPM.display (← camera.render world)
