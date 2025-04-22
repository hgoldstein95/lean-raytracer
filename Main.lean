import Lean
import RayTracer.Widget.PPM
import RayTracer.Basic

open Lean (ToJson FromJson Json)

def main : IO Unit := do
  let config := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 50,
    logging := true,
    samplesPerPixel := 100,
  }

  let fileName := "scenes/four-spheres.json"
  let .ok json := Json.parse (← IO.FS.readFile fileName)
    | IO.throwServerError s!"Failed to parse {fileName}"
  let .ok world := FromJson.fromJson? json
    | IO.throwServerError s!"Failed to deserialize {fileName}"

  let camera ← Camera.init config
  IO.println <| PPM.display (← camera.render world)
