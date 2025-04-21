import RayTracer.Widget.PPM
import RayTracer.Basic

def main : IO Unit := do
  let config := {
    aspectRatio := 16.0 / 9.0,
    imageWidth := 400,
    maxRayDepth := 50,
    logging := true,
    samplesPerPixel := 100,
  }
  let camera ← Camera.init config
  let fileName := "scenes/four-spheres.json"
  let s ← IO.FS.readFile fileName
  let .ok json := Lean.Json.parse s
    | IO.throwServerError s!"Failed to parse {fileName}"
  match Lean.FromJson.fromJson? json with
  | .ok world => IO.println <| PPM.display (← camera.render world)
  | .error e => IO.throwServerError s!"Failed to deserialize {fileName}\n{e}"
