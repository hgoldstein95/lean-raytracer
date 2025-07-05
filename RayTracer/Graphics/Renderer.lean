import RayTracer.Graphics.Camera

def averageAll (pxs : List (Array Vec3)) : Array Vec3 := Option.get! do
  let n ← (·.size) <$> pxs[0]?
  return Array.ofFn (n := n) (λ i => Option.get! do
    let row ← pxs.mapM (λ row => row[i]?)
    return row.sum / row.length.toUInt32.toFloat)

def render
    (camera : Camera)
    (world : Entity) :
    IO PPM := do
  let pixelsTask :=
    Task.mapList (λ xs => (averageAll xs).map RGB.ofVec3) <|
      ← (List.range camera.cores).mapM λ i =>
        BaseIO.asTask (prio := .dedicated) (Camera.computePixels camera world i.toUInt32)
  return {
      width := camera.imageWidth,
      height := camera.imageHeight,
      pixels := pixelsTask.get,
    }
