import RayTracer.Graphics.RGB

open RGB

namespace PPM

structure PPM where
  width : UInt64
  height : UInt64
  pixels : Array RGB
  deriving Repr

def PPM.empty (width height : UInt64) : PPM :=
  {width, height, pixels := Array.emptyWithCapacity (width.toNat * height.toNat)}

def PPM.addPixel (image : PPM) (px : RGB) : PPM :=
  {image with pixels := image.pixels.push px}

def PPM.display (image : PPM) : String :=
  let pixels := image.pixels.foldr (λ x acc => x.display ++ "\n" ++ acc) ""
  s!"P3\n{image.width} {image.height}\n255\n{pixels}"

end PPM
