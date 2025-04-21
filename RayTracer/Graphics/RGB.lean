import RayTracer.Geometry.Vec3

open Vec3

@[reducible]
def RGB := Vec3' UInt8

def RGB.r (px : RGB) : UInt8 := px.x
def RGB.g (px : RGB) : UInt8 := px.y
def RGB.b (px : RGB) : UInt8 := px.z

def RGB.display (px : RGB) : String :=
  s!"{px.r} {px.g} {px.b}"

def RGB.ofVec3 (v : Vec3) : RGB :=
  let ir := (255.999 * v.x).toUInt8
  let ig := (255.999 * v.y).toUInt8
  let ib := (255.999 * v.z).toUInt8
  ⟨ir, ig, ib⟩
