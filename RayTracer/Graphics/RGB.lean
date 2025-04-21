import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Interval

open Vec3

@[reducible]
def RGB := Vec3' UInt8

def RGB.r (px : RGB) : UInt8 := px.x
def RGB.g (px : RGB) : UInt8 := px.y
def RGB.b (px : RGB) : UInt8 := px.z

def RGB.display (px : RGB) : String :=
  s!"{px.r} {px.g} {px.b}"

def RGB.ofVec3 (v : Vec3) : RGB :=
  let intensity := Interval.mk 0.0 0.999
  let ir := (256 * intensity.clamp v.x).toUInt8
  let ig := (256 * intensity.clamp v.y).toUInt8
  let ib := (256 * intensity.clamp v.z).toUInt8
  ⟨ir, ig, ib⟩
