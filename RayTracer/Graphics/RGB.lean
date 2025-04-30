import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Interval

open Vec3

structure RGB where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving Repr, BEq

instance : Inhabited RGB where
  default := ⟨0, 0, 0⟩

def RGB.display (px : RGB) : String :=
  s!"{px.r} {px.g} {px.b}"

private def Float.linearToGamma (x : Float) : Float :=
  if x > 0 then x.sqrt else 0

def RGB.ofVec3 (v : Vec3) : RGB :=
  let intensity := Interval.mk 0.0 0.999
  let ir := (256 * intensity.clamp v.x.linearToGamma).toUInt8
  let ig := (256 * intensity.clamp v.y.linearToGamma).toUInt8
  let ib := (256 * intensity.clamp v.z.linearToGamma).toUInt8
  ⟨ir, ig, ib⟩
