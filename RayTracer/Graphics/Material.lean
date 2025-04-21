import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3

structure ScatterResult where
  attenuation : Float
  scattered : Ray

def Material :=
  (r : Ray) →
  (normal : Vec3) →
  (point : Point3) →
  IO ScatterResult

namespace Lambertian

def mk (albedo : Float) : Material := λ _ normal point => do
  let v ← Vec3.randomUnit
  let directionCandidate : Vec3 := normal + v

  let direction :=
    if directionCandidate.isNearZero then normal else directionCandidate

  pure {
    scattered := {origin := point, direction},
    attenuation := albedo,
  }

end Lambertian
