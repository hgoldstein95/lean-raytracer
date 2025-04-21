import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3

structure ScatterResult where
  attenuation : Vec3
  scattered : Ray

def Material :=
  (r : Ray) →
  (normal : Vec3) →
  (point : Point3) →
  IO ScatterResult

namespace Lambertian

def mk (albedo : Vec3) : Material := λ _ normal point => do
  let v ← Vec3.randomUnit
  let directionCandidate : Vec3 := normal + v

  let direction :=
    if directionCandidate.isNearZero then normal else directionCandidate

  pure {
    scattered := {origin := point, direction},
    attenuation := albedo,
  }

end Lambertian

namespace Metal

def mk (albedo : Vec3) : Material := λ r normal point => do
  let reflected := Vec3.reflect r.direction normal
  let scattered := {origin := point, direction := reflected}
  let attenuation := albedo
  return {scattered, attenuation}

end Metal
