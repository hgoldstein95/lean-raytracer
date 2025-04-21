import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3

structure ScatterResult where
  attenuation : Vec3
  scattered : Ray

def Material :=
  (r : Ray) →
  (normal : Vec3) →
  (point : Point3) →
  IO (Option ScatterResult)

namespace Lambertian

def mk (albedo : Vec3) : Material := λ _ normal point => do
  let v ← Vec3.randomUnit
  let directionCandidate : Vec3 := normal + v

  let direction :=
    if directionCandidate.isNearZero then normal else directionCandidate

  return some {
    scattered := {origin := point, direction},
    attenuation := albedo,
  }

end Lambertian

namespace Metal

def mk (albedo : Vec3) (fuzz : Float) : Material := λ r normal point => do
  let v ← Vec3.randomUnit
  let reflected := (Vec3.reflect r.direction normal).normalize + (fuzz * v)
  let scattered : Ray := {origin := point, direction := reflected}
  let attenuation := albedo
  return if (scattered.direction ⬝ normal) > 0 then
    some {scattered, attenuation}
  else
    none

end Metal
