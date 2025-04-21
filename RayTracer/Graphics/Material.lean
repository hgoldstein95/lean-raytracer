import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3
import RayTracer.Graphics.Collision

structure ScatterResult where
  attenuation : Vec3
  scattered : Ray

def Material :=
  (r : Ray) →
  (collision : Collision) →
  IO (Option ScatterResult)

namespace Lambertian

def mk (albedo : Vec3) : Material := λ _ collision => do
  let v ← Vec3.randomUnit
  let directionCandidate : Vec3 := collision.normal + v

  let direction :=
    if directionCandidate.isNearZero then collision.normal else directionCandidate

  return some {
    scattered := {origin := collision.point, direction},
    attenuation := albedo,
  }

end Lambertian

namespace Metal

def mk (albedo : Vec3) (fuzz : Float) : Material := λ r collision => do
  let v ← Vec3.randomUnit
  let reflected := (Vec3.reflect r.direction collision.normal).normalize + (fuzz * v)
  let scattered : Ray := {origin := collision.point, direction := reflected}
  let attenuation := albedo
  return if (scattered.direction ⬝ collision.normal) > 0 then
    some {scattered, attenuation}
  else
    none

end Metal

namespace Dielectric

def mk (refractionIndex : Float) : Material := λ r collision => do
  let ri := if collision.frontFace then 1.0 / refractionIndex else refractionIndex
  let normDir := r.direction.normalize
  let refracted := Vec3.refract normDir collision.normal ri
  let scattered := {origin := collision.point, direction := refracted}
  return some {scattered, attenuation := 1}

end Dielectric
