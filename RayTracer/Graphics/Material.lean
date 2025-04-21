import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3
import RayTracer.Graphics.Collision

structure ScatterResult where
  attenuation : Vec3
  scattered : Ray

inductive Material where
  | lambertian (albedo : Vec3)
  | metal (albedo : Vec3) (fuzz : Float)
  | dialectric (refractionIndex : Float)
  deriving BEq, Repr

namespace Material

def scatter
    (m : Material)
    (r : Ray)
    (collision : Collision) :
    IO (Option ScatterResult) :=
  match m with
  | lambertian albedo => scatterLambertian albedo
  | metal albedo fuzz => scatterMetal albedo fuzz
  | dialectric refractionIndex => scatterDialectric refractionIndex
  where
    scatterLambertian albedo := do
      let v ← Vec3.randomUnit
      let directionCandidate : Vec3 := collision.normal + v

      let direction :=
        if directionCandidate.isNearZero then collision.normal
        else
          directionCandidate

      return some {
        scattered := {origin := collision.point, direction},
        attenuation := albedo,
      }

    scatterMetal albedo fuzz := do
      let v ← Vec3.randomUnit
      let reflected :=
        (Vec3.reflect r.direction collision.normal).normalize + (fuzz * v)
      let scattered : Ray := {origin := collision.point, direction := reflected}
      let attenuation := albedo
      return if (scattered.direction ⬝ collision.normal) > 0 then
        some {scattered, attenuation}
      else
        none

    scatterDialectric refractionIndex := do
      let ri :=
        if collision.frontFace then 1.0 / refractionIndex else refractionIndex
      let normDir := r.direction.normalize
      let refracted := Vec3.refract normDir collision.normal ri
      let scattered := {origin := collision.point, direction := refracted}
      return some {scattered, attenuation := 1}

end Material
