import Lean
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Vec3
import RayTracer.Graphics.Collision
import RayTracer.Util

open Lean (ToJson FromJson)

structure ScatterResult where
  attenuation : Vec3
  scattered : Ray

inductive Material where
  | lambertian (albedo : Vec3)
  | metal (albedo : Vec3) (fuzz : Float)
  | dialectric (refractionIndex : Float)
  deriving BEq, Repr, ToJson, FromJson

namespace Material

def scatter
    (m : Material)
    (r : Ray)
    (collision : Collision) :
    CameraM (Option ScatterResult) :=
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

    reflectance cosine refractionIndex :=
      let r0 := (1.0 - refractionIndex) / (1.0 + refractionIndex)
      let r0 := r0 * r0
      r0 + (1 - r0) * Float.pow (1 - cosine) 5

    scatterDialectric refractionIndex := do
      let ri :=
        if collision.frontFace then
          1.0 / refractionIndex
        else
          refractionIndex

      let unitDir := r.direction.normalize
      let cosTheta := min (-unitDir ⬝ collision.normal) 1.0
      let sinTheta := (1.0 - cosTheta * cosTheta).sqrt

      let cannotRefract := (ri * sinTheta) > 1.0
      let f ← .randFloat
      let direction :=
        if cannotRefract || reflectance cosTheta ri > f then
          Vec3.reflect unitDir collision.normal
        else
          Vec3.refract unitDir collision.normal ri

      let scattered := {origin := collision.point, direction}
      return some {scattered, attenuation := 1}

end Material
