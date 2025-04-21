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

def lambertian (albedo : Float) : Material := λ _ normal point => do
  let v ← Vec3.randomUnit
  let direction := normal + v
  pure {
    scattered := {origin := point, direction},
    attenuation := albedo,
  }
