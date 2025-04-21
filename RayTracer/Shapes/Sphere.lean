import RayTracer.Vec3
import RayTracer.Hit
import RayTracer.Ray

open Ray

structure Sphere where
  center : Point3
  radius : Float
  deriving BEq, Repr

instance : Hit Sphere where
  hit := λ (s : Sphere) (r : Ray) tmin tmax => do
    let oc : Vec3 := s.center - r.origin
    let a := r.direction.lengthSquared
    let h := r.direction ⬝ oc
    let c := oc.lengthSquared - s.radius * s.radius

    let discriminant := h * h - a * c
    if discriminant < 0 then none

    let root₁ := (h - discriminant.sqrt) / a
    let root₂ := (h + discriminant.sqrt) / a

    let root : Option Float :=
      if tmin <= root₁ && root₁ <= tmax then
        root₁
      else if tmin <= root₂ && root₂ <= tmax then
        root₂
      else
        none

    if let some t := root then
      let point := r.at t
      let outwardNormal := Vec3.normalize (point - s.center)
      Collision.mkOutwardNormal r t point outwardNormal
    else
      none
