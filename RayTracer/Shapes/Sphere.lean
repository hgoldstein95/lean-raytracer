import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Hit

open Ray

structure Sphere where
  center : Point3
  radius : Float
  deriving BEq, Repr

instance : Hit Sphere where
  hit := λ (s : Sphere) (r : Ray) tRange => do
    let oc : Vec3 := s.center - r.origin
    let a := r.direction.lengthSquared
    let h := r.direction ⬝ oc
    let c := oc.lengthSquared - s.radius * s.radius

    let discriminant := h * h - a * c
    if discriminant < 0 then none

    let sqrtd := discriminant.sqrt
    let root₁ := (h - sqrtd) / a
    let root₂ := (h + sqrtd) / a

    let root : Option Float :=
      if tRange.surrounds root₁ then
        root₁
      else if tRange.surrounds root₂ then
        root₂
      else
        none

    if let some t := root then
      let point := r.at t
      let outwardNormal := (point - s.center) / s.radius
      Collision.mkWithOutwardNormal r t point outwardNormal
    else
      none
