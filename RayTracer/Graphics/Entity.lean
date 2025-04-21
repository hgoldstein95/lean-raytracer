import RayTracer.Util
import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Interval
import RayTracer.Graphics.Material
import RayTracer.Graphics.Collision

def Entity :=
  (r : Ray) →
  (tRange : Interval) →
  Option (Collision × Material)

def Entity.ofList (objs : List Entity) : Entity :=
  λ (r : Ray) tRange => do
    let mut result := none
    let mut closestSoFar := tRange.max
    for obj in objs do
      let tRange' := {tRange with max := closestSoFar}
      if let some p := obj r tRange' then do
        closestSoFar := p.fst.t
        result := some p
    result

namespace Sphere

structure Args where
  center : Point3
  radius : Float
  material : Material

def mk (s : Args) : Entity := λ (r : Ray) tRange => do
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
    let point := r.atT t
    let outwardNormal := (point - s.center) / s.radius
    some ⟨Collision.mkWithOutwardNormal r t point outwardNormal, s.material⟩
  else
    none

end Sphere
