import RayTracer.Util
import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Interval
import RayTracer.Graphics.Material
import RayTracer.Graphics.Collision

inductive Entity where
  | sphere
      (center : Point3)
      (radius : Float)
      (material : Material)
  | ofList
      (objs : List Entity)
  deriving BEq, Repr

namespace Entity

def collide
    (e : Entity)
    (r : Ray)
    (tRange : Interval) :
    Option (Collision × Material) :=
  match e with
  | sphere center radius material => collideSphere center radius material
  | ofList objs => collideOfList objs
  where
    collideSphere center radius material := do
      let oc : Vec3 := center - r.origin
      let a := r.direction.lengthSquared
      let h := r.direction ⬝ oc
      let c := oc.lengthSquared - radius * radius

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
        let outwardNormal := (point - center) / radius
        some ⟨Collision.mkWithOutwardNormal r t point outwardNormal, material⟩
      else
        none

    collideOfList objs := do
      let mut result := none
      let mut closestSoFar := tRange.max
      for obj in objs do
        let tRange' := {tRange with max := closestSoFar}
        if let some p := obj.collide r tRange' then do
          closestSoFar := p.fst.t
          result := some p
      result

end Entity
