import Lean
import RayTracer.Util
import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Interval
import RayTracer.Graphics.Material
import RayTracer.Graphics.Collision

open Lean (ToJson FromJson)

inductive Entity where
  | sphere
      (center : Point3)
      (radius : Float)
      (material : Material)
  | ofList
      (objs : List Entity)
  deriving BEq, Repr, ToJson, FromJson

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

    minByT? :=
      @List.min?  _ {min a b := if min a.1.t b.1.t == a.1.t then a else b}

    collideOfList objs :=
      objs
        |>.map (·.collide r tRange)
        |>.reduceOption
        |> minByT?

end Entity
