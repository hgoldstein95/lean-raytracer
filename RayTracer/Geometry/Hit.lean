import RayTracer.Util
import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Interval
import RayTracer.Graphics.Material

structure Collision where
  t : Float
  point : Point3
  normal : Vec3
  frontFace : Bool
  material : Material

def Collision.mkWithOutwardNormal
    (r : Ray)
    (t : Float)
    (point : Point3)
    (outwardNormal : Vec3)
    (material : Material) :
    Collision :=
  let frontFace := (r.direction ⬝ outwardNormal) < 0.0
  let normal := if frontFace then outwardNormal else -1.0 * outwardNormal
  {t, point, normal, frontFace, material}

class Hit (α : Type u) where
  hit
    (obj : α)
    (r : Ray)
    (tRange : Interval) :
    Option Collision

instance [Hit α] : Hit (List α) where
  -- TODO: Refactor this, it doesn't need to be so imperative
  hit := λ (objs : List α) (r : Ray) tRange => do
    let mut result := none
    let mut closestSoFar := tRange.max
    for obj in objs do
      let tRange' := {tRange with max := closestSoFar}
      if let some collision := Hit.hit obj r tRange' then do
        closestSoFar := collision.t
        result := some collision
    result

def Hittable : Type 1 := Σ (α : Type), Hit α × α

def Hittable.mk {α : Type} [Hit α] (a : α) : Hittable := ⟨α, by assumption, a⟩

instance : Hit Hittable where
  hit := λ ⟨_, _, a⟩ r tRange => Hit.hit a r tRange
