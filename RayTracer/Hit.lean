import RayTracer.Vec3
import RayTracer.Ray
import RayTracer.Util

open Vec3

structure Collision where
  t : Float
  point : Point3
  normal : Vec3
  frontFace : Bool
  deriving BEq, Repr

def Collision.mkOutwardNormal
    (r : Ray)
    (t : Float)
    (point : Point3)
    (outwardNormal : Vec3) :
    Collision :=
  let frontFace := (r.direction ⬝ outwardNormal) < 0
  let normal := if frontFace then outwardNormal else -1.0 * outwardNormal
  {
    t := t,
    point := point,
    normal := normal,
    frontFace := frontFace
  }

class Hit (α : Type u) where
  hit
    (obj : α)
    (r : Ray)
    (tmin : Float := -Float.infinity)
    (tmax : Float := Float.infinity) :
    Option Collision

instance [Hit α] : Hit (List α) where
  -- TODO: Refactor this, it doesn't need to be so imperative
  hit := λ (objs : List α) (r : Ray) tmin tmax => do
    let mut result := none
    let mut closestSoFar := tmax
    for obj in objs do
      if let some collision := Hit.hit obj r tmin closestSoFar then do
        closestSoFar := collision.t
        result := some collision
    result

def Hittable : Type 1 := Σ (α : Type), Hit α × α

def Hittable.mk {α : Type} [Hit α] (a : α) : Hittable := ⟨α, by assumption, a⟩

instance : Hit Hittable where
  hit := λ ⟨_, _, a⟩ r tmin tmax => Hit.hit a r tmin tmax
