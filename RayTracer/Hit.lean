import RayTracer.Vec3
import RayTracer.Ray

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

class Hit (α : Type) where
  hit
    (obj : α)
    (r : Ray)
    (tmin : Float := -1.0 / 0.0)
    (tmax : Float := 1.0 / 0.0) :
    Option Collision

instance [Hit α] : Hit (List α) where
  hit := λ (objs : List α) (r : Ray) tmin tmax => do
    let mut result := none
    let mut closestSoFar := tmax
    for obj in objs do
      if let some collision := Hit.hit obj r tmin closestSoFar then do
        closestSoFar := collision.t
        result := some collision
    result
