import RayTracer.Vec3
import RayTracer.Ray

open Vec3

structure Collision where
  t : Float
  point : Point3
  normal : Vec3
  deriving BEq, Repr

class Hit (α : Type) where
  hit
    (obj : α)
    (r : Ray)
    (tmin : Float := -1.0 / 0.0)
    (tmax : Float := 1.0 / 0.0) :
    Option Collision

-- def Ray.hitSphere (r : Ray) (s : Sphere) : Option Float :=
