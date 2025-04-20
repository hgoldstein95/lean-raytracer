import RayTracer.Vec3

open Vec3

namespace Ray

structure Ray where
  origin : Point3
  direction : Vec3
  deriving BEq, Repr

def Ray.at (r : Ray) (t : Float) : Point3 :=
  r.origin + t * r.direction

end Ray
