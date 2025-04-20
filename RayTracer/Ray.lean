import RayTracer.Vec3

open Vec3

structure Ray where
  origin : Point3
  direction : Vec3
  deriving BEq, Repr

namespace Ray

def Ray.at (r : Ray) (t : Float) : Point3 :=
  r.origin + t * r.direction

end Ray
