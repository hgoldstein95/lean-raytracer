import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
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
