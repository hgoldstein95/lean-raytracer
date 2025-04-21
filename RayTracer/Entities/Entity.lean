import RayTracer.Util
import RayTracer.Geometry.Vec3
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Interval
import RayTracer.Graphics.Material
import RayTracer.Graphics.Collision

def Entity :=
  (r : Ray) →
  (tRange : Interval) →
  Option Collision

def Entity.ofList (objs : List Entity) : Entity :=
  λ (r : Ray) tRange => do
    let mut result := none
    let mut closestSoFar := tRange.max
    for obj in objs do
      let tRange' := {tRange with max := closestSoFar}
      if let some collision := obj r tRange' then do
        closestSoFar := collision.t
        result := some collision
    result
