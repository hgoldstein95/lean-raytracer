import RayTracer.Util
import RayTracer.Geometry.Vec3

structure OnlineMean where
  n : UInt32
  m : Vec3
  s : Vec3

namespace OnlineMean

def init : OnlineMean := {n := 0, m := 0, s := 0}

def addSample (prev : OnlineMean) (sample : Vec3) : OnlineMean := Id.run do
  let n := prev.n + 1
  let m := prev.m + (sample - prev.m) / n.toFloat
  let s := prev.s + (sample - m) * (sample - prev.m)
  return {n, m, s}

def convergenceDelta (om : OnlineMean) : Float :=
  (om.s / (om.n.toFloat - 1)).length

def mean (om : OnlineMean) : Vec3 := om.m

end OnlineMean
