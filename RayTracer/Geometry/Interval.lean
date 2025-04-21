import RayTracer.Util

structure Interval where
  min : Float
  max : Float
  deriving BEq, Repr

namespace Interval

def size (i : Interval) : Float := i.max - i.min

def contains (i : Interval) (x : Float) : Bool :=
  i.min <= x && x <= i.max

def surrounds (i : Interval) (x : Float) : Bool :=
  i.min < x && x < i.max

def clamp (i : Interval) (x : Float) : Float :=
  if x < i.min then i.min else if i.max < x then i.max else x

def everything : Interval := ⟨-Float.infinity, Float.infinity⟩

def nothing : Interval := ⟨Float.infinity, -Float.infinity⟩

end Interval

instance : Inhabited Interval where
  default := Interval.everything
