import RayTracer.Util

structure Vec3' (α : Type) where
  x : α
  y : α
  z : α
  deriving BEq, Repr

namespace Vec3'

instance [ToString α] : ToString (Vec3' α) where
  toString v := s!"⟨{v.x}, {v.y}, {v.z}⟩"

instance [OfNat α 0] : OfNat (Vec3' α) 0 where
  ofNat := ⟨0, 0, 0⟩

instance [OfNat α 1] : OfNat (Vec3' α) 1 where
  ofNat := ⟨1, 1, 1⟩

instance [Add α] : HAdd (Vec3' α) (Vec3' α) (Vec3' α) where
  hAdd v w := {
    x := v.x + w.x,
    y := v.y + w.y,
    z := v.z + w.z
  }

instance [Add α] : HAdd (Vec3' α) α (Vec3' α) where
  hAdd v c := v + ⟨c, c, c⟩

instance [Add α] : HAdd α (Vec3' α) (Vec3' α) where
  hAdd c v := v + c

instance [Sub α] : HSub (Vec3' α) (Vec3' α) (Vec3' α) where
  hSub v w := {
    x := v.x - w.x,
    y := v.y - w.y,
    z := v.z - w.z
  }

instance [Sub α] : HSub (Vec3' α) α (Vec3' α) where
  hSub v c := v - ⟨c, c, c⟩

instance [Mul α] : HMul (Vec3' α) α (Vec3' α) where
  hMul v c := {
    x := v.x * c,
    y := v.y * c,
    z := v.z * c
  }

instance [Mul α] : HMul α (Vec3' α) (Vec3' α) where
  hMul c v := v * c

instance
    [HMul (Vec3' α) α (Vec3' α)] [Div α] [OfNat α 1] :
    HDiv (Vec3' α) α (Vec3' α)
  where
  hDiv v c := v * (1 / c)

def dot [Add α] [Mul α] (v w : Vec3' α) : α :=
  v.x * w.x +
  v.y * w.y +
  v.z * w.z

notation a " ⬝ " b => dot a b

end Vec3'

@[reducible]
def Vec3 := Vec3' Float

namespace Vec3

def lengthSquared (v : Vec3) : Float :=
  (v.x * v.x) +
  (v.y * v.y) +
  (v.z * v.z)

def length (v : Vec3) : Float := v.lengthSquared.sqrt

def normalize (v : Vec3) : Vec3 := v / v.length

def projectToHemisphere (v : Vec3) (normal : Vec3) : Vec3 :=
  if (v ⬝ normal) > 0.0 then v else -1.0 * v

def random : IO Vec3 := do
  pure ⟨(← IO.randFloat), (← IO.randFloat), (← IO.randFloat)⟩

def randomInRange (min max : Float) : IO Vec3 := do
  pure ⟨
      (← IO.randFloatInRange min max),
      (← IO.randFloatInRange min max),
      (← IO.randFloatInRange min max)
    ⟩

partial def randomUnit : IO Vec3 := do
  let p ← Vec3.randomInRange (-1.0) 1.0
  let lenSq := p.lengthSquared
  if 1e-160 < lenSq && lenSq <= 1 then
    pure (p / lenSq.sqrt)
  else
    Vec3.randomUnit

end Vec3

abbrev Point3 := Vec3
