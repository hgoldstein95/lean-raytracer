import Lean
import RayTracer.FastRandom

structure Vec3' (α : Type) where
  x : α
  y : α
  z : α
  deriving BEq, Repr, Lean.ToJson, Lean.FromJson

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

instance [Mul α] : HMul (Vec3' α) (Vec3' α) (Vec3' α) where
  hMul v w := {
    x := v.x * w.x,
    y := v.y * w.y,
    z := v.z * w.z
  }

instance [Mul α] : HMul (Vec3' α) α (Vec3' α) where
  hMul v c := v * ⟨c, c, c⟩

instance [Mul α] : HMul α (Vec3' α) (Vec3' α) where
  hMul c v := v * c

instance [Neg α] : Neg (Vec3' α) where
  neg v := ⟨-v.x, -v.y, -v.z⟩

instance
    [HMul (Vec3' α) α (Vec3' α)] [Div α] [OfNat α 1] :
    HDiv (Vec3' α) α (Vec3' α)
  where
  hDiv v c := v * (1 / c)

def dot [Add α] [Mul α] (v w : Vec3' α) : α :=
  v.x * w.x +
  v.y * w.y +
  v.z * w.z

def cross [Mul α] [Sub α] (u v : Vec3' α) : Vec3' α :=
  {
    x := u.y * v.z - u.z * v.y,
    y := u.z * v.x - u.x * v.z,
    z := u.x * v.y - u.y * v.x,
  }

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
  if (v ⬝ normal) > 0.0 then v else -v

def isNearZero (v : Vec3) : Bool :=
  let s := 1e-8
  v.x.abs < s && v.y.abs < s && v.z.abs < s

def reflect (v n : Vec3) : Vec3 :=
  v - 2 * (v ⬝ n) * n

def refract (uv n : Vec3) (etaRatio : Float) : Vec3 :=
  let cosTheta := min (-uv ⬝ n) 1.0
  let rOutPerp : Vec3 := etaRatio * (uv + cosTheta * n)
  let rOutPar : Vec3 := -(1.0 - rOutPerp.lengthSquared).abs.sqrt * n
  rOutPerp + rOutPar

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
