import Lean
import RayTracer.CameraM

open Lean (ToJson FromJson)

structure Vec3 where
  x : Float
  y : Float
  z : Float
  deriving BEq, Repr, ToJson, FromJson

instance : Inhabited Vec3 where
  default := ⟨0, 0, 0⟩

instance : ToString Vec3 where
  toString v := s!"⟨{v.x}, {v.y}, {v.z}⟩"

instance : OfNat Vec3 0 where
  ofNat := ⟨0, 0, 0⟩

instance : OfNat Vec3 1 where
  ofNat := ⟨1, 1, 1⟩

instance : Add Vec3 where
  add v w :=
    {
      v with
      x := v.x + w.x,
      y := v.y + w.y,
      z := v.z + w.z
    }

instance : HAdd Vec3 Float Vec3 where
  hAdd v c :=
    {
      v with
      x := v.x + c,
      y := v.y + c,
      z := v.z + c,
    }

instance : HAdd Float Vec3 Vec3 where
  hAdd c v := v + c

instance : Sub Vec3 where
  sub v w :=
    {
      v with
      x := v.x - w.x,
      y := v.y - w.y,
      z := v.z - w.z
    }

instance : HSub Vec3 Float Vec3 where
  hSub v c :=
    {
      v with
      x := v.x - c,
      y := v.y - c,
      z := v.z - c,
    }

instance : Mul Vec3 where
  mul v w :=
    {
      v with
      x := v.x * w.x,
      y := v.y * w.y,
      z := v.z * w.z,
    }

instance : HMul Vec3 Float Vec3 where
  hMul v c :=
    {
      v with
      x := v.x * c,
      y := v.y * c,
      z := v.z * c,
    }

instance : HMul Float Vec3 Vec3 where
  hMul c v := v * c

instance : Neg Vec3 where
  neg v :=
    {
      v with
      x := -v.x,
      y := -v.y,
      z := -v.z
    }

instance
    [HMul Vec3 α Vec3] [Div α] [OfNat α 1] :
    HDiv Vec3 α Vec3
  where
  hDiv v c := v * (1 / c)

namespace Vec3

def dot (v w : Vec3) : Float :=
  v.x * w.x +
  v.y * w.y +
  v.z * w.z

notation a " ⬝ " b => dot a b

def cross (u v : Vec3) : Vec3 :=
  {
    x := u.y * v.z - u.z * v.y,
    y := u.z * v.x - u.x * v.z,
    z := u.x * v.y - u.y * v.x,
  }

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

def random : CameraM Vec3 := do
  pure ⟨(← .randFloat), (← .randFloat), (← .randFloat)⟩

def randomInRange (min max : Float) : CameraM Vec3 := do
  pure ⟨
      (← .randFloatInRange min max),
      (← .randFloatInRange min max),
      (← .randFloatInRange min max)
    ⟩

partial def randomUnit : CameraM Vec3 := do
  let p ← Vec3.randomInRange (-1.0) 1.0
  let lenSq := p.lengthSquared
  if 1e-160 < lenSq && lenSq <= 1 then
    return (p / lenSq.sqrt)
  randomUnit

partial def randomInUnitDisk : CameraM Vec3 := do
  let p : Vec3 :=
    ⟨(← .randFloatInRange (-1) 1), (← .randFloatInRange (-1) 1), 0⟩
  if p.lengthSquared < 1 then
    return p
  randomInUnitDisk

end Vec3

abbrev Point3 := Vec3
