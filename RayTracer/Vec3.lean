namespace Vec3

structure Vec3' (α : Type) where
  x : α
  y : α
  z : α
  deriving BEq, Repr

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

@[reducible]
def Vec3 := Vec3' Float

def Vec3.length (v : Vec3) : Float :=
  Float.sqrt <| (v.x * v.x) + (v.y * v.y) + (v.z * v.z)

def Vec3.normalize (v : Vec3) : Vec3 :=
  v / v.length

@[reducible]
def Point3 := Vec3' Float

end Vec3
