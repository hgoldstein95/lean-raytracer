import RayTracer.FastRandom

def CameraM (α : Type) :=
  UInt32 → (α × UInt32)

instance : Monad CameraM where
  pure x := λ s => ⟨x, s⟩
  bind x f := λ s =>
    let ⟨a, s'⟩ := x s
    f a s'

namespace CameraM

def randFloat : CameraM Float := FastRandom.next

def randFloatInRange (min max : Float) : CameraM Float := do
  let f ← CameraM.randFloat
  pure (min + (max - min) * f)

def run (seed : UInt32) (c : CameraM α) : α := (c seed).fst

end CameraM
