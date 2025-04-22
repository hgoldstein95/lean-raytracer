namespace FastRandom

structure GenSt where
  s1 : UInt64
  s2 : UInt64

instance : Inhabited GenSt where
  default := ⟨0, 0⟩

instance : Repr GenSt where
  reprPrec | ⟨s1, s2⟩, _ => Std.Format.bracket "⟨" (repr s1 ++ ", " ++ repr s2) "⟩"

namespace GenSt

def init (s : UInt64 := 0) : GenSt :=
  let q  := s / 2147483562
  let s1 := s % 2147483562
  let s2 := q % 2147483398
  ⟨s1 + 1, s2 + 1⟩

def maxValue : Float := 2147483562

def next : GenSt → UInt64 × GenSt
  | ⟨s1, s2⟩ =>
    let k    := s1 / 53668
    let s1'  := 40014 * (s1 - k * 53668) - k * 12211
    let s1'' := if s1' < 0 then (s1' + 2147483563) else s1'
    let k'   := (s2 / 52774)
    let s2'  := 40692 * (s2 - k' * 52774) - k' * 3791
    let s2'' := if s2' < 0 then (s2' + 2147483399) else s2'
    let z    := s1'' - s2''
    let z'   := if z < 1 then (z + 2147483562) else z % 2147483562
    (z', ⟨s1'', s2''⟩)

end GenSt

end FastRandom

open FastRandom

initialize IO.fastRandomRef : IO.Ref GenSt ←
  let seed := ByteArray.toUInt64LE! (← IO.getRandomBytes 8)
  IO.mkRef (GenSt.init seed)

-- In range [0, 1)
def IO.randFloat : IO Float := do
  let n ← IO.fastRandomRef.modifyGet GenSt.next
  return (n.toFloat - 1.0) / GenSt.maxValue

def IO.randFloatInRange (min max : Float) : IO Float := do
  let f ← IO.randFloat
  pure (min + (max - min) * f)
