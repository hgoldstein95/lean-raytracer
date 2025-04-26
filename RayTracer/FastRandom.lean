namespace FastRandom

def next (oldState : UInt32) : Float × UInt32 := Id.run do
  let state := oldState * 747796405 + 2891336453
  let mut result : UInt32 := ((state >>> ((state >>> 28) + 4)).xor state) * 277803737
  result := (result >>> 22).xor result
  return ⟨result.toFloat / 4294967295.0, state⟩

end FastRandom

open FastRandom

initialize IO.fastRandomRef : IO.Ref UInt32 ←
  let seed := ByteArray.toUInt64LE! (← IO.getRandomBytes 8)
  IO.mkRef seed.toUInt32

-- In range [0, 1)
def IO.randFloat : IO Float := do
  IO.fastRandomRef.modifyGet FastRandom.next

def IO.randFloatInRange (min max : Float) : IO Float := do
  let f ← IO.randFloat
  pure (min + (max - min) * f)
