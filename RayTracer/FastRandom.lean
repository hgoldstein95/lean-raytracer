namespace FastRandom

def next (oldState : UInt32) : Float × UInt32 := Id.run do
  let state := oldState * 747796405 + 2891336453
  let mut result : UInt32 := ((state >>> ((state >>> 28) + 4)).xor state) * 277803737
  result := (result >>> 22).xor result
  return ⟨result.toFloat / 4294967295.0, state⟩

end FastRandom
