-- In range [0, 1)
def IO.randFloat : IO Float := do
  let g ← IO.stdGenRef.get
  let ⟨lo, hi⟩ := RandomGen.range g
  let ⟨n, g'⟩ := RandomGen.next g
  IO.stdGenRef.set g'
  pure ((n - lo).toUInt64.toFloat / ((hi - lo).toUInt64.toFloat + 1))

def IO.randFloatInRange (min max : Float) : IO Float := do
  let f ← IO.randFloat
  pure (min + (max - min) * f)
