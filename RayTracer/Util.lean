def progressBar (p outOf : Nat) (ticks : Nat := 10) : String :=
  let filled := ((p.toFloat / outOf.toFloat) * ticks.toFloat).toUInt64.toNat
  let unFilled := ticks - filled
  List.replicate filled "█" ++ List.replicate unFilled " "
    |> String.join
    |> (· ++ "|")
    |> ("|" ++ ·)

def Float.infinity := 1.0 / 0.0

def Float.PI := 3.14159265358

def Float.degreesToRadians (degrees : Float) : Float :=
  degrees * Float.PI / 180.0

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
