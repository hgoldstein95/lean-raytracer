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

def IO.randFloat : IO Float := do
  let g ← IO.stdGenRef.get
  let ⟨lo, hi⟩ := RandomGen.range g
  let ⟨n, g'⟩ := RandomGen.next g
  IO.stdGenRef.set g'
  pure ((n - lo).toFloat / (hi - lo).toFloat + 1)
