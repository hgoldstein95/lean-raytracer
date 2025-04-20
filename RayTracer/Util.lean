def progressBar (p outOf : Nat) (ticks : Nat := 10) : String :=
  let filled := ((p.toFloat / outOf.toFloat) * ticks.toFloat).toUInt64.toNat
  let unFilled := ticks - filled
  List.replicate filled "█" ++ List.replicate unFilled " "
    |> String.join
    |> (· ++ "|")
    |> ("|" ++ ·)
