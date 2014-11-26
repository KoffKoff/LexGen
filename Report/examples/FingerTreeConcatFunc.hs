(><) ::FingerTree a -> FingerTree a -> FingerTree a
xs (><) ys = app3 xs [] ys

app3 :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
app3 Empty ts xs      = ts <|' xs
app3 xs ts Empty      = xs |>' ts
app3 (Single x) ts xs = x <| (ts <|' xs)
app3 xs ts (Single x) = (xs |>' ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2)
  = Deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2