viewL :: FingerTree a -> ViewL a
viewL Empty          = EmptyL
viewL (Single x)     = x :< Empty
viewL (Deep pr m sf) = head pr :< deepL (tail pr) m sf

deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL [] m sf = case viewL m of
  EmptyL  -> toTree sf
  a :< m' -> Deep (nodeToDigit a) m' sf
deepL pr m sf = Deep pr m sf

data ViewL a = EmptyL
             | a :< FingerTree a
