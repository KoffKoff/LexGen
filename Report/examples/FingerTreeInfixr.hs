(<|)                       :: a -> FingerTree a -> FingerTree a
a (<|) Empty               = Single a 
a (<|) Single b            = Deep [a] Empty [b]
a (<|) Deep [b,c,d,e] m sf = Deep [a, b] (Node3 c d e (<|) m) sf
a (<|) Deep pr m sf        = Deep([a] ++ pr) m sf
