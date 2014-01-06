infixl 5 (|>)
(|>)                       :: FingerTree a -> a -> FingerTree a
Empty               (|>) a = Single a 
Single b            (|>) a = Deep [b] Empty [a]
Deep pr m [e,d,c,b] (|>) a = Deep pr (m (|>) Node3 e d c) [b,a]
Deep pr m sf        (|>) a = Deep pr m (sf ++ [a])
