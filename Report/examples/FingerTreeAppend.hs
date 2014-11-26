(<|') :: [a] -> FingerTree a -> FingerTree a
(<|') = flip (foldr (<|))

(|>') :: FingerTree a -> [a] -> FingerTree a
(|>') = foldl (|>)