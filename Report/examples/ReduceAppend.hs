(<|') :: (Reduce f) => f a -> FingerTree a -> FingerTree a
(<|') = reducer (<|)

(|>') :: (Reduce f) => FingerTree a  -> f a -> FingerTree a
(|>') = reducel (|>)
