data FingerTree v a = Empty
| Single a
| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

deep :: (Measured a v) =>
  Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep ((measure pr) $\bullet$ (measure m) $\bullet$ (measure sf)) pr m sf

instance (Measured a v) => Measured (FingerTree v a) v where
  measure Empty      = $\emptyset$
  measure (Single x) = measure x
  measure (Deep v)   = v
