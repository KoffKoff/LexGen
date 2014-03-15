data FingerTree v a = Empty
| Single a
| Deep v (Digit a) (FingerTree v (Node v a)) (Digit a)

deep :: (Measured a v) =>
  Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf = Deep ($\parallel$pr$\parallel$ $\bullet$ $\parallel$m$\parallel$ $\bullet$ $\parallel$sf$\parallel$) pr m sf

instance (Measured a v) => Measured (FingerTree v a) v where
  measure Empty      = $\emptyset$
  measure (Single x) = measure x
  measure (Deep v)   = v
