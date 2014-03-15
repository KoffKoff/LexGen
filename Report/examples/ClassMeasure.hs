class (Monoid v) => Measured a v where
  measure :: a -> v
