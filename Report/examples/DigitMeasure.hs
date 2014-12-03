instance (Measured a v) => Measured (Digit a) v where
  measure xs = reducel (\i a -> i <> measure a) mempty xs
