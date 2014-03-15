instance (Measured a v) => Measured (Digit a) v where
measure xs = reducel (\i a -> i $\bullet$ measure a) $\bullet$ xs
