instance (Measured a v) => Measured (Digit a) v where
  measure xs = foldl (\v x -> v <> measure x) mempty xs

data Node v a = Node2 v a a | Node3 v a a a

node2 :: (Measured a v) => a -> a -> Node v a
node2 a b = Node2 (measure a <> measure b) a b

node3 :: (Measured a v) => a -> a -> a -> Node v a
node3 a b c = Node3 (measure a <> measure b <> measure c) a b c

instance (Monoid v) => Measured (Node v a) v where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v
