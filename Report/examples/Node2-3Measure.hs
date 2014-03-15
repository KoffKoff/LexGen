data Node v a = Node2 v a a | Node3 v a a a

node2 :: (Measured a v) => a -> a -> Node v a
node2 a b = Node2 ($\parallel$a$\parallel$ $\bullet$ $\parallel$b$\parallel$) a b

node3 :: (Measured a v) => a -> a -> a -> Node v a
node3 a b c = Node3 ($\parallel$a$\parallel$ $\bullet$ $\parallel$b$\parallel$ $\bullet$ $\parallel$c$\parallel$) a b c

instance (Monoid v) => Measured (Node v a) v where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v
