nodes :: [a ] -> [Node a]
nodes [a, b]           = [Node2 a b ]
nodes [a, b, c]        = [Node3 a b c ]
nodes [a, b, c, d]     = [Node2 a b,Node2 c d ]
nodes (a : b : c : xs) =  Node3 a b c : nodes xs
