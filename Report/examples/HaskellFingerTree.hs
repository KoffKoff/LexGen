data FingerTree a = Empty
                | Single a
                | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                
type Digit a = [a]

data Node a = Node2 a a | Node3 a a a