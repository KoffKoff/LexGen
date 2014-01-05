data FingerTree a = Empty
                | Single a
                | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                
type Digit a = [a]
