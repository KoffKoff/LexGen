instance Reduce Node where
    reducer (-<) (Node2 a b) z   = a (-<) (b (-<) z)
    reducer (-<) (Node3 a b c) z = a (-<) (b (-<) (c (-<) z))
    
    reducel (>-) z (Node2 a b)   = (z (>-) b) (>-) a
    reducel (>-) z (Node3 c b a) = ((z (>-) c) (>-) b) (>-) a 
