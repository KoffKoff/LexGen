instance Reduce FingerTree where
  reducer (-<) Empty z          = z
  reducer (-<) (Single x) z     = x (-<) z
  reducer (-<) (Deep pr m sf) z = pr (-<') ( m (-<'') ( sf (-<') z ))
    where (-<')  = reducer (-<)
          (-<'') = reducer (reducer (-<))
     
  reducel (>-) z Empty          = z
  reducel (>-) z (Single x)     = z (>-) x
  reducel (>-) z (Deep pr m sf) = ((z (>-') pr ) (>-'') m ) (>-') sf
    where (>-')  = reducel (>-)
          (>-'') = reducel (reducel (>-))
