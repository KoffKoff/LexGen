tabulate :: (State,State) -> Transition -> Table
access :: Table -> Transition

tabulate range f = listArray range [f i | i <- [fst range..snd range]]
access a x = a ! x

instance Monoid Table where
  mempty = tabulate stateRange (\_ -> emptyTokens)
  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)