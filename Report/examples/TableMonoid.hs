tabulate :: (State,State) -> (State -> b) -> Table State b
access :: Table State b -> (State -> b)

tabulate range f = listArray range [f i | i <- [fst range..snd range]]
access a x = a ! x

instance Monoid (Table State Tokens) where
  mempty = tabulate stateRange (\_ -> emptyTokens)
  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)