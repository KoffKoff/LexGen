combineTokens :: Transition -> Transition -> Transition
combineTokens trans1 trans2 in_state
    | isInvalid toks1 = toks1
    | isEmpty toks1   = trans2 in_state
    | otherwise = combineWithRHS toks1 trans2
  where toks1 = getTokens trans1 in_state
