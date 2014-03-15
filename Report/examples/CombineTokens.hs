combineTokens trans1 trans2 in_state
    | isInvalid toks1 = toks1
    | isEmpty toks1   = trans2 in_state
    | otherwise = combineWithRHS toks1 trans2
  where toks1 = trans1 in_state
