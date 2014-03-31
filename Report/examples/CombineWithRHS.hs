combineWithRHS :: Tokens -> Transition -> Tokens
combineWithRHS toks1 trans2 | isEmpty toks2 = toks1
                            | isValid toks2 =
    let toks2' = mergeTokens (lastToken toks1) toks2 trans2
    in appendTokens seq1 toks2'                           
                            | otherwise = case lastToken toks1 of
    Multi suffToks ->
      let toks2' = combineWithRHS suffToks trans2
      in appendTokens seq1 toks2'
    One tok -> appendTokens (seq1 |> tok) (getTokens trans2 startState)
    Str s -> invalidTokens s
  where toks2 = getTokens trans2 (outState toks1)
        seq1 = currentSeq toks1
