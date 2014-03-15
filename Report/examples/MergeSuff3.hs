mergeSuff (One token1) (Str s) trans2 =
  let toks2 = trans2 startState
  in if isValid toks2
     then Multi $ toks2 {currentSeq = token1 <| currentSeq toks2}
     else Multi $ createTokens (singleton token1) (Str s) (-1)
