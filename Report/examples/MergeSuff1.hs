mergeSuff (Multi toks1) suff2 trans2 = Multi $
  let newToks = combineWithRHS toks1 trans2
  in if isValid $ newToks
     then newToks
     else let newSuff = mergeSuff (lastToken toks1) suff2 trans2
          in toks1 {lastToken = newSuff}
