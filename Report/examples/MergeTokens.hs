mergeTokens :: Suffix -> Tokens -> Transition -> Tokens
mergeTokens suff1 toks2 trans2 = case viewl (currentSeq toks2) of
  token2 :< seq2' -> let newToken = mergeToken suff1 token2
                     in toks2 {currentSeq = newToken <| seq2'}
  EmptyL -> case alex_accept ! out_state of
    [] -> let newSuff = mergeSuff suff1 (lastToken toks2) trans2
          in toks2 {lastToken = newSuff}
    acc -> let lex = suffToStr suff1 <>
                     suffToStr (lastToken toks2)
           in toks2 {lastToken = One $ createToken lex acc}
  where out_state = outState toks2
