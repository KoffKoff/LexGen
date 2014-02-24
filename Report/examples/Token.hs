data Tokens    = NoTokens
               | InvalidTokens (Seq Char)
               | Tokens { currentSeq :: (Seq Token)
                        , lastToken  :: Suffix
                        , outState   :: State}
