type State = Int

type Transition = State -> Tokens

getTokens :: Transition -> State -> Tokens
getTokens trans state = trans state