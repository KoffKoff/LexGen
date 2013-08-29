
type State = Int
type Transition = State -> Tokens -- Transition from in state to Tokens
data Tokens    = Tokens {currentSeq :: Seq PartToken
                        ,outState   :: State
                        ,suffix     :: Suffix}
                 deriving Show
data Suffix    = None
               | End {getToks :: Tokens}
                 deriving Show
data Size      = Size Int
type LexTree   = FingerTree (Table State Tokens,Size) Char
data PartToken = Token { lexeme      :: String 
                       , token_id    :: Accepts}
type Accepts   = [AlexAcc (Posn -> String -> Token) ()]
