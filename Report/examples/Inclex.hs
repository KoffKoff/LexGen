type State = Int
type Transition = State -> Tokens -- Transition from in state to Tokens
data Tokens = NoTokens
            | InvalidTokens !(Seq Char)
            | Tokens { currentSeq :: !(Seq IntToken)
                     , lastToken  :: !Suffix
                     , outState   :: !State}
-- The suffix is the sequence of as long as possible accepting tokens.
-- It can itself contain a suffix for the last token.
                 deriving Show
-- This is either a Sequence of tokens or one token if it hits an accepting
-- state with later characters
data Suffix   = Str !(Seq Char)
              | One !IntToken
              | Multi !Tokens
                 deriving Show
type Size     = Sum Int
type LexTree  = FingerTree (Table State Tokens,Size) Char
data IntToken = Token { lexeme   :: !(Seq Char)
                      , token_id :: Accepts}
type Accepts   = [AlexAcc (Posn -> Seq Char -> Token) ()]

tabulate :: (State,State) -> (State -> b) -> Table State b
access :: Table State b -> (State -> b)

{-- Functional Table variant
newtype Table a b = Tab {getFun :: a -> b}
tabulate _ f = Tab f
access a x = (getFun a) x
--}

type Table a b = Array State b
tabulate range f = listArray range [f i | i <- [fst range..snd range]]
access a x = a ! x

instance Monoid (Table State Tokens) where
  mempty = tabulate stateRange (\_ -> emptyTokens)
  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)

-- The base case for when one character is lexed.
instance Measured (Table State Tokens,Size) Char where
  measure c =
    let bytes = encode c
        cSeq = singleton c
        baseCase in_state | in_state == -1 = InvalidTokens cSeq
                          | otherwise = case foldl automata in_state bytes of
          -1 -> InvalidTokens cSeq
          os -> case alex_accept ! os of
            []  -> Tokens empty (Str cSeq) os
            acc -> Tokens empty (One (createToken cSeq acc)) os
    in (tabulate stateRange $ baseCase, Sum 1)

createToken :: (Seq Char) -> Accepts -> IntToken
createToken lex acc = Token lex acc

createTokens :: Seq IntToken -> Suffix -> State -> Tokens
createTokens seq suf state = if null seq
                             then NoTokens
                             else Tokens seq suf state

invalidTokens :: (Seq Char) -> Tokens
invalidTokens s = InvalidTokens s

emptyTokens :: Tokens
emptyTokens = NoTokens

--------- Combination functions, the conquer step

-- Combines two transition maps
combineTokens :: Transition -> Transition -> Transition
combineTokens trans1 trans2 in_state | isInvalid toks1 = toks1
                                     | isEmpty toks1   = trans2 in_state
                                     | otherwise = combineWithRHS toks1 trans2
  where toks1 = trans1 in_state

-- Tries to merge tokens first, if it can't it either appends the token or calls
-- itself if the suffix contains Tokens instaed of a single token.
combineWithRHS :: Tokens -> Transition -> Tokens
combineWithRHS toks1 trans2 | isEmpty toks2 = toks1
                            | isValid toks2 =
    let toks2' = mergeTokens (lastToken toks1) toks2 trans2
    in appendTokens seq1 toks2'                           
                            | otherwise     = case lastToken toks1 of
    Multi suffToks ->
      let toks2' = combineWithRHS suffToks trans2 -- try to combine suffix with transition
      in appendTokens seq1 toks2'
    One tok -> appendTokens (seq1 |> tok) (trans2 startState)
    Str s -> invalidTokens s
  where toks2 = trans2 $ outState toks1
        seq1 = currentSeq toks1

-- Creates one token from the last token of the first sequence and and the first
-- token of the second sequence and inserts it between the init of the first
-- sequence and the tail of the second sequence
mergeTokens :: Suffix -> Tokens -> Transition -> Tokens
mergeTokens suff1 toks2 trans2 = case viewl (currentSeq toks2) of
  token2 :< seq2' -> let newToken = mergeToken suff1 token2
                     in toks2 {currentSeq = newToken <| seq2'}
  EmptyL -> case alex_accept ! out_state of
    [] -> toks2 {lastToken = mergeSuff suff1 (lastToken toks2) trans2}
    acc -> let lex = suffToStr suff1 <> suffToStr (lastToken toks2)
           in toks2 {lastToken = One $ createToken lex acc}
  where out_state = outState toks2

-- Creates on token from a suffix and a token
mergeToken :: Suffix -> IntToken -> IntToken
mergeToken suff1 token2 = token2 {lexeme = suffToStr suff1 <> lexeme token2}

-- Creates the apropiet new suffix from two suffixes
mergeSuff :: Suffix -> Suffix -> Transition -> Suffix
mergeSuff (Multi toks1) suff2 trans2 = Multi $
  let newToks = combineWithRHS toks1 trans2
  in if isValid $ newToks
     then newToks
     else toks1 {lastToken = mergeSuff (lastToken toks1) suff2 trans2}
mergeSuff (Str s1) suff2 _ = Str $ s1 <> suffToStr suff2
mergeSuff (One token1) (Str s) trans2 =
  let toks2 = trans2 startState
  in if isValid toks2
     then Multi $ toks2 {currentSeq = token1 <| currentSeq toks2}
     else Multi $ createTokens (singleton token1) (Str s) (-1)
mergeSuff suff1 (One token2) _ = One $ mergeToken suff1 token2
mergeSuff suff1 (Multi toks2) trans2 = Multi $ mergeTokens suff1 toks2 trans2

-- Prepends a sequence of tokens on the sequence in Tokens
appendTokens :: Seq IntToken -> Tokens -> Tokens
appendTokens seq1 toks2 | isValid toks2 =
  toks2 {currentSeq = seq1 <> currentSeq toks2}
                        | otherwise = toks2

---------- Constructors

makeTree :: String -> LexTree
makeTree  = fromList

measureToTokens :: (Table State Tokens,Size) -> Seq Token
measureToTokens m = case access (fst $ m) startState of
  InvalidTokens s -> error $ "Unacceptable token: " ++ toList s
  NoTokens -> empty
  Tokens seq suff out_state ->
    snd $ foldlWithIndex showToken (Pn 0 1 1,empty) $ intToks seq suff
  where showToken (pos,toks) _ (Token lex accs) =
          let pos' = foldl alexMove pos lex
          in case accs of
            [] -> (pos',toks)
            AlexAcc f:_ -> (pos',toks |> f pos lex)
            AlexAccSkip:_ -> (pos',toks)
        intToks seq (Str str) = error $ "Unacceptable token: " ++ toList str
        intToks seq (One token) = seq |> token
        intToks seq (Multi (Tokens seq' suff' _)) = intToks (seq <> seq') suff'

treeToTokens :: LexTree -> Seq Token
treeToTokens = measureToTokens . measure

------------- Util funs

isValid :: Tokens -> Bool
isValid (Tokens _ _ _) = True
isValid _ = False

isEmpty :: Tokens -> Bool
isEmpty NoTokens = True
isEmpty _        = False

isInvalid :: Tokens -> Bool
isInvalid (InvalidTokens _) = True
isInvalid _ = False

suffToStr :: Suffix -> Seq Char
suffToStr (Str s) = s
suffToStr (One token) = lexeme token
suffToStr (Multi toks) =
  concatLexemes (currentSeq toks) <> suffToStr (lastToken toks)

isAccepting :: Tokens -> Bool
isAccepting (Tokens _ suff _) = case suff of
  Str _ -> False
  One _ -> True
  Multi toks -> isAccepting toks
isAccepting NoTokens = True
isAccepting (InvalidTokens _) = False

concatLexemes :: Seq IntToken -> Seq Char
concatLexemes = foldr ((<>) . lexeme) mempty

insertAtIndex :: String -> Int -> LexTree -> LexTree
insertAtIndex str i tree = 
  if i < 0
  then error "index must be >= 0"
  else l <> (makeTree str) <> r
     where (l,r) = splitTreeAt i tree

splitTreeAt :: Int -> LexTree -> (LexTree,LexTree)
splitTreeAt i tree = split (\(_,s) -> getSum s>i) tree

size :: LexTree -> Int
size tree = getSum . snd $ measure tree

-- Starting state
startState = 0
-- A tuple that says how many states there are
stateRange = let (start,end) = bounds alex_accept
             in (start-1,end)

-- Takes an in state and a byte and returns the corresponding out state using
-- the DFA generated by Alex
automata :: Int -> Word8 -> Int
automata (-1) _ = -1
automata s c = let base   = alex_base ! s
                   ord_c  = fromEnum c
                   offset = base + ord_c
                   check  =  alex_check ! offset
               in if (offset >= (0)) && (check == ord_c)
                  then alex_table ! offset
                  else alex_deflt ! s