-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

#ifdef ALEX_GHC
#undef __GLASGOW_HASKELL__
#define ALEX_IF_GHC_GT_500 #if __GLASGOW_HASKELL__ > 500
#define ALEX_IF_GHC_LT_503 #if __GLASGOW_HASKELL__ < 503
#define ALEX_IF_GHC_GT_706 #if __GLASGOW_HASKELL__ > 706
#define ALEX_ELIF_GHC_500 #elif __GLASGOW_HASKELL__ == 500
#define ALEX_IF_BIGENDIAN #ifdef WORDS_BIGENDIAN
#define ALEX_ELSE #else
#define ALEX_ENDIF #endif
#define ALEX_DEFINE #define
#endif

#ifdef ALEX_GHC
#define ILIT(n) n#
#define IBOX(n) (I# (n))
#define FAST_INT Int#
ALEX_IF_GHC_GT_706
ALEX_DEFINE GTE(n,m) (tagToEnum# (n >=# m))
ALEX_DEFINE EQ(n,m) (tagToEnum# (n ==# m))
ALEX_ELSE
ALEX_DEFINE GTE(n,m) (n >=# m)
ALEX_DEFINE EQ(n,m) (n ==# m)
ALEX_ENDIF
#define PLUS(n,m) (n +# m)
#define MINUS(n,m) (n -# m)
#define TIMES(n,m) (n *# m)
#define NEGATE(n) (negateInt# (n))
#define IF_GHC(x) (x)
#else
#define ILIT(n) (n)
#define IBOX(n) (n)
#define FAST_INT Int
#define GTE(n,m) (n >= m)
#define EQ(n,m) (n == m)
#define PLUS(n,m) (n + m)
#define MINUS(n,m) (n - m)
#define TIMES(n,m) (n * m)
#define NEGATE(n) (negate (n))
#define IF_GHC(x)
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#

ALEX_IF_GHC_LT_503
uncheckedShiftL# = shiftL#
ALEX_ENDIF

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off =
ALEX_IF_BIGENDIAN
  narrow16Int# i
  where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
ALEX_ELSE
  indexInt16OffAddr# arr off
ALEX_ENDIF
#else
alexIndexInt16OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC
{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off = 
ALEX_IF_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
		     (b2 `uncheckedShiftL#` 16#) `or#`
		     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
ALEX_ELSE
  indexInt32OffAddr# arr off
ALEX_ENDIF
#else
alexIndexInt32OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC
ALEX_IF_GHC_LT_503
quickIndex arr i = arr ! i
ALEX_ELSE
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
ALEX_ENDIF
#else
quickIndex arr i = arr ! i
#endif

-- -----------------------------------------------------------------------------
-- Some structures, move to wrapper?

type Transition      = Int -> Tokens
data Suffix          = Str !String
                     | One !Token
                     | Multi !Tokens
type Size            = Sum Int

-- -----------------------------------------------------------------------------
-- Main lexing routines

-- Push a byte through the DFA and returns the outstate
-- automata :: Int -> Word8 -> Int
automata (-1) _ = -1
automata s c = let base   = alexIndexInt32OffAddr alex_base s
                   IBOX(ord_c) = fromIntegral c
                   offset = PLUS(base,ord_c)
                   check  = alexIndexInt16OffAddr alex_check offset
               in if GTE(offset,ILIT(0)) && EQ(check,ord_c)
                  then alexIndexInt16OffAddr alex_table offset
                  else alexIndexInt16OffAddr alex_deflt s

-- Combines two transition maps
--combineTokens :: Transition -> Transition -> Transition
combineTokens trans1 trans2 in_state | isInvalid toks1 = toks1
                                     | isEmpty toks1   = trans2 in_state
                                     | otherwise = combineWithRHS toks1 trans2
  where toks1 = trans1 in_state

-- Tries to merge tokens first, if it can't it either appends the token or calls
-- itself if the suffix contains Tokens instaed of a single token.
--combineWithRHS :: Tokens -> Transition -> Tokens
combineWithRHS toks1 trans2 | isEmpty toks2 = toks1
                            | isValid toks2 =
    let toks2' = mergeTokens (lastToken toks1) toks2 trans2
    in appendTokens seq1 toks2'                           
                            | otherwise     = case lastToken toks1 of
    Multi suffToks ->
      let toks2' = combineWithRHS suffToks trans2 -- try to merge suffix
      in appendTokens seq1 toks2'
    One tok -> appendTokens (seq1 |> tok) (trans2 startState)
    Str s -> invalidTokens s
  where toks2 = trans2 $ outState toks1
        seq1 = currentSeq toks1

-- Creates one token from the last token of the first sequence and and the first
-- token of the second sequence and inserts it between the init of the first
-- sequence and the tail of the second sequence
--mergeTokens :: Suffix -> Tokens -> Transition -> Tokens
mergeTokens suff1 toks2 trans2 = case viewl (currentSeq toks2) of
  token2 :< seq2' -> let newToken = mergeToken suff1 token2
                     in toks2 {currentSeq = newToken <| seq2'}
  EmptyL -> case checkAccs (alex_accept `quickIndex` IBOX(out_state)) of
    AlexAccNone -> toks2 {lastToken = mergeSuff suff1 (lastToken toks2) trans2}
    acc -> let lex = suffToStr suff1 ++ suffToStr (lastToken toks2)
           in toks2 {lastToken = One $ createToken lex acc}
  where out_state = outState toks2

-- Creates on token from a suffix and a token
--mergeToken :: Suffix -> IntToken -> IntToken
mergeToken suff1 token2 = token2 {lexeme = suffToStr suff1 ++ lexeme token2}

-- Creates the apropiet new suffix from two suffixes
--mergeSuff :: Suffix -> Suffix -> Transition -> Suffix
mergeSuff (Multi toks1) suff2 trans2 = Multi $
  let newToks = combineWithRHS toks1 trans2
  in if isValid $ newToks
     then newToks
     else toks1 {lastToken = mergeSuff (lastToken toks1) suff2 trans2}
mergeSuff (Str s1) suff2 _ = Str $ s1 ++ suffToStr suff2
mergeSuff (One token1) (Str s) trans2 =
  let toks2 = trans2 startState
  in if isValid toks2
     then Multi $ toks2 {currentSeq = token1 <| currentSeq toks2}
     else Multi $ createTokens (singleton token1) (Str s) NEGATE(1)
mergeSuff suff1 (One token2) _ = One $ mergeToken suff1 token2
mergeSuff suff1 (Multi toks2) trans2 = Multi $ mergeTokens suff1 toks2 trans2

-- Prepends a sequence of tokens on the sequence in Tokens
--appendTokens :: Seq IntToken -> Tokens -> Tokens
appendTokens seq1 toks2 | isValid toks2 =
  toks2 {currentSeq = seq1 <> currentSeq toks2}
                        | otherwise = toks2

suffToStr :: Suffix -> String
suffToStr (Str s) = s
suffToStr (One token) = lexeme token
suffToStr (Multi toks) =
  concatLexemes (currentSeq toks) ++ suffToStr (lastToken toks)

concatLexemes :: Seq IntToken -> String
concatLexemes = foldr ((++) . lexeme) ""

splitTreeAt :: Int -> LexTree -> (LexTree,LexTree)
splitTreeAt i tree = split (\(_,s) -> getSum s>i) tree

size :: LexTree -> Int
size tree = getSum . snd $ measure tree

--checkAccs :: AlexAcc -> AlexAcc
#ifndef ALEX_NOPRED
checkAccs (AlexAccPred a predx rest) | predx user orig_input IBOX(len) input =
  AlexLastAcc a input IBOX(len)
                                     | otherwise = check_accs rest
checkAccs (AlexAccSkipPred predx rest) | predx user orig_input IBOX(len) input =
  AlexLastSkip input IBOX(len)
                                       | otherwise = check_accs rest
#endif
checkAccs accs = accs

data AlexAcc a user
  = AlexAccNone
  | AlexAcc a
  | AlexAccSkip
#ifndef ALEX_NOPRED
  | AlexAccPred a   (AlexAccPred user) (AlexAcc a user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc a user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext IBOX(sc) user _ _ input = 
     case alex_scan_tkn user input ILIT(0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.
#endif

-- used by wrappers
iUnbox IBOX(i) = i
