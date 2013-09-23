-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

import Prelude hiding (foldl,foldr,null)
import Data.Word (Word8)
import Data.FingerTree (FingerTree,Measured,measure,split,fromList)
import Data.Sequence hiding (fromList)
import Data.Foldable (foldl,foldr)
import Data.Monoid
#if defined(ALEX_BASIC_BYTESTRING) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING)

import qualified Data.Char
import qualified Data.ByteString.Lazy     as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)

#elif defined(ALEX_STRICT_BYTESTRING)

import qualified Data.Char
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Unsafe   as ByteString

#else

import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

#endif

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The base case

data Tokens    = NoTokens
#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_GSCAN)
               | InvalidTokens !String
#else
               | InvalidTokens !ByteStrin.ByteString
#endif
               | Tokens { currentSeq :: !(Seq IntToken)
                        , lastToken  :: !Suffix
                        , outState   :: !Int}

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_GSCAN)
type LexTree   = FingerTree (Table Int Tokens,Size) Char
data IntToken a user = Token { lexeme   :: !String
                             , token_id :: AlexAcc a user}

createToken :: String -> AlexAcc a user -> IntToken
createToken lex acc = Token lex acc

makeTree :: String -> LexTree
makeTree = fromList

-- The base case for when one character is lexed.
instance Measured (Table State Tokens,Size) Char where
#else
type LexTree   = FingerTree (Table Int Tokens,Size) Byte
data IntToken a user = Token { lexeme   :: !ByteString.ByteString
                             , token_id :: AlexAcc a user}

createToken :: ByteString.ByteString -> AlexAcc a user -> IntToken
createToken lex acc = Token lex acc

makeTree :: ByteString.ByteString -> LexTree
makeTree = fromList . unpackBytes

-- The base case for when one character is lexed.
instance Measured (Table State Tokens,Size) Byte where
#endif
  measure c =
#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_GSCAN)
    let bytes = utf8Encode c
        invalid = InvalidTokens [c]
#else
    let bytes = [c]
        invalid = InvalidTokens $ singleton c
#endif
        baseCase in_state | in_state == -1 = invalid
                          | otherwise = case foldl automata in_state bytes of
          -1 -> invalid
          os -> case alex_accept `quickIndex` os of
            AlexAccNone -> Tokens empty (Str [c]) os
            acc -> Tokens empty (One (createToken [c] acc)) os
    in (tabulate stateRange $ baseCase, Sum 1)

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_GSCAN)
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
#endif

-- -----------------------------------------------------------------------------
-- Default monad

#ifdef ALEX_MONAD
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
#ifdef ALEX_MONAD_USER_STATE
      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
#endif
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,       
                        alex_chr = '\n',
                        alex_bytes = [],
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} -> 
        Right (s, (pos,c,bs,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

#ifdef ALEX_MONAD_USER_STATE
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())
#endif

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)
#endif /* ALEX_MONAD */


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

#ifdef ALEX_MONAD_BYTESTRING
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: ByteString.ByteString,      -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int        -- the current startcode
#ifdef ALEX_MONAD_USER_STATE
      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
#endif
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: ByteString.ByteString -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,       
                        alex_chr = '\n',
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
        Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)
#endif /* ALEX_MONAD_BYTESTRING */


-- -----------------------------------------------------------------------------
-- Basic wrapper

#ifdef ALEX_BASIC
createTokens :: Seq IntToken -> Suffix -> State -> Tokens
createTokens seq suf state = if null seq
                             then NoTokens
                             else Tokens seq suf state

-- Wrapper template
invalidTokens :: String -> Tokens
invalidTokens s = InvalidTokens s

-- Wrapper template
emptyTokens :: Tokens
emptyTokens = NoTokens

-- Wrapper template
isValid :: Tokens -> Bool
isValid (Tokens _ _ _) = True
isValid _ = False

-- Wrapper template
isEmpty :: Tokens -> Bool
isEmpty NoTokens = True
isEmpty _        = False

-- Wrapper template
isInvalid :: Tokens -> Bool
isInvalid (InvalidTokens _) = True
isInvalid _ = False

-- Wrapper template
measureToTokens :: (Table State Tokens,Size) -> Seq Token
measureToTokens m = case access (fst $ m) startState of
  InvalidTokens s -> error $ "Unacceptable token: " ++ s
  NoTokens -> empty
  Tokens seq suff out_state ->
    snd $ foldlWithIndex showToken $ intToks seq suff
  where showToken toks _ (Token lex accs) =
          in case accs of
            [] -> toks
            AlexAcc f:_ -> toks |> f pos lex
            AlexAccSkip:_ -> toks
        intToks seq (Str str) = error $ "Unacceptable token: " ++ str
        intToks seq (One token) = seq |> token
        intToks seq (Multi (Tokens seq' suff' _)) = intToks (seq <> seq') suff'

-- Generic template
treeToTokens :: LexTree -> Seq Token
treeToTokens = measureToTokens . measure
#endif


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

#ifdef ALEX_BASIC_BYTESTRING

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',str)
  where go inp@(_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (ByteString.take (fromIntegral len) str) : go inp'


#endif

#ifdef ALEX_STRICT_BYTESTRING

-- alexScanTokens :: String -> [token]
alexScanTokens str = go (AlexInput '\n' str)
  where go inp@(AlexInput _ str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (ByteString.unsafeTake len str) : go inp'

#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

#ifdef ALEX_POSN
-- Wrapper template
invalidTokens :: String -> Tokens
invalidTokens s = InvalidTokens s

-- Wrapper template
emptyTokens :: Tokens
emptyTokens = NoTokens

-- Wrapper template
isValid :: Tokens -> Bool
isValid (Tokens _ _ _) = True
isValid _ = False

-- Wrapper template
isEmpty :: Tokens -> Bool
isEmpty NoTokens = True
isEmpty _        = False

-- Wrapper template
isInvalid :: Tokens -> Bool
isInvalid (InvalidTokens _) = True
isInvalid _ = False


-- Wrapper template
measureToTokens :: (Table State Tokens,Size) -> Seq Token
measureToTokens m = case access (fst $ m) startState of
  InvalidTokens s -> error $ "Unacceptable token: " ++ s
  NoTokens -> empty
  Tokens seq suff out_state ->
    snd $ foldlWithIndex showToken (Pn 0 1 1,empty) $ intToks seq suff
  where showToken (pos,toks) _ (Token lex accs) =
          let pos' = foldl alexMove pos lex
          in case accs of
            [] -> (pos',toks)
            AlexAcc f:_ -> (pos',toks |> f pos lex)
            AlexAccSkip:_ -> (pos',toks)
        intToks seq (Str str) = error $ "Unacceptable token: " ++ str
        intToks seq (One token) = seq |> token
        intToks seq (Multi (Tokens seq' suff' _)) = intToks (seq <> seq') suff'

-- Generic template
treeToTokens :: LexTree -> Seq Token
treeToTokens = measureToTokens . measure
#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

#ifdef ALEX_POSN_BYTESTRING
--alexScanTokens :: ByteString -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (ByteString.take (fromIntegral len) str) : go inp'
#endif


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

#ifdef ALEX_GSCAN
alexGScan stop state inp = alex_gscan stop alexStartPos '\n' [] inp (0,state)

alex_gscan stop p c bs inp (sc,state) =
  case alexScan (p,c,bs,inp) sc of
        AlexEOF     -> stop p c inp (sc,state)
        AlexError _ -> stop p c inp (sc,state)
        AlexSkip (p',c',bs',inp') len -> alex_gscan stop p' c' bs' inp' (sc,state)
        AlexToken (p',c',bs',inp') len k ->
             k p c inp len (\scs -> alex_gscan stop p' c' bs' inp' scs)
                (sc,state)
#endif
