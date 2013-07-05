{-# LANGUAGE CPP, FlexibleInstances #-}
module BuildDFA where

import Alex.AbsSyn
import Alex.CharSet
import Alex.DFA
import Alex.DFAMin
import Alex.NFA
import Alex.Map ( Map )
import qualified Alex.Map as M hiding ( Map )
import Alex.ParseMonad ( runP )
import Alex.Parser
import Alex.Scan

import Data.Char ( chr )
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM hiding (IntMap)
import Data.Monoid (mappend)
import System.IO ( stderr, Handle, IOMode(..), openFile, hClose, hPutStr, hPutStrLn )
#if __GLASGOW_HASKELL__ >= 612
import System.IO ( hGetContents, hSetEncoding, utf8 )
#endif

-- We need to force every file we open to be read in
-- as UTF8
-- MOVE TO WRAPPER
alexReadFile :: FilePath -> IO String
#if __GLASGOW_HASKELL__ >= 612
alexReadFile file = do
  h <- alexOpenFile file ReadMode
  hGetContents h
#else
alexReadFile = readFile
#endif

-- We need to force every file we write to be written
-- to as UTF8
-- MOVE TO WRAPPER
alexOpenFile :: FilePath -> IOMode -> IO Handle
#if __GLASGOW_HASKELL__ >= 612
alexOpenFile file mode = do
  h <- openFile file mode
  hSetEncoding h utf8
  return h
#else
alexOpenFile = openFile
#endif

parseScanner :: FilePath -> String -> Scanner
parseScanner file prg =
  case runP prg initialParserEnv parse of
    Left (Just (AlexPn _ line col),err) -> 
      error (file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
      error (file ++ ": " ++ err ++ "\n")
    Right (maybe_header, directives, scanner, maybe_footer) -> scanner
-- At the moment we are only interested in the scanner, the directives may be intersting later

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = M.fromList [("white", charSet " \t\n\v\f\r")
		        ,("printable", charSetRange (chr 32) (chr 0x10FFFF)) -- FIXME: Look it up the unicode standard
                        ,(".", charSetComplement emptyCharSet
                               `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = M.empty

-- MOVE TO WRAPPER
build :: FilePath -> IO (DFA SNum Code)
build file = do
    basename <- case (reverse file) of
                    'x':'.':r   -> return (reverse r)
                    _           -> error "File must end with suffix '.x'"
    prg <- alexReadFile file
    return (makeDFA $ parseScanner file prg)

makeDFA :: Scanner -> DFA SNum Code
makeDFA scanner1 = 
    let (scanner2, scs, _) = encodeStartCodes scanner1
    in minimizeDFA $ scanner2dfa UTF8 (fst $ extractActions scanner2) scs

dfaToDFA' :: (Ord a, Ord s) => DFA s a -> DFA' s a
dfaToDFA' (DFA ss dfass) = DFA' ss dfass'
  where dfass' = M.foldlWithKey convert M.empty dfass
        convert dfa' is (State as os) = foldl (\d a -> M.insertWith mappend a
          (M.singleton is (acceptLookup a os)) d) dfa' as

acceptLookup :: Accept a -> IntMap s -> s
acceptLookup (Acc i _ _ _) ss = case IM.lookup i ss of
  Just s -> s
  Nothing -> error "Incomplete lexer WTF MATE?"

data DFA' s a = DFA'
  { dfa_start_states :: [s]
  , dfa_states       :: Map (Accept a) (Edges s) }

type Edges s = Map s s

data Accept' a = Acc' {
  	  accAction     :: Maybe a,
	  accLeftCtx    :: Maybe CharSet, -- cannot be converted to byteset at this point.
	  accRightCtx   :: RightContext SNum
    }
    deriving (Eq,Ord)
-- End move

-- For testing purposes
instance Show (DFA SNum Code) where
  show (DFA sc cs) = "Start codes: " ++ show sc ++ "\nAutomata: " ++ show cs

instance Show (State SNum Code) where
  show (State _ o) = show o
