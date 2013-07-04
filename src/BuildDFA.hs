{-# LANGUAGE CPP, FlexibleInstances #-}
module BuildDFA where

import AbsSyn
import CharSet
import DFA
import DFAMin
import NFA
import Map ( Map )
import qualified Map as M hiding ( Map )
import ParseMonad ( runP )
import Parser
import Scan
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM hiding (IntMap)
import Data.Monoid

#if __GLASGOW_HASKELL__ < 610
import Control.Exception as Exception ( block, unblock, catch, throw )
#endif
#if __GLASGOW_HASKELL__ >= 610
import Control.Exception ( bracketOnError )
#endif
import Control.Monad ( when, liftM )
import Data.Char ( chr )
import Data.List ( isSuffixOf )
import Data.Maybe ( isJust, fromJust )
import Data.Version ( showVersion )
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )
import System.Directory ( removeFile )
import System.Environment ( getProgName, getArgs )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, Handle, IOMode(..), openFile, hClose, hPutStr, hPutStrLn )
#if __GLASGOW_HASKELL__ >= 612
import System.IO ( hGetContents, hSetEncoding, utf8 )
#endif

-- We need to force every file we open to be read in
-- as UTF8
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
alexOpenFile :: FilePath -> IOMode -> IO Handle
#if __GLASGOW_HASKELL__ >= 612
alexOpenFile file mode = do
  h <- openFile file mode
  hSetEncoding h utf8
  return h
#else
alexOpenFile = openFile
#endif

parseScript :: FilePath -> String
  -> IO (Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code))
parseScript file prg =
  case runP prg initialParserEnv parse of
    Left (Just (AlexPn _ line col),err) -> 
	    die (file ++ ":" ++ show line ++ ":" ++ show col
             ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
        die (file ++ ": " ++ err ++ "\n")
    Right script -> return script

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = M.fromList [("white", charSet " \t\n\v\f\r"),
		           ("printable", charSetRange (chr 32) (chr 0x10FFFF)), -- FIXME: Look it up the unicode standard
		           (".", charSetComplement emptyCharSet 
			    `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = M.empty

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

build :: FilePath -> IO(DFA SNum Code)
build file = do
    basename <- case (reverse file) of
                    'x':'.':r   -> return (reverse r)
                    _           -> die "File must end with suffix '.x'"
    prg <- alexReadFile file
    script <- parseScript file prg
    return (makeDFA file basename script)

makeDFA :: FilePath -> FilePath -> 
    (Maybe (AlexPosn, Code), [Directive], Scanner, Maybe (AlexPosn, Code)) ->
    DFA SNum Code
makeDFA file basename script = do
    let (maybe_header, directives, scanner1, maybe_footer) = script
        (scanner2, scs, sc_hdr) = encodeStartCodes scanner1
        (scanner_final, actions) = extractActions scanner2     
    let dfa = scanner2dfa UTF8 scanner_final scs
        min_dfa = minimizeDFA dfa
        nm = scannerName scanner_final
        usespreds = usesPreds min_dfa
    min_dfa
    
main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute [] args of
      ([],[file],[]) -> do dfa <- build file
      -- DO FUNNY STUF
                           return ()
      _ -> die "ILLEGAL SYNTAX GO TO JAIL"

ghciTest :: FilePath -> IO (DFA SNum Code)
ghciTest file = do dfa <- build file
                   return dfa

-- Move out of main before anything funny happens
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
