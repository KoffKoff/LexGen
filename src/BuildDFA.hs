{-# LANGUAGE CPP, FlexibleInstances #-}
module BuildDFA (build, makeDFA) where

import Alex.AbsSyn
import Alex.CharSet
import Alex.DFA
import Alex.DFAMin
import Alex.NFA
import Alex.ParseMonad ( runP, AlexPosn(..))
import Alex.Parser
import Alex.Map ( Map )
import qualified Alex.Map as M hiding ( Map )

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

-- Reads a file and callse parseScanner followed by makeDFA on the content
-- MOVE TO WRAPPER
build :: FilePath -> IO (DFA' SNum)
build file = do
    basename <- case (reverse file) of
                    'x':'.':r   -> return (reverse r)
                    _           -> error "File must end with suffix '.x'"
    prg <- alexReadFile file
    return . dfaToDFA' . makeDFA $ parseScanner file prg

-- Gets the scanner from the code in the alex file
parseScanner :: FilePath -> String -> Scanner
parseScanner file prg =
  case runP prg initialParserEnv parse of
    Left (Just (AlexPn _ line col),err) -> 
      error (file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
      error (file ++ ": " ++ err ++ "\n")
    Right (maybe_header, directives, scanner, maybe_footer) -> scanner
-- At the moment we are only interested in the scanner, the directives may be intersting later

-- Does some work on the scanner and turns it into an minimized DFA
makeDFA :: Scanner -> DFA SNum Code
makeDFA scanner1 = 
    let (scanner2, scs, _) = encodeStartCodes scanner1
    in minimizeDFA $ scanner2dfa UTF8 (fst $ extractActions scanner2) scs

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = M.fromList [("white", charSet " \t\n\v\f\r")
		        ,("printable", charSetRange (chr 32) (chr 0x10FFFF)) -- FIXME: Look it up the unicode standard
                        ,(".", charSetComplement emptyCharSet
                               `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = M.empty
{-
-- Converts an alex DFA into DFA' (The outer map is indexed by Accept 'a')
dfaToDFA' :: (Ord a,Ord s) => DFA s a -> DFA' s a
dfaToDFA' (DFA ss dfass) = DFA' ss dfass'
  where dfass' = M.foldlWithKey convert M.empty dfass
        convert dfa' is (State as os) = foldl (\d a -> M.insertWith mappend a
          (M.singleton is (acceptLookup a os)) d) dfa' as

acceptLookup :: Accept a -> IntMap s -> s
acceptLookup (Acc i _ _ _) ss = case IM.lookup i ss of
  Just s -> s
  Nothing -> error "Incomplete lexer WTF MATE?"
-}

dfaToDFA' :: Ord s => DFA s a -> DFA' s
dfaToDFA' (DFA scs states) = DFA' scs states'
  where states' = M.foldlWithKey convert IM.empty states
        convert pStates is (State _ cToOs) = IM.foldlWithKey (insertStuff is) pStates cToOs
        insertStuff is pStates byte os = IM.insertWith mappend byte (M.singleton is os) pStates
