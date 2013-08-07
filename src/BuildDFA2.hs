{-# LANGUAGE CPP, FlexibleInstances #-}
module BuildDFA2 (build, makeDFA, alexReadFile
                ) where

import Alex.AbsSyn
import Alex.CharSet
import Alex.DFA
import Alex.DFAMin
import Alex.NFA
import Alex.ParseMonad ( runP, AlexPosn(..))
import Alex.Parser
import AbsSyn2

import Data.Map ( Map )
import qualified Data.Map as M hiding ( Map )
import Data.Char ( chr )
import Data.Array as A
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM hiding (IntMap)
import Data.Monoid (mappend)
import Data.Foldable (toList)
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
--build :: FilePath -> IO (DFA' SNum Code)
build file = do
    basename <- case (reverse file) of
                    'x':'.':r   -> return (reverse r)
                    _           -> error "File must end with suffix '.x'"
    prg <- alexReadFile file
    let dfa = makeDFA $ parseScanner file prg
        dfa' = dfaToDFA' dfa
    return dfa'

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

--dfaToDFA' :: Ix s => DFA s a -> DFA' s a
dfaToDFA' (DFA scs states) = DFA' scs states' accepting
  where states' = array (0,255) $ IM.toList $ M.foldlWithKey convert IM.empty states
        convert pStates is (State _ cToOs) = IM.foldlWithKey (insertStuff is) pStates cToOs
        insertStuff is pStates byte os = IM.insertWith mappend byte (creatEdge is os) pStates
        accepting = array' . M.toList $ M.map state_acc states
        creatEdge is os = M.singleton is os

array' :: (Enum i, Ix i) => [(i,b)] -> Array i b
array' list = array (toEnum 0,toEnum $ length list - 1) list