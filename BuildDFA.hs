
module BuildDFA where

import AbsSyn
import CharSet
import DFA
import DFAMin
import NFA
import Map ( Map )
import qualified Map hiding ( Map )
import ParseMonad ( runP )
import Scan

import Control.Exception as Exception ( block, unblock, catch, throw )
import Control.Exception ( bracketOnError )
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
import System.IO ( hGetContents, hSetEncoding, utf8 )

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
initSetEnv = Map.fromList [("white", charSet " \t\n\v\f\r"),
		           ("printable", charSetRange (chr 32) (chr 0x10FFFF)), -- FIXME: Look it up the unicode standard
		           (".", charSetComplement emptyCharSet 
			    `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = Map.empty

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

dieAlex :: String -> IO a
dieAlex s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

build :: FilePath -> DFA SNum Code
build file = do
    basename <- case (reverse file) of
                    'x':'.':r   -> return (reverse r)
                    _           -> die (file ++ ": filename must end in \'.x\' \n")
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
    let dfa = scanner2dfa encoding scanner_final scs
        min_dfa = minimizeDFA dfa
        nm = scannerName scanner_final
        usespreds = usesPreds min_dfa
    return min_dfa
