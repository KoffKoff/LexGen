module HsBuilder where

import System.IO
import Alex.AbsSyn
import BuildDFA
import qualified Data.Map as M
import qualified Data.IntMap as IM

constructLexer :: FilePath -> String -> IO ()
constructLexer alex_file out_file = do
  dfa <- build alex_file
  let prg = langExts ++ "module " ++ out_file ++ " where\n\n" ++
            imports ++ types ++ instances ++ mainFuns ++ combinatorFuns ++ utilFuns ++ dfaOut dfa
  out_h <- alexOpenFile (out_file ++ ".hs") WriteMode
  hPutStr out_h prg

alexOpenFile :: FilePath -> IOMode -> IO Handle
alexOpenFile file mode = do
  h <- openFile file mode
  hSetEncoding h utf8
  return h

imports :: String
imports = "import System.IO\n" ++
          "import Data.Monoid\n" ++
          "import Data.Word\n" ++
          "import Data.Maybe\n" ++
          "import Data.Bits\n" ++
          "import Data.FingerTree (FingerTree)\n" ++
          "import qualified Data.FingerTree as F\n" ++
          "import Data.Map (Map)\n" ++
          "import qualified Data.Map as Map\n" ++
          "import Data.IntMap (IntMap)\n" ++
          "import qualified Data.IntMap as IM\n" ++
          "import Data.Sequence\n" ++
          "import Alex.AbsSyn\n\n"

langExts :: String
langExts = "{-#LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}\n"

types :: String
types = "--type SNum = Int\n" ++
        "--type Code = String\n" ++
        "type Byte = Word8\n" ++
        "type Transition = " ++ "Map SNum (SNum,[Accept Code])\n" ++
        "type Automata = " ++ "IntMap Transition\n" ++
        "data Token = T { transitions :: Transition\n" ++
        "               , lexeme      :: String\n" ++
        "               , sub_tokens  :: [Token]\n" ++
        "               , token_id    :: Maybe SNum}\n" ++
        "data Tokens = Toks (Seq Token)\n" ++
        "type LexTree = FingerTree Tokens Byte\n\n" ++
        "{-data Accept a\n" ++
        "  = Acc { accPrio       :: Int,\n" ++
        "          accAction     :: Maybe a,\n" ++
        "          accLeftCtx    :: Maybe CharSet," ++
        " -- cannot be converted to byteset at this point.\n" ++
        "          accRightCtx   :: RightContext SNum\n" ++
        "    }\n" ++
        "    deriving (Eq,Ord)-}\n\n"

instances :: String
instances = "instance Monoid Tokens where\n" ++
            "  mempty = Toks mempty\n" ++
            "  (Toks toks1) `mappend` (Toks toks2) = Toks $ combineTokens toks1 toks2\n\n" ++
            "instance F.Measured Tokens Byte where\n" ++
            "  measure byte = let t = automata IM.! fromEnum byte\n" ++
            "                 in Toks $ singleton (T t [toEnum" ++
            "(fromEnum byte)] [] (getTokenId t))\n\n" ++
            "instance Show Token where\n" ++
            "  show token = lexeme token ++ \":\" ++ if isJust id " ++
            "then show (fromJust id) else show id\n" ++
            "    where id = token_id token\n\n" ++
            "instance Show Tokens where\n" ++
            "  show (Toks toks) = foldlWithIndex (\\str _ tok -> str ++" ++
            "\"\\n\" ++ show tok) \"\" toks\n"


dfaOut :: DFA' SNum Code -> String
dfaOut dfa = start ++ newFun "start_state" [] "SNum" ++ show start_state ++ "\n" ++
             newFun "automata" [] "Automata" ++ automata ++ "\n"
  where start_state:_ = dfa'_start_states dfa
        automata = fixAutomata $ show $ dfa'_states dfa
        start = createBlockComment "The automata"

fixAutomata :: String -> String
fixAutomata ('f':'r':'o':'m':'L':'i':'s':'t':rest) = "IM.fromList" ++ replace
                                                     "fromList" "Map.fromList" rest

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace ls repl strs | ls == take (length ls) strs = repl ++ replace ls repl
                                                     (drop (length ls) strs)
                     | otherwise = head strs : (replace ls repl $ tail strs)

combinatorFuns :: String
combinatorFuns = start ++ combineTokens ++ tokenAppender ++ divideAppender ++ mergeToken
  where start = createBlockComment "The Combinators for the tokens"
        combineTokens =
          newFun "combineTokens" [("toks1","Seq Token"),("toks2","Seq Token")] "Seq Token" ++
          "case viewr toks1 of\n" ++
          "  EmptyR         -> toks2\n" ++
          "  toks1' :> tok1 -> let toks2' = tokenAppender tok1 toks2\n" ++
          "                    in toks1' >< toks2'\n"
        tokenAppender =
          newFun "tokenAppender" [("tok1","Token"),("toks2","Seq Token")] "Seq Token" ++
          "case viewl toks2 of\n" ++
          "  EmptyL         -> singleton tok1\n" ++
          "  tok2 :< toks2' -> let e = startTransition $ tabulate (transitions tok1) (transitions tok2)\n" ++
          "                    in if Map.null e\n" ++
          "                       then divideAppender tok1 tok2 >< toks2'\n" ++
          "                       else tokenAppender (mergeToken tok1 tok2) toks2'\n"
        divideAppender =
          newFun "divideAppender" [("tok1","Token"),("tok2","Token")] "Seq Token" ++
          "case sub_tokens tok2 of\n" ++
          "  []                -> case token_id tok1 of\n" ++
          "    Just _  -> fromList [tok1,tok2]\n" ++
          "    Nothing -> case sub_tokens tok1 of\n" ++
          "      [] -> fromList [tok1,tok2]\n" ++
          "      [subtok1,subtok2] -> divideAppender subtok1 subtok2 |> tok2\n" ++
          "  [subtok1,subtok2] ->\n" ++
          "    let e = startTransition $ tabulate (transitions tok1) (transitions subtok1)\n" ++
          "    in if Map.null e\n" ++
          "       then case sub_tokens subtok1 of\n" ++
          "         [] -> fromList [tok1,tok2]\n" ++
          "         mt -> combineTokens (divideAppender tok1 subtok1) (singleton subtok2)\n" ++
          "       else divideAppender (mergeToken tok1 subtok1) subtok2\n"
        mergeToken =
          newFun "mergeToken" [("tok1","Token"),("tok2","Token")] "Token" ++ "\n" ++
          "  let e = tabulate (transitions tok1) (transitions tok2)\n" ++
          "  in T e (lexeme tok1 `mappend` lexeme tok2) [tok1,tok2] (getTokenId e)\n"

utilFuns :: String
utilFuns = start ++ tabulate ++ getTokenId ++ startTransition ++ lexCode ++ encode
  where start = createBlockComment "Utility functions"
        getTokenId =
          newFun "getTokenId" [("trans","Transition")] "Maybe SNum" ++
          "case Map.lookup start_state trans of\n" ++
          "  Just (id,a:as) -> Just id\n" ++
          "  _              -> Nothing\n"
        startTransition =
          newFun "startTransition" [("trans","Transition")] "Transition" ++
          "case Map.lookup start_state trans of\n" ++
          "  Just osa -> Map.singleton start_state osa\n" ++
          "  _        -> Map.empty\n"
        tabulate =
          newFun "tabulate" [("trans1","Transition"),("trans2","Transition")] "Transition" ++
          "Map.foldlWithKey findTransition Map.empty trans1\n" ++
          "  where findTransition currentTrans inState (outState,_) =\n" ++
          "          case Map.lookup outState trans2 of\n" ++
          "            Just os -> Map.insert inState os currentTrans\n" ++
          "            _       -> currentTrans\n"
        lexCode =
          newFun "lexCode" [("","LexTree")] "Tokens" ++ "F.measure\n"
        encode =
          newFun "encode" [("","Char")] "[Word8]" ++
          "map fromIntegral . go . fromEnum\n" ++
          " where\n" ++
          "  go oc\n" ++
          "   | oc <= 0x7f       = [oc]\n" ++
          "   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)\n" ++
          "                        , 0x80 + oc .&. 0x3f\n" ++
          "                        ]\n" ++
          "   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)\n" ++
          "                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)\n" ++
          "                        , 0x80 + oc .&. 0x3f\n" ++
          "                        ]\n" ++
          "   | otherwise        = [ 0xf0 + (oc `shiftR` 18)\n" ++
          "                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)\n" ++
          "                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)\n" ++
          "                        , 0x80 + oc .&. 0x3f\n" ++
          "                        ]\n"

mainFuns :: String
mainFuns = start ++ lexFile ++ readCode
  where start = createBlockComment "Functions that read the lexfile"
        lexFile =
          newFun "lexFile" [("file","FilePath")] "IO LexTree" ++
          "readCode file >>= return . F.fromList\n"
        readCode =
          newFun "readCode" [("file","FilePath")] "IO [Byte]" ++ "do\n" ++
          "  handle <- openFile file ReadMode\n" ++
          "  hSetEncoding handle utf8\n" ++
          "  hGetContents handle >>= return . concatMap encode\n"

newFun :: String -> [(String,String)] -> String -> String
newFun name vars_type return =
  let (vars,types) = foldl (\(vs,ts) (v,t) -> (vs ++ " " ++ v,ts ++ t ++ " -> ")) ("","") vars_type
  in "\n" ++ name ++ " :: " ++ types ++ return ++ "\n" ++
                               name ++ vars ++ " = "

createBlockComment :: String -> String
createBlockComment str = "\n\n" ++ take 80 (repeat '-') ++ "\n" ++
                         foldl (\s line -> s ++ "-- " ++ line ++ take (75 - length line)
                                           (repeat ' ') ++ "--\n") "" strs ++
                         take 80 (repeat '-')
  where strs = chunkList 74 str

chunkList :: Int -> String -> [String]
chunkList _ [] = []
chunkList l str = take l str:chunkList l (drop l str)