module HsBuilder where

import System.IO
import Alex.AbsSyn
import BuildDFA

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
          "import Data.Array\n" ++
          "import Data.Sequence\n" ++
          "import Text.Printf\n --maybe cut this later\n" ++
          "import Alex.AbsSyn\n\n"

langExts :: String
langExts = "{-#LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}\n"

types :: String
types = createBlockComment "Data Types" ++
        "--type SNum       = Int\n" ++
        "--type Code       = String\n" ++
        "type Byte       = Word8\n" ++
        "type Transition = Map SNum (SNum,[Accept Code])\n" ++
        "type Automata   = Array Int Transition\n" ++
        "type TokenID    = Maybe [Accept Code]\n" ++
        "type Lexeme     = String\n" ++
        "data Tokens     = Toks (Seq Token)\n" ++
        "type LexTree    = FingerTree Tokens Byte\n\n" ++
        "data Token = T { transitions :: Transition\n" ++
        "               , lexeme      :: Lexeme\n" ++
        "               , sub_tokens  :: [Token]\n" ++
        "               , token_id    :: TokenID}\n" ++
        "{-data Accept a\n" ++
        "  = Acc { accPrio       :: Int,\n" ++
        "          accAction     :: Maybe a,\n" ++
        "          accLeftCtx    :: Maybe CharSet," ++
        " -- cannot be converted to byteset at this point.\n" ++
        "          accRightCtx   :: RightContext SNum\n" ++
        "    }\n" ++
        "    deriving (Eq,Ord)-}\n\n"

instances :: String
instances = start ++ monoid_tokens ++ measured_tokens ++ show_tokens ++ show_token
  where start = createBlockComment "Instances"
        monoid_tokens =
          "instance Monoid Tokens where\n" ++
          "  mempty = Toks mempty\n" ++
          "  (Toks toks1) `mappend` (Toks toks2) = Toks $ combineTokens toks1 toks2\n\n"
        measured_tokens =
          "instance F.Measured Tokens Byte where\n" ++
          "  measure byte = let t = automata ! fromEnum byte\n" ++
          "                 in Toks $ singleton (T t [toEnum" ++
          "(fromEnum byte)] [] (getTokenId t))\n\n"
        show_token =
          "instance Show Token where\n" ++
          "  show token = printf \"%-11s:%s\" (showID $ token_id token)\n" ++
          "                                   (fixLex $ lexeme token)\n"
        show_tokens =
          "instance Show Tokens where\n" ++
          "  show (Toks toks) = foldlWithIndex (\\str _ tok -> str ++" ++
          "\"\\n\" ++ show tok) \"\" toks\n\n"


dfaOut :: DFA' SNum Code -> String
dfaOut dfa = start ++ newFun "start_state" [] "SNum" ++ show start_state ++ "\n" ++
             newFun "automata" [] "Automata" ++ automata ++ "\n"
  where start_state:_ = dfa'_start_states dfa
        automata = fixAutomata $ show $ dfa'_states dfa
        start = createBlockComment "The automata"

fixAutomata :: String -> String
fixAutomata automata = replace "fromList" "Map.fromList" automata

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace ls repl strs | ls == take (length ls) strs = repl ++ replace ls repl
                                                     (drop (length ls) strs)
                     | otherwise = head strs : (replace ls repl $ tail strs)

combinatorFuns :: String
combinatorFuns = start ++ combineTokens ++ tokenAppender ++ divideAppender ++
                 splitToken ++ mergeToken
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
          "  [] -> splitToken tok1 tok2\n" ++
          "  [subtok1,subtok2] ->\n" ++
          "    let e = startTransition $ tabulate (transitions tok1) (transitions subtok1)\n" ++
          "    in if Map.null e\n" ++
          "       then case sub_tokens subtok1 of\n" ++
          "         [] -> splitToken tok1 tok2\n" ++
          "         mt -> combineTokens (divideAppender tok1 subtok1) (singleton subtok2)\n" ++
          "       else divideAppender (mergeToken tok1 subtok1) subtok2\n"
        splitToken =
          newFun "splitToken" [("tok1","Token"),("tok2","Token")] "Seq Token" ++
          "case token_id tok1 of\n" ++
          "  Just _  -> fromList [tok1,tok2]\n" ++
          "  Nothing -> case sub_tokens tok1 of\n" ++
          "    [] -> fromList [tok1,tok2]\n" ++
          "    [subtok1,subtok2] ->\n" ++
          "      let subsubtok2 :< toks2 = viewl $ tokenAppender subtok2 (singleton tok2)\n" ++
          "      in splitToken subtok1 subsubtok2 >< toks2\n"
        mergeToken =
          newFun "mergeToken" [("tok1","Token"),("tok2","Token")] "Token" ++ "\n" ++
          "  let e = tabulate (transitions tok1) (transitions tok2)\n" ++
          "  in T e (lexeme tok1 `mappend` lexeme tok2) [tok1,tok2] (getTokenId e)\n"

utilFuns :: String
utilFuns = start ++ tabulate ++ getTokenId ++ startTransition ++ tokens ++
           encode ++ showID ++ fixLex
  where start = createBlockComment "Utility functions"
        getTokenId =
          newFun "getTokenId" [("trans","Transition")] "TokenID" ++
          "case Map.lookup start_state trans of\n" ++
          "  Just (id,a:as) -> Just (a:as)\n" ++
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
        tokens =
          newFun "tokens" [("","LexTree")] "Tokens" ++ "F.measure\n"
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
        showID =
          newFun "showID" [("tokenid","TokenID")] "String" ++
          "case tokenid of\n" ++
          "  Just accs -> show $ map show_acc_num accs\n" ++
          "  Nothing   -> \"No Token\"\n" ++
          "  where show_acc_num (Acc p _ _ _) = \"Acc \" ++ show p\n"
        fixLex =
          newFun "fixLex" [("lexeme","Lexeme")] "Lexeme" ++
          "foldl convert_linebreaks \"\" lexeme\n" ++
          "  where convert_linebreaks str '\\n' = str ++ \"\\\\n\"\n" ++
          "        convert_linebreaks str c = str ++ [c]\n"

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
                         take 80 (repeat '-') ++ "\n"
  where strs = chunkList 74 str

chunkList :: Int -> String -> [String]
chunkList _ [] = []
chunkList l str = take l str:chunkList l (drop l str)