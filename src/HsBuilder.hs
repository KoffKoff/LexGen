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
        "type TokenID    = [Accept Code]\n" ++
        "type Transition = Map SNum SNum\n" ++
        "type Automata   = Array Int Transition\n" ++
        "type Accepts    = Array SNum [Accept Code]\n" ++
        "type Lexeme     = String\n" ++
        "newtype Tokens  = Tokens {getSeq :: Map SNum (Seq Token,SNum)}\n" ++
        "type LexTree    = FingerTree Tokens Byte\n\n" ++
        "data Token = Token { lexeme      :: Lexeme\n" ++
        "                   , token_id    :: TokenID}\n" ++
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
          "  mempty = Tokens mempty\n" ++
          "  mappend = combineTokens\n\n"
        measured_tokens =
          "instance F.Measured Tokens Byte where\n" ++
          "  measure byte = let t = automata ! fromEnum byte\n" ++
          "                 in Tokens $ Map.map (\\os -> (" ++
          "singleton (Token [toEnum (fromEnum byte)] (snd os)),fst os)) t\n\n"
        show_token =
          "instance Show Token where\n" ++
          "  show token = printf \"%-11s:%s\" (showID $ token_id token)\n" ++
          "                                   (fixLex $ lexeme token)\n"
        show_tokens =
          "instance Show Tokens where\n" ++
          "  show (Tokens seqs) = case Map.lookup (startState) seqs of\n" ++
          "    Just (toks,_) -> foldlWithIndex (\\str _ tok -> str ++" ++
          "\"\\n\" ++ show tok) \"\" toks\n" ++
          "    _             -> \"Lexical error\"\n\n"


dfaOut :: DFA'' -> String
dfaOut dfa = start ++ newFun "startState" [] "SNum" ++ show start_state ++ "\n"
             ++ automata ++ accepting
  where start_state:_ = start_states dfa
        automata = newFun "automata" [] "Automata" ++
                   fixAutomata (show $ states dfa) ++ "\n"
        accepting = newFun "accepting" [] "Accepts" ++
                    show (accepts dfa)
        start = createBlockComment "The automata"

fixAutomata :: String -> String
fixAutomata automata = replace "fromList" "Map.fromList" automata

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace ls repl strs | ls == take (length ls) strs = repl ++ replace ls repl
                                                     (drop (length ls) strs)
                     | otherwise = head strs : (replace ls repl $ tail strs)

combinatorFuns :: String
combinatorFuns = start ++ combineTokens ++ combineSequence ++ mergeTokens ++ appendTokens
  where start = createBlockComment "The Combinators for the tokens"
        combineTokens =
          newFun "combineTokens" [("toks1","Tokens"),("toks2","Tokens")] "Tokens" ++
          "Map.foldlWithKey (combineSequence toks2) mempty (getSeq toks1)\n"
        combineSequence =
          newFun "combineSequence" [("toks2","Tokens"),("outToks","Tokens")
                                   ,("inState","SNum"),("(tokSeq1,midState)","(Seq Token,SNum)")]
                                   "Tokens" ++ "\n" ++
          "  Tokens $ case Map.lookup midState (getSeq toks2) of\n" ++
          "    Just (tokSeq2,outState) ->\n" ++
          "      Map.insert inState (mergeTokens tokSeq1 tokSeq2,outState) (getSeq outToks)\n" ++
          "    Nothing -> let _ :> t1end = viewr tokSeq1\n" ++
          "               in case (token_id t1end,Map.lookup startState (getSeq toks2)) of\n" ++
          "      (a:as,Just (tokSeq2,outState)) ->\n" ++
          "        Map.insert inState (appendTokens tokSeq1 tokSeq2,outState) (getSeq outToks)\n" ++
          "      _                              -> getSeq outToks"
        mergeTokens =
          newFun "mergeTokens" [("toks1","Seq Token"),("toks2","Seq Token")] "Seq Token" ++
          "\n" ++
          "  let toks1' :> token1 = viewr toks1\n" ++
          "      token2 :< toks2' = viewl toks2\n" ++
          "  in (toks1' |> Token (lexeme token1 ++ lexeme token2) (token_id token2)) >< toks2'\n"
        appendTokens =
          newFun "appendTokens" [("","Seq Token"),("","Seq Token")] "Seq Token" ++
          "mappend"

utilFuns :: String
utilFuns = start ++ tokens ++ encode ++ showID ++ fixLex
  where start = createBlockComment "Utility functions"
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
          "  []   -> \"No Token\"\n" ++
          "  accs -> show $ map show_acc_num accs\n" ++
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