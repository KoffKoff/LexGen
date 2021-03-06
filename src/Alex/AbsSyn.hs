-- -----------------------------------------------------------------------------
-- 
-- AbsSyn.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- This module provides a concrete representation for regular expressions and
-- scanners.  Scanners are used for tokenising files in preparation for parsing.
--
-- ----------------------------------------------------------------------------}

module Alex.AbsSyn (
  Code, Directive(..),
  Scanner(..),
  RECtx(..),
  RExp(..),
--  DFA'(..),Edges(..), --Accept'(..)
  DFA(..), State(..), SNum, StartCode, Accept(..),
  RightContext(..), showRCtx,
  encodeStartCodes, extractActions,
  Target(..),
  UsesPreds(..), usesPreds
  ) where

import Alex.CharSet ( CharSet )
import Alex.Map ( Map )
import qualified Alex.Map as Map hiding ( Map )
import Data.IntMap (IntMap)
import Alex.Sort ( nub' )
import Alex.Util ( str, nl )

import Data.Maybe ( fromJust )

infixl 4 :|
infixl 5 :%%

-- -----------------------------------------------------------------------------
-- Abstract Syntax for Alex scripts

type Code = String

data Directive
   = WrapperDirective String		-- use this wrapper

-- TODO: update this comment
--
-- A `Scanner' consists of an association list associating token names with
-- regular expressions with context.  The context may include a list of start
-- codes, some leading context to test the character immediately preceding the
-- token and trailing context to test the residual input after the token.
--  
-- The start codes consist of the names and numbers of the start codes;
-- initially the names only will be generated by the parser, the numbers being
-- allocated at a later stage.  Start codes become meaningful when scanners are
-- converted to DFAs; see the DFA section of the Scan module for details.

data Scanner = Scanner { scannerName   :: String,
			 scannerTokens :: [RECtx] }
  deriving Show

data RECtx = RECtx { reCtxStartCodes :: [(String,StartCode)],
		     reCtxPreCtx     :: Maybe CharSet,
		     reCtxRE	     :: RExp,
		     reCtxPostCtx    :: RightContext RExp,
		     reCtxCode	     :: Maybe Code
		   }

data RightContext r
  = NoRightContext 
  | RightContextRExp r
  | RightContextCode Code
  deriving (Eq,Ord,Show)

instance Show RECtx where
  showsPrec _ (RECtx scs _ r rctx code) = 
	showStarts scs . shows r . showRCtx rctx . showMaybeCode code

showMaybeCode :: Maybe String -> String -> String
showMaybeCode Nothing = id
showMaybeCode (Just code) = showCode code

showCode :: String -> String -> String
showCode code = showString " { " . showString code . showString " }"

showStarts :: [(String, StartCode)] -> String -> String
showStarts [] = id
showStarts scs = shows scs

showRCtx :: Show r => RightContext r -> String -> String
showRCtx NoRightContext = id
showRCtx (RightContextRExp r) = ('\\':) . shows r
showRCtx (RightContextCode code) = showString "\\ " . showCode code

-- -----------------------------------------------------------------------------
-- DFAs

data DFA s a = DFA
  { dfa_start_states :: [s],
    dfa_states       :: Map s (State s a)
  }

data State s a = State { state_acc :: [Accept a],
                         state_out :: IntMap s -- 0..255 only
                       }

type SNum = Int

data Accept a
  = Acc { accPrio       :: Int,
	  accAction     :: Maybe a,
	  accLeftCtx    :: Maybe CharSet, -- cannot be converted to byteset at this point.
	  accRightCtx   :: RightContext SNum
    }
    deriving (Eq,Ord)

-- debug stuff
instance Show a => Show (Accept a) where
  show (Acc p act lctx rctx) = "Acc " ++ show p ++ " (" ++ show act ++ ") " ++ show lctx ++ " " ++ show rctx

type StartCode = Int

-- -----------------------------------------------------------------------------
-- Predicates / contexts

-- we can generate somewhat faster code in the case that
-- the lexer doesn't use predicates
data UsesPreds = UsesPreds | DoesntUsePreds

usesPreds :: DFA s a -> UsesPreds
usesPreds dfa
    | any acceptHasCtx [ acc | st  <- Map.elems (dfa_states dfa)
                             , acc <- state_acc st ]
    = UsesPreds
    | otherwise
    = DoesntUsePreds
  where
    acceptHasCtx Acc { accLeftCtx  = Nothing
                     , accRightCtx = NoRightContext } = False
    acceptHasCtx _                                    = True

-- -----------------------------------------------------------------------------
-- Regular expressions

-- `RExp' provides an abstract syntax for regular expressions.  `Eps' will
-- match empty strings; `Ch p' matches strings containinng a single character
-- `c' if `p c' is true; `re1 :%% re2' matches a string if `re1' matches one of
-- its prefixes and `re2' matches the rest; `re1 :| re2' matches a string if
-- `re1' or `re2' matches it; `Star re', `Plus re' and `Ques re' can be
-- expressed in terms of the other operators.  See the definitions of `ARexp'
-- for a formal definition of the semantics of these operators.

data RExp 
  = Eps
  | Ch CharSet
  | RExp :%% RExp
  | RExp :| RExp
  | Star RExp
  | Plus RExp
  | Ques RExp	

instance Show RExp where
  showsPrec _ Eps = showString "()"
  showsPrec _ (Ch _) = showString "[..]"
  showsPrec _ (l :%% r)  = shows l . shows r
  showsPrec _ (l :| r)  = shows l . ('|':) . shows r
  showsPrec _ (Star r) = shows r . ('*':)
  showsPrec _ (Plus r) = shows r . ('+':)
  showsPrec _ (Ques r) = shows r . ('?':)


-- -----------------------------------------------------------------------------
-- Utils

-- Map the available start codes onto [1..]

encodeStartCodes:: Scanner -> (Scanner,[StartCode],ShowS)
encodeStartCodes scan = (scan', 0 : map snd name_code_pairs, sc_hdr)
	where
	scan' = scan{ scannerTokens = map mk_re_ctx (scannerTokens scan) }

	mk_re_ctx (RECtx scs lc re rc code)
	  = RECtx (map mk_sc scs) lc re rc code

	mk_sc (nm,_) = (nm, if nm=="0" then 0 
				       else fromJust (Map.lookup nm code_map))

	sc_hdr tl =
		case name_code_pairs of
		  [] -> tl
		  (nm,_):rst -> "\n" ++ nm ++ foldr f t rst
			where
			f (nm', _) t' = "," ++ nm' ++ t'
			t = " :: Int\n" ++ foldr fmt_sc tl name_code_pairs
		where
		fmt_sc (nm,sc) t = nm ++ " = " ++ show sc ++ "\n" ++ t

	code_map = Map.fromList name_code_pairs

	name_code_pairs = zip (nub' (<=) nms) [1..]

	nms = [nm | RECtx{reCtxStartCodes = scs} <- scannerTokens scan,
		    (nm,_) <- scs, nm /= "0"]


-- Grab the code fragments for the token actions, and replace them
-- with function names of the form alex_action_$n$.  We do this
-- because the actual action fragments might be duplicated in the
-- generated file.

extractActions :: Scanner -> (Scanner,ShowS)
extractActions scanner = (scanner{scannerTokens = new_tokens}, decl_str)
 where
  (new_tokens, decls) = unzip (zipWith f (scannerTokens scanner) act_names)

  f r@RECtx{ reCtxCode = Just code } name
	= (r{reCtxCode = Just name}, Just (mkDecl name code))
  f r@RECtx{ reCtxCode = Nothing } _
	= (r{reCtxCode = Nothing}, Nothing)

  mkDecl fun code = str fun . str " = " . str code . nl

  act_names = map (\n -> "alex_action_" ++ show (n::Int)) [0..]

  decl_str = foldr (.) id [ decl | Just decl <- decls ]

-- -----------------------------------------------------------------------------
-- Code generation targets

data Target = GhcTarget | HaskellTarget

