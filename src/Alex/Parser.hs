{-# OPTIONS_GHC -w #-}
-- -----------------------------------------------------------------------------
-- 
-- Parser.y, part of Alex
--
-- (c) Simon Marlow 2003
--
-- -----------------------------------------------------------------------------

{-# OPTIONS_GHC -w #-}

module Alex.Parser ( parse, P ) where
import Alex.AbsSyn
import Alex.Scan
import Alex.CharSet
import Alex.ParseMonad hiding ( StartCode )

import Data.Char
--import Debug.Trace

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ((Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code)))
	| HappyAbsSyn5 (Maybe (AlexPosn,Code))
	| HappyAbsSyn6 ([Directive])
	| HappyAbsSyn7 (Directive)
	| HappyAbsSyn8 (())
	| HappyAbsSyn10 (Scanner)
	| HappyAbsSyn11 ([RECtx])
	| HappyAbsSyn13 (RECtx)
	| HappyAbsSyn15 ([(String,StartCode)])
	| HappyAbsSyn17 (String)
	| HappyAbsSyn18 (Maybe Code)
	| HappyAbsSyn19 (Maybe CharSet, RExp, RightContext RExp)
	| HappyAbsSyn20 (CharSet)
	| HappyAbsSyn21 (RightContext RExp)
	| HappyAbsSyn22 (RExp)
	| HappyAbsSyn25 (RExp -> RExp)
	| HappyAbsSyn29 ([CharSet])
	| HappyAbsSyn30 ((AlexPosn,String))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

action_0 (56) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (56) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (62) = happyShift action_7
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 _ = happyReduce_5

action_3 _ = happyReduce_2

action_4 (63) = happyAccept
action_4 _ = happyFail

action_5 (60) = happyShift action_12
action_5 (61) = happyShift action_13
action_5 (8) = happyGoto action_10
action_5 (9) = happyGoto action_11
action_5 _ = happyReduce_8

action_6 (62) = happyShift action_7
action_6 (6) = happyGoto action_9
action_6 (7) = happyGoto action_6
action_6 _ = happyReduce_5

action_7 (53) = happyShift action_8
action_7 _ = happyFail

action_8 _ = happyReduce_6

action_9 _ = happyReduce_4

action_10 (54) = happyShift action_32
action_10 (10) = happyGoto action_31
action_10 _ = happyFail

action_11 (60) = happyShift action_12
action_11 (61) = happyShift action_13
action_11 (8) = happyGoto action_30
action_11 (9) = happyGoto action_11
action_11 _ = happyReduce_8

action_12 (31) = happyShift action_21
action_12 (46) = happyShift action_23
action_12 (48) = happyShift action_24
action_12 (57) = happyShift action_26
action_12 (58) = happyShift action_27
action_12 (27) = happyGoto action_29
action_12 (28) = happyGoto action_19
action_12 (30) = happyGoto action_20
action_12 _ = happyFail

action_13 (31) = happyShift action_21
action_13 (43) = happyShift action_22
action_13 (46) = happyShift action_23
action_13 (48) = happyShift action_24
action_13 (53) = happyShift action_25
action_13 (57) = happyShift action_26
action_13 (58) = happyShift action_27
action_13 (59) = happyShift action_28
action_13 (22) = happyGoto action_14
action_13 (23) = happyGoto action_15
action_13 (24) = happyGoto action_16
action_13 (26) = happyGoto action_17
action_13 (27) = happyGoto action_18
action_13 (28) = happyGoto action_19
action_13 (30) = happyGoto action_20
action_13 _ = happyFail

action_14 _ = happyReduce_10

action_15 (31) = happyShift action_21
action_15 (37) = happyShift action_58
action_15 (43) = happyShift action_22
action_15 (46) = happyShift action_23
action_15 (48) = happyShift action_24
action_15 (53) = happyShift action_25
action_15 (57) = happyShift action_26
action_15 (58) = happyShift action_27
action_15 (59) = happyShift action_28
action_15 (24) = happyGoto action_57
action_15 (26) = happyGoto action_17
action_15 (27) = happyGoto action_18
action_15 (28) = happyGoto action_19
action_15 (30) = happyGoto action_20
action_15 _ = happyReduce_36

action_16 _ = happyReduce_38

action_17 (38) = happyShift action_53
action_17 (39) = happyShift action_54
action_17 (40) = happyShift action_55
action_17 (41) = happyShift action_56
action_17 (25) = happyGoto action_52
action_17 _ = happyReduce_40

action_18 (45) = happyShift action_44
action_18 _ = happyReduce_50

action_19 _ = happyReduce_53

action_20 _ = happyReduce_56

action_21 _ = happyReduce_62

action_22 (31) = happyShift action_21
action_22 (43) = happyShift action_22
action_22 (44) = happyShift action_51
action_22 (46) = happyShift action_23
action_22 (48) = happyShift action_24
action_22 (53) = happyShift action_25
action_22 (57) = happyShift action_26
action_22 (58) = happyShift action_27
action_22 (59) = happyShift action_28
action_22 (22) = happyGoto action_50
action_22 (23) = happyGoto action_15
action_22 (24) = happyGoto action_16
action_22 (26) = happyGoto action_17
action_22 (27) = happyGoto action_18
action_22 (28) = happyGoto action_19
action_22 (30) = happyGoto action_20
action_22 _ = happyFail

action_23 (31) = happyShift action_21
action_23 (46) = happyShift action_23
action_23 (48) = happyShift action_24
action_23 (57) = happyShift action_26
action_23 (58) = happyShift action_27
action_23 (28) = happyGoto action_49
action_23 (30) = happyGoto action_20
action_23 _ = happyFail

action_24 (31) = happyShift action_21
action_24 (46) = happyShift action_23
action_24 (48) = happyShift action_24
action_24 (50) = happyShift action_48
action_24 (57) = happyShift action_26
action_24 (58) = happyShift action_27
action_24 (27) = happyGoto action_46
action_24 (28) = happyGoto action_19
action_24 (29) = happyGoto action_47
action_24 (30) = happyGoto action_20
action_24 _ = happyReduce_61

action_25 _ = happyReduce_48

action_26 (47) = happyShift action_45
action_26 _ = happyReduce_54

action_27 _ = happyReduce_63

action_28 _ = happyReduce_49

action_29 (45) = happyShift action_44
action_29 _ = happyReduce_9

action_30 _ = happyReduce_7

action_31 (56) = happyShift action_3
action_31 (5) = happyGoto action_43
action_31 _ = happyReduce_3

action_32 (31) = happyShift action_21
action_32 (33) = happyShift action_41
action_32 (43) = happyShift action_22
action_32 (46) = happyShift action_23
action_32 (48) = happyShift action_24
action_32 (50) = happyShift action_42
action_32 (53) = happyShift action_25
action_32 (57) = happyShift action_26
action_32 (58) = happyShift action_27
action_32 (59) = happyShift action_28
action_32 (11) = happyGoto action_33
action_32 (12) = happyGoto action_34
action_32 (13) = happyGoto action_35
action_32 (15) = happyGoto action_36
action_32 (19) = happyGoto action_37
action_32 (20) = happyGoto action_38
action_32 (22) = happyGoto action_39
action_32 (23) = happyGoto action_15
action_32 (24) = happyGoto action_16
action_32 (26) = happyGoto action_17
action_32 (27) = happyGoto action_40
action_32 (28) = happyGoto action_19
action_32 (30) = happyGoto action_20
action_32 _ = happyReduce_13

action_33 _ = happyReduce_11

action_34 (31) = happyShift action_21
action_34 (33) = happyShift action_41
action_34 (43) = happyShift action_22
action_34 (46) = happyShift action_23
action_34 (48) = happyShift action_24
action_34 (50) = happyShift action_42
action_34 (53) = happyShift action_25
action_34 (57) = happyShift action_26
action_34 (58) = happyShift action_27
action_34 (59) = happyShift action_28
action_34 (11) = happyGoto action_81
action_34 (12) = happyGoto action_34
action_34 (13) = happyGoto action_35
action_34 (15) = happyGoto action_36
action_34 (19) = happyGoto action_37
action_34 (20) = happyGoto action_38
action_34 (22) = happyGoto action_39
action_34 (23) = happyGoto action_15
action_34 (24) = happyGoto action_16
action_34 (26) = happyGoto action_17
action_34 (27) = happyGoto action_40
action_34 (28) = happyGoto action_19
action_34 (30) = happyGoto action_20
action_34 _ = happyReduce_13

action_35 _ = happyReduce_16

action_36 (31) = happyShift action_21
action_36 (41) = happyShift action_80
action_36 (43) = happyShift action_22
action_36 (46) = happyShift action_23
action_36 (48) = happyShift action_24
action_36 (50) = happyShift action_42
action_36 (53) = happyShift action_25
action_36 (57) = happyShift action_26
action_36 (58) = happyShift action_27
action_36 (59) = happyShift action_28
action_36 (13) = happyGoto action_79
action_36 (19) = happyGoto action_37
action_36 (20) = happyGoto action_38
action_36 (22) = happyGoto action_39
action_36 (23) = happyGoto action_15
action_36 (24) = happyGoto action_16
action_36 (26) = happyGoto action_17
action_36 (27) = happyGoto action_40
action_36 (28) = happyGoto action_19
action_36 (30) = happyGoto action_20
action_36 _ = happyFail

action_37 (32) = happyShift action_77
action_37 (56) = happyShift action_78
action_37 (18) = happyGoto action_76
action_37 _ = happyFail

action_38 (31) = happyShift action_21
action_38 (43) = happyShift action_22
action_38 (46) = happyShift action_23
action_38 (48) = happyShift action_24
action_38 (53) = happyShift action_25
action_38 (57) = happyShift action_26
action_38 (58) = happyShift action_27
action_38 (59) = happyShift action_28
action_38 (22) = happyGoto action_75
action_38 (23) = happyGoto action_15
action_38 (24) = happyGoto action_16
action_38 (26) = happyGoto action_17
action_38 (27) = happyGoto action_18
action_38 (28) = happyGoto action_19
action_38 (30) = happyGoto action_20
action_38 _ = happyFail

action_39 (36) = happyShift action_73
action_39 (51) = happyShift action_74
action_39 (21) = happyGoto action_72
action_39 _ = happyReduce_34

action_40 (45) = happyShift action_44
action_40 (50) = happyShift action_71
action_40 _ = happyReduce_50

action_41 (52) = happyShift action_69
action_41 (55) = happyShift action_70
action_41 (16) = happyGoto action_67
action_41 (17) = happyGoto action_68
action_41 _ = happyFail

action_42 _ = happyReduce_29

action_43 _ = happyReduce_1

action_44 (31) = happyShift action_21
action_44 (46) = happyShift action_23
action_44 (48) = happyShift action_24
action_44 (57) = happyShift action_26
action_44 (58) = happyShift action_27
action_44 (28) = happyGoto action_66
action_44 (30) = happyGoto action_20
action_44 _ = happyFail

action_45 (57) = happyShift action_65
action_45 _ = happyFail

action_46 (31) = happyShift action_21
action_46 (45) = happyShift action_44
action_46 (46) = happyShift action_23
action_46 (48) = happyShift action_24
action_46 (57) = happyShift action_26
action_46 (58) = happyShift action_27
action_46 (27) = happyGoto action_46
action_46 (28) = happyGoto action_19
action_46 (29) = happyGoto action_64
action_46 (30) = happyGoto action_20
action_46 _ = happyReduce_61

action_47 (49) = happyShift action_63
action_47 _ = happyFail

action_48 (31) = happyShift action_21
action_48 (46) = happyShift action_23
action_48 (48) = happyShift action_24
action_48 (57) = happyShift action_26
action_48 (58) = happyShift action_27
action_48 (27) = happyGoto action_46
action_48 (28) = happyGoto action_19
action_48 (29) = happyGoto action_62
action_48 (30) = happyGoto action_20
action_48 _ = happyReduce_61

action_49 _ = happyReduce_59

action_50 (44) = happyShift action_61
action_50 _ = happyFail

action_51 _ = happyReduce_47

action_52 _ = happyReduce_39

action_53 _ = happyReduce_41

action_54 _ = happyReduce_42

action_55 _ = happyReduce_43

action_56 (57) = happyShift action_60
action_56 _ = happyFail

action_57 _ = happyReduce_37

action_58 (31) = happyShift action_21
action_58 (43) = happyShift action_22
action_58 (46) = happyShift action_23
action_58 (48) = happyShift action_24
action_58 (53) = happyShift action_25
action_58 (57) = happyShift action_26
action_58 (58) = happyShift action_27
action_58 (59) = happyShift action_28
action_58 (22) = happyGoto action_59
action_58 (23) = happyGoto action_15
action_58 (24) = happyGoto action_16
action_58 (26) = happyGoto action_17
action_58 (27) = happyGoto action_18
action_58 (28) = happyGoto action_19
action_58 (30) = happyGoto action_20
action_58 _ = happyFail

action_59 _ = happyReduce_35

action_60 (35) = happyShift action_90
action_60 (42) = happyShift action_91
action_60 _ = happyFail

action_61 _ = happyReduce_51

action_62 (49) = happyShift action_89
action_62 _ = happyFail

action_63 _ = happyReduce_57

action_64 _ = happyReduce_60

action_65 _ = happyReduce_55

action_66 _ = happyReduce_52

action_67 (34) = happyShift action_88
action_67 _ = happyFail

action_68 (35) = happyShift action_87
action_68 _ = happyReduce_22

action_69 _ = happyReduce_23

action_70 _ = happyReduce_24

action_71 _ = happyReduce_30

action_72 _ = happyReduce_28

action_73 _ = happyReduce_31

action_74 (31) = happyShift action_21
action_74 (43) = happyShift action_22
action_74 (46) = happyShift action_23
action_74 (48) = happyShift action_24
action_74 (53) = happyShift action_25
action_74 (56) = happyShift action_86
action_74 (57) = happyShift action_26
action_74 (58) = happyShift action_27
action_74 (59) = happyShift action_28
action_74 (22) = happyGoto action_85
action_74 (23) = happyGoto action_15
action_74 (24) = happyGoto action_16
action_74 (26) = happyGoto action_17
action_74 (27) = happyGoto action_18
action_74 (28) = happyGoto action_19
action_74 (30) = happyGoto action_20
action_74 _ = happyFail

action_75 (36) = happyShift action_73
action_75 (51) = happyShift action_74
action_75 (21) = happyGoto action_84
action_75 _ = happyReduce_34

action_76 _ = happyReduce_17

action_77 _ = happyReduce_26

action_78 _ = happyReduce_25

action_79 _ = happyReduce_14

action_80 (31) = happyShift action_21
action_80 (43) = happyShift action_22
action_80 (46) = happyShift action_23
action_80 (48) = happyShift action_24
action_80 (50) = happyShift action_42
action_80 (53) = happyShift action_25
action_80 (57) = happyShift action_26
action_80 (58) = happyShift action_27
action_80 (59) = happyShift action_28
action_80 (13) = happyGoto action_82
action_80 (14) = happyGoto action_83
action_80 (19) = happyGoto action_37
action_80 (20) = happyGoto action_38
action_80 (22) = happyGoto action_39
action_80 (23) = happyGoto action_15
action_80 (24) = happyGoto action_16
action_80 (26) = happyGoto action_17
action_80 (27) = happyGoto action_40
action_80 (28) = happyGoto action_19
action_80 (30) = happyGoto action_20
action_80 _ = happyReduce_19

action_81 _ = happyReduce_12

action_82 (31) = happyShift action_21
action_82 (43) = happyShift action_22
action_82 (46) = happyShift action_23
action_82 (48) = happyShift action_24
action_82 (50) = happyShift action_42
action_82 (53) = happyShift action_25
action_82 (57) = happyShift action_26
action_82 (58) = happyShift action_27
action_82 (59) = happyShift action_28
action_82 (13) = happyGoto action_82
action_82 (14) = happyGoto action_96
action_82 (19) = happyGoto action_37
action_82 (20) = happyGoto action_38
action_82 (22) = happyGoto action_39
action_82 (23) = happyGoto action_15
action_82 (24) = happyGoto action_16
action_82 (26) = happyGoto action_17
action_82 (27) = happyGoto action_40
action_82 (28) = happyGoto action_19
action_82 (30) = happyGoto action_20
action_82 _ = happyReduce_19

action_83 (42) = happyShift action_95
action_83 _ = happyFail

action_84 _ = happyReduce_27

action_85 _ = happyReduce_32

action_86 _ = happyReduce_33

action_87 (52) = happyShift action_69
action_87 (55) = happyShift action_70
action_87 (16) = happyGoto action_94
action_87 (17) = happyGoto action_68
action_87 _ = happyFail

action_88 _ = happyReduce_20

action_89 _ = happyReduce_58

action_90 (42) = happyShift action_92
action_90 (57) = happyShift action_93
action_90 _ = happyFail

action_91 _ = happyReduce_44

action_92 _ = happyReduce_45

action_93 (42) = happyShift action_97
action_93 _ = happyFail

action_94 _ = happyReduce_21

action_95 _ = happyReduce_15

action_96 _ = happyReduce_18

action_97 _ = happyReduce_46

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_1,happy_var_2,happy_var_4,happy_var_5)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_1 of T pos (CodeT code) -> 
						Just (pos,code)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 (Nothing
	)

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyTerminal (T _ (StringT happy_var_2)))
	_
	 =  HappyAbsSyn7
		 (WrapperDirective happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn8
		 (()
	)

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 (()
	)

happyReduce_9 = happyMonadReduce 2 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal (T _ (SMacDefT happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen (( newSMac happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_10 = happyMonadReduce 2 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal (T _ (RMacDefT happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen (( newRMac happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (T _ (BindT happy_var_1)))
	 =  HappyAbsSyn10
		 (Scanner happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  11 happyReduction_13
happyReduction_13  =  HappyAbsSyn11
		 ([]
	)

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn11
		 ([ replaceCodes happy_var_1 happy_var_2 ]
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 12 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (map (replaceCodes happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (let (l,e,r) = happy_var_1 in 
					  RECtx [] l e r happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  14 happyReduction_19
happyReduction_19  =  HappyAbsSyn11
		 ([]
	)

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 ((happy_var_1,0) : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 ([(happy_var_1,0)]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn17
		 ("0"
	)

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyTerminal (T _ (IdT happy_var_1)))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (case happy_var_1 of T _ (CodeT code) -> Just code
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn18
		 (Nothing
	)

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ((Just happy_var_1,happy_var_2,happy_var_3)
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  19 happyReduction_28
happyReduction_28 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn19
		 ((Nothing,happy_var_1,happy_var_2)
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn20
		 (charSetSingleton '\n'
	)

happyReduce_30 = happySpecReduce_2  20 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  21 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn21
		 (RightContextRExp (Ch (charSetSingleton '\n'))
	)

happyReduce_32 = happySpecReduce_2  21 happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (RightContextRExp happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  21 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn21
		 (RightContextCode (case happy_var_2 of 
						T _ (CodeT code) -> code)
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  21 happyReduction_34
happyReduction_34  =  HappyAbsSyn21
		 (NoRightContext
	)

happyReduce_35 = happySpecReduce_3  22 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 :| happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  23 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 :%% happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  24 happyReduction_39
happyReduction_39 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 happy_var_1
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn25
		 (Star
	)

happyReduce_42 = happySpecReduce_1  25 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn25
		 (Plus
	)

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn25
		 (Ques
	)

happyReduce_44 = happySpecReduce_3  25 happyReduction_44
happyReduction_44 _
	(HappyTerminal (T _ (CharT happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (repeat_rng (digit happy_var_2) Nothing
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 25 happyReduction_45
happyReduction_45 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T _ (CharT happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (repeat_rng (digit happy_var_2) (Just Nothing)
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 5 25 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyTerminal (T _ (CharT happy_var_4))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T _ (CharT happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (repeat_rng (digit happy_var_2) (Just (Just (digit happy_var_4)))
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_2  26 happyReduction_47
happyReduction_47 _
	_
	 =  HappyAbsSyn22
		 (Eps
	)

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyTerminal (T _ (StringT happy_var_1)))
	 =  HappyAbsSyn22
		 (foldr (:%%) Eps 
					    (map (Ch . charSetSingleton) happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyMonadReduce 1 26 happyReduction_49
happyReduction_49 ((HappyTerminal (T _ (RMacT happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen (( lookupRMac happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn22
		 (Ch happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  26 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  27 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 `charSetMinus` happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  27 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyTerminal (T _ (CharT happy_var_1)))
	 =  HappyAbsSyn20
		 (charSetSingleton happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  28 happyReduction_55
happyReduction_55 (HappyTerminal (T _ (CharT happy_var_3)))
	_
	(HappyTerminal (T _ (CharT happy_var_1)))
	 =  HappyAbsSyn20
		 (charSetRange happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happyMonadReduce 1 28 happyReduction_56
happyReduction_56 ((HappyAbsSyn30  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lookupSMac happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_57 = happySpecReduce_3  28 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (foldr charSetUnion emptyCharSet happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyMonadReduce 4 28 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do { dot <- lookupSMac (tokPosn happy_var_1, ".");
		      	        return (dot `charSetMinus`
			      		  foldr charSetUnion emptyCharSet happy_var_3) })
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_59 = happyMonadReduce 2 28 happyReduction_59
happyReduction_59 ((HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do { dot <- lookupSMac (tokPosn happy_var_1, ".");
		      	        return (dot `charSetMinus` happy_var_2) })
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_60 = happySpecReduce_2  29 happyReduction_60
happyReduction_60 (HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1 : happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  29 happyReduction_61
happyReduction_61  =  HappyAbsSyn29
		 ([]
	)

happyReduce_62 = happySpecReduce_1  30 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((tokPosn happy_var_1, ".")
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  30 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (case happy_var_1 of T p (SMacT s) -> (p, s)
	)
happyReduction_63 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	T _ EOFT -> action 63 63 tk (HappyState action) sts stk;
	T _ (SpecialT '.') -> cont 31;
	T _ (SpecialT ';') -> cont 32;
	T _ (SpecialT '<') -> cont 33;
	T _ (SpecialT '>') -> cont 34;
	T _ (SpecialT ',') -> cont 35;
	T _ (SpecialT '$') -> cont 36;
	T _ (SpecialT '|') -> cont 37;
	T _ (SpecialT '*') -> cont 38;
	T _ (SpecialT '+') -> cont 39;
	T _ (SpecialT '?') -> cont 40;
	T _ (SpecialT '{') -> cont 41;
	T _ (SpecialT '}') -> cont 42;
	T _ (SpecialT '(') -> cont 43;
	T _ (SpecialT ')') -> cont 44;
	T _ (SpecialT '#') -> cont 45;
	T _ (SpecialT '~') -> cont 46;
	T _ (SpecialT '-') -> cont 47;
	T _ (SpecialT '[') -> cont 48;
	T _ (SpecialT ']') -> cont 49;
	T _ (SpecialT '^') -> cont 50;
	T _ (SpecialT '/') -> cont 51;
	T _ ZeroT -> cont 52;
	T _ (StringT happy_dollar_dollar) -> cont 53;
	T _ (BindT happy_dollar_dollar) -> cont 54;
	T _ (IdT happy_dollar_dollar) -> cont 55;
	T _ (CodeT _) -> cont 56;
	T _ (CharT happy_dollar_dollar) -> cont 57;
	T _ (SMacT _) -> cont 58;
	T _ (RMacT happy_dollar_dollar) -> cont 59;
	T _ (SMacDefT happy_dollar_dollar) -> cont 60;
	T _ (RMacDefT happy_dollar_dollar) -> cont 61;
	T _ WrapperT -> cont 62;
	_ -> happyError' tk
	})

happyError_ 63 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = ((>>=))
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: P a
happyError = failP "parse error"

-- -----------------------------------------------------------------------------
-- Utils

digit c = ord c - ord '0'

repeat_rng :: Int -> Maybe (Maybe Int) -> (RExp->RExp)
repeat_rng n (Nothing) re = foldr (:%%) Eps (replicate n re)
repeat_rng n (Just Nothing) re = foldr (:%%) (Star re) (replicate n re)
repeat_rng n (Just (Just m)) re = intl :%% rst
	where
	intl = repeat_rng n Nothing re
	rst = foldr (\re re'->Ques(re :%% re')) Eps (replicate (m-n) re)

replaceCodes codes rectx = rectx{ reCtxStartCodes = codes }
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
