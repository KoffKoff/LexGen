type LexTree  = FingerTree (Table State Tokens,Size) Char

type Table a b = Array State b
