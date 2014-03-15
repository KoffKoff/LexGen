mergeSuff :: Suffix -> Suffix -> Transition -> Suffix
mergeSuff suff1 (One token2) _ = One $ mergeToken suff1 token2
