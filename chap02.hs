valid :: (Bool -> Bool -> Bool) -> Bool
valid bf = and [ bf p q | p <- [True, False],
                          q <- [True, False]]

logEquiv :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv bf1 bf2 = and [ bf1 p q == bf2 p q | p <- [True, False],
                                              q <- [True, False]]

deMorganL :: Bool -> Bool -> Bool
deMorganL p q = not (p && q)

deMorganR :: Bool -> Bool -> Bool
deMorganR p q = not p || not q

xor :: Bool -> Bool -> Bool
xor p q = p /= q

imply :: Bool -> Bool -> Bool
imply p q = not p || q
