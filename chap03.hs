forall, exists :: (a -> Bool) -> [a] -> Bool
forall p = and . map p
exists p = or . map p

some, every :: [a] -> (a -> Bool) -> Bool
some xs p  = exists p xs
every xs p = forall p xs

imply, xor :: Bool -> Bool -> Bool
imply p q = not p || q
xor p q = p /= q

unique :: Eq a => (a -> Bool) -> [a] -> Bool
unique p xs = some xs (\ x -> p x && every xs (\ y -> (imply (p y) (y == x))))

parity :: [Bool] -> Bool
parity [] = True
parity (True:xs) = not (parity xs)
parity (False:xs) = parity xs

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)

prime :: Integer -> Bool
prime n | n <= 1    = False
        | otherwise = ldp n == n where
    ldp = lpdf primes
    lpdf (p:ps) m | rem m p == 0 = p
                  | p ^ 2 > m    = m
                  | otherwise    = lpdf ps m
    primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
    where
    mark :: [Integer] -> Integer -> Integer -> [Integer]
    mark [] k m = []
    mark (y:ys) k m | k == m    = 0 : (mark ys 1 m)
                    | otherwise = y : (mark ys (k + 1) m)

primes :: [Integer]
primes = sieve [2..]

reduce :: (a -> a -> a) -> [a] -> a
reduce op [] = error "empty list not accepted."
reduce op [x] = x
reduce op (x : xs) = op x (reduce op xs)

primePairs :: [(Integer, Integer)]
primePairs = pairs primes
    where
    pairs (x:y:xys) | x + 2 == y = (x, y) : pairs (y:xys)
                    | otherwise  = pairs(y:xys)
