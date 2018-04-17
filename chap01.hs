blowup_aux :: Int -> Char -> String
blowup_aux 0 x = []
blowup_aux n x = x : (blowup_aux (n - 1) x)

blowup :: String -> String
blowup [] = []
blowup xs = blowup (init xs) ++ (blowup_aux (length xs) (last xs))

minString :: [String] -> String
minString [x] = x
minString (x:xs) | x > minString xs     = minString xs
                 | otherwise            = x

quickRemove :: String -> [String] -> [String]
quickRemove a [] = []
quickRemove a (x:xs) | a == x       = xs
                     | otherwise    = x : (quickRemove a xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs = x : (srtString (quickRemove x xs)) where x = (minString xs)

prefix :: String -> String -> Bool
prefix [] b = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) | x /= y       = False
                     | otherwise    = prefix xs ys

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs (y:ys) = prefix xs (y:ys) || (substring xs ys)

factorisation :: Int -> [Int]
factorisation a | a < -1    = (-1) : (factorisation (-a))
                | a == 1    = []
                | a > 1     = c : (factorisation (div a c)) where c = (ldpf primes1 a)

lengths :: [[a]] -> [Int]
lengths xs = map length xs

sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

ldpf :: [Int] -> Int -> Int
ldpf (p:ps) n | rem n p == 0    = p
              | p ^ 2 > n       = n
              | otherwise       = ldpf ps n

prime :: Int -> Bool
prime n | n <= 1    = False
        | otherwise = ldpf primes1 n == n

primes1 :: [Int]
primes1 = 2 : filter prime [3..]
