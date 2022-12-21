fibInit :: Int -> Int -> Int -> Int
fibInit n0 _ 0 = n0
fibInit _ n1 1 = n1
fibInit n0 n1 n = fibInit n0 n1 (n-2) + fibInit n0 n1 (n-1)

fibInitL :: Int -> Int -> Int -> [Int]
fibInitL _ _ (-1) = []
fibInitL n0 n1 n = n0 : fibInitL n1 (n0+n1) (n-1)

fibInit2 :: Int -> Int -> Int -> Int
fibInit2 n0 n1 n = lastHelp (fibInitL n0 n1 n)

lastHelp :: [Int] -> Int
lastHelp [x] = x
lastHelp (x : xs) = lastHelp xs

normalize :: [Int] -> [Int]
normalize [] = []
normalize xs = subt xs (minHelp xs)

minHelp :: [Int] -> Int
minHelp [x] = x
minHelp (x : y : xs) | x < y = minHelp (x : xs)
                     | otherwise = minHelp (y : xs)

subt :: [Int] -> Int -> [Int]
subt [] _ = []
subt (x : xs) y = (x - y) : subt xs y

sumMaxs :: [Int] -> Int
sumMaxs [] = 0
sumMaxs (x : xs) = x + sumMaxsHelp x xs

sumMaxsHelp :: Int -> [Int] -> Int
sumMaxsHelp x [] = 0
sumMaxsHelp x (y : ys) | y > x = y + sumMaxsHelp y ys
                       | otherwise = sumMaxsHelp x ys

sumNonMins :: [Int] -> Int
sumNonMins [] = 0
sumNonMins (x : xs) = sumNonMinsHelp x xs

sumNonMinsHelp :: Int -> [Int] -> Int
sumNonMinsHelp x [] = 0
sumNonMinsHelp x (y : ys) | y > x = y + sumNonMinsHelp x ys
                          | otherwise = sumNonMinsHelp y ys

primeTwins :: Int -> (Int, Int)
primeTwins n | prime (n+1) && prime (n+3) = (n+1, n+3)
             | otherwise = primeTwins (n+1)

-- prime von Aufgabe 7
prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime n = isPrime (properDivisors n 1)
  where
    isPrime            (_:_:_) = False
    isPrime            _       = True

properDivisors :: Int -> Int -> [Int]
properDivisors n m | n <= m        = []
                   | rem n m == 0  = m:properDivisors n (m+1)
                   | otherwise     = properDivisors n (m+1)

multiples :: [Int] -> Int -> Int -> [Int]
multiples [] _ _ = []
multiples (x : xs) n m | n /= m = multiples (x : xs) n n
                                  ++ multiples (x : xs) (n+1) m
                       | n == m && rem n x == 0 = n : []
                       | n == m && not (rem n x == 0) = multiples xs n m
