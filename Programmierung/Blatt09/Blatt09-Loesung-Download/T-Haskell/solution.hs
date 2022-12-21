fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

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

nthSmallestPerfectNumber :: Int -> Int
nthSmallestPerfectNumber n = tryAll n 1
  where
    sumList []     = 0
    sumList (x:xs) = x + sumList xs
    tryAll n k | sumList (properDivisors k 1) == k && n > 1
                  = tryAll (n-1) (k+1)
               | sumList (properDivisors k 1) == k
                  = k
               | otherwise
                  = tryAll n (k+1)

powersOfTwo :: Int -> Int -> [Int]
powersOfTwo n m | n > m  = []
                | otherwise = (2^n) : powersOfTwo (n+1) m

selectKsmallest :: Int -> [Int] -> Int
selectKsmallest _ [] = 0
selectKsmallest k (pivot:rest) =
  let
  split :: [Int] -> ([Int], [Int])
  split [] = ([], [])
  split (y:ys) = if y <= pivot then (y:left, right) else (left, y:right)
                 where (left, right) = split ys
  left, right :: [Int]
  (left, right) = split rest
  leftLen :: Int
  leftLen = length left
  in
   if leftLen == (k-1) then pivot else
     if leftLen > (k-1) then selectKsmallest k left else
       selectKsmallest (k-1-leftLen) right
