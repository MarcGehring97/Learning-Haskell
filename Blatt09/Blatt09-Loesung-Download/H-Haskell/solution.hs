-- a)
remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove e (x:xs) = if e==x then remove e xs else x:remove e xs

symmetricDifference :: [Int] -> [Int] -> [Int]
symmetricDifference xs ys = difference xs ys ++ difference ys xs
  where
    difference :: [Int] -> [Int] -> [Int]
    difference xs []     = xs
    difference xs (y:ys) = difference (remove y xs) ys

-- b)
powerlist :: [Int] -> [[Int]]
powerlist [] = [[]]
powerlist (x:xs) = powerlist xs ++ insertFront (powerlist xs)
  where
    insertFront :: [[Int]] -> [[Int]]
    insertFront []     = []
    insertFront (p:ps) = (x:p):insertFront ps

-- c)
permutations :: [Int] -> [[Int]]
permutations []     = [[]]
permutations [x]    = [[x]]
permutations (x:xs) = helper (permutations xs)
  where
    insertEverywhere :: [Int] -> [Int] -> [[Int]]
    insertEverywhere ps []     = [ps++[x]]
    insertEverywhere ps (e:es) =
      (ps++(x:e:es)):insertEverywhere (ps++[e]) es
    helper :: [[Int]] -> [[Int]]
    helper []     = []
    helper (l:ls) = insertEverywhere [] l ++ helper ls


-- Graphen
testGraph = [(1,2),(2,3),(3,1),(4,5),(3,4)] :: [(Int,Int)]

-- d)
nodes :: [(Int,Int)] -> [Int]
nodes xs = removeDuplicates (allNodes xs)
  where
    allNodes :: [(Int,Int)] -> [Int]
    allNodes [] = []
    allNodes ((a,b):es) = a:b:allNodes es
    removeDuplicates :: [Int] -> [Int]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x:removeDuplicates (remove x xs) -- a)

existsPath :: [(Int,Int)] -> Int -> Int -> Bool
existsPath g a b = helper [] a b g
  where
    helper :: [(Int,Int)] -> Int -> Int -> [(Int,Int)] -> Bool
    helper _  a b [] = a == b
    helper us a b ((x,y):es)
      | a == x    = existsPath (us++es) y b || helper us a b es
      | otherwise = helper ((x,y):us) a b es

-- f)
isConnected :: [(Int,Int)] -> Bool
isConnected g = checkPairs vs vs
  where
    vs :: [Int]
    vs = nodes g
    checkPairs :: [Int] -> [Int] -> Bool
    checkPairs [] _ = True
    checkPairs (_:xs) [] = checkPairs xs vs
    checkPairs (x:xs) (y:ys) = existsPath g x y && checkPairs (x:xs) ys
