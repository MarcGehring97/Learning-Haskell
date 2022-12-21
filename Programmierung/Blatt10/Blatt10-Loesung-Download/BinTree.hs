import Text.Show.Functions

-- a)
data BinTree a b = Node a (BinTree a b) (BinTree a b) | Leaf b
                     deriving Show

-- b)
example :: BinTree (Int -> Bool) Char
example = Node (\x -> x > 4)
             (Node (\x -> x * x == x)
                 (Leaf 'g')
                 (Node (\x -> x == 0) (Leaf 'u') (Leaf 'l'))
             )
             (Node (\x -> x >= 7) (Leaf 'f') (Leaf 'i'))

-- c)
countInnerNodes :: BinTree a b -> Int
countInnerNodes (Leaf _) = 0
countInnerNodes (Node _ bt1 bt2) =
  1 + countInnerNodes bt1 + countInnerNodes bt2

-- d)
decodeInt :: BinTree (Int -> Bool) b -> Int -> b
decodeInt (Leaf c)       x = c
decodeInt (Node f bt1 bt2) x | f x       = decodeInt bt2 x
                             | otherwise = decodeInt bt1 x

-- e)
decode :: BinTree (Int -> Bool) b -> [Int] -> [b]
decode _ []     = []
decode bt (x:xs) = decodeInt bt x : decode bt xs

-- f)
examplebt :: BinTree Int ()
examplebt = Node 6 (Node 1
                     (Node 0 (Leaf ()) (Leaf ()))
                     (Node 2 (Leaf ()) (Leaf ())))
                  (Node 6 (Leaf ()) (Leaf ()))

insertSorted :: BinTree Int () -> Int -> BinTree Int ()
insertSorted (Leaf ())    v = Node v (Leaf ()) (Leaf ())
insertSorted (Node x l r) v
  | v < x     = Node x (insertSorted l v) r
  | otherwise = Node x l (insertSorted r v)

-- g)
treeSort :: [Int] -> [Int]
treeSort keys = toList (buildTree keys)
  where
    buildTree []     = Leaf ()
    buildTree (x:xs) = insertSorted (buildTree xs) x
    toList (Leaf ())    = []
    toList (Node x l r) = toList l ++ x:toList r


-- h)
examplebt1 :: BinTree Int ()
examplebt1 = Node 3
               (Node 1 (Leaf ()) (Leaf ()))
               (Node 6
                 (Node 5 (Leaf ()) (Leaf ()))
                 (Node 8 (Leaf ()) (Leaf ())))

examplebt2 :: BinTree Int ()
examplebt2 = Node 4
               (Node 2 (Leaf ()) (Leaf ()))
               (Node 6 (Leaf ()) (Leaf ()))


mergeTrees :: BinTree Int () -> BinTree Int () -> BinTree Int ()
mergeTrees bt (Leaf ())    = bt
mergeTrees bt (Node x l r) =
  mergeTrees (mergeTrees (insertSorted bt x) l) r

-- i)
numberOfOccurrences :: BinTree Int () -> Int -> Int
numberOfOccurrences (Leaf ()) _ = 0
numberOfOccurrences (Node x l r) v
  | v < x     = numberOfOccurrences l v
  | v > x     = numberOfOccurrences r v
  | otherwise = 1 + numberOfOccurrences r v
