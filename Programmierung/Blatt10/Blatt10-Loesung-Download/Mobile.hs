-- a)
data Mobile a = Stern | Seepferdchen | Elefant (Mobile a)
                      | Kaenguru a (Mobile a) (Mobile a) deriving Show

mobileLinks :: Mobile Int
mobileLinks = Kaenguru 1
                  (Elefant (Kaenguru 2
                        Stern
                        (Kaenguru 3
                            Seepferdchen
                            Stern
                        )
                  ))
                  Seepferdchen

mobileRechts :: Mobile Int
mobileRechts = Elefant (Kaenguru 1 (Elefant Stern) (Elefant Seepferdchen))

-- b)
count :: Mobile a -> Int
count Stern               = 1
count Seepferdchen        = 1
count (Elefant     m)     = 1 + count m
count (Kaenguru _  m1 m2) = 1 + count m1 + count m2

-- c)
liste :: Mobile a -> [a]
liste Stern                   = []
liste Seepferdchen            = []
liste (Elefant m)             = liste m
liste (Kaenguru inhalt m1 m2) = inhalt : liste m1 ++ liste m2


-- d)
greife :: Mobile a -> Int -> Mobile a
greife x                  1 = x
greife (Elefant m)        x = greife m (x-1)
greife (Kaenguru _ m1 m2) x
  | x-1 <= count m1  = greife m1 (x-1)
  | otherwise        = greife m2 (x-1 - count m1)
greife _                  _ = Stern -- Gesuchte Figur existiert nicht
