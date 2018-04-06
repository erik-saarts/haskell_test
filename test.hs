minusort :: (Num a, Ord a) => [a] -> [a]
minusort [] = []
minusort (a:jaak)
    | otherwise = algus ++ [a] ++ lopp
    where algus = minusort [x | x <- jaak, x <= a]
          lopp = minusort [x | x <- jaak, x > a]

minuzipwidth :: ( a -> b -> c ) -> [a] -> [b] -> [c]
minuzipwidth _ [] _ = []
minuzipwidth _ _ [] = []
minuzipwidth f (a:taila) (b:tailb) = f a b : minuzipwidth f taila tailb

minuflip :: ( a -> b -> c) -> ( b -> a -> c)
minuflip f a b = f b a

minumap :: ( a -> b ) -> [a] -> [b]
minumap _ [] = []
minumap f (a:taila) = f a : minumap f taila

minufilter :: ( a -> Bool ) -> [a] -> [a]
minufilter _ [] = []
minufilter f (a:taila)
    | f a = a: minufilter f taila
    | otherwise = minufilter f taila

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | odd n = n : collatz (n*3+1)
    | otherwise = n : collatz (n `div` 2)

longerthan15 :: [a] -> Bool
longerthan15 n
    | length n > 15 = True
    | otherwise = False

foldleft :: (Num a) => (a -> a -> a) -> a -> [a] -> a
foldleft f acc [] = acc
foldleft f acc (a:taila) = foldleft f (f acc a) taila
