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
    | f a == True = a: minufilter f taila
    | otherwise = minufilter f taila
