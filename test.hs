minusort :: (Num a, Ord a) => [a] -> [a]
minusort [] = []
minusort (a:jaak)
    | otherwise = algus ++ [a] ++ lopp
    where algus = minusort [x | x <- jaak, x <= a]
          lopp = minusort [x | x <- jaak, x > a]
