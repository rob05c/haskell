merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] all@(x:xs) = all
merge all@(x:xs) [] = all
merge allx@(x:xs) ally@(y:ys)
      | x < y = x : merge xs ally
      | otherwise = y : merge allx ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort all@(x:[]) = all
mergesort x = 
          let half = length x `div` 2
          in merge (mergesort $ take half x) (mergesort $ drop half x)
