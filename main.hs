import Data.List

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charname 'b' = "Bethany"
charname 'c' = "Charles"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--xs = [(1, 3), (2, 4), (5, 6)
--[a + b | (a, b) <- xs]

head' :: [a] -> a
head' [] = error "head' can't be called on empty list"
head' (x:_) = x

-- as-pattern
firstLetter :: String -> String
firstLetter "" = "Empty string - no first letter"
firstLetter all@(x:xs) = "The first letter of '" ++ all ++ "' is " ++ [x]

-- guard
bigNumber :: Double -> Bool
bigNumber n
  | n < 10 = False
  | otherwise = True

-- infix declaration
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- where keyword
bmiTell :: Double -> Double -> String
bmiTell weight height
        | bmi <= skinny = "underweight"
        | bmi <= normal = "normal"
        | bmi <= fat = "overweight"
        | otherwise = "whale"
        where bmi = weight / height ^ 2
              skinny = 18.5
              normal = 25.0
              fat = 30.0
-- let
cylinder :: Double -> Double -> Double
cylinder r h =   
         let sideArea = 2 * pi * r * h
             topArea = pi * r * 2
         in sideArea + 2 * topArea

-- case
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a long list"

max' :: (Ord a) => [a] -> a
max' [] = error "max given empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
      | a == x = True
      | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
          let smallerOrEqual = [a | a <- xs, a <= x]
              larger = [a | a <- xs, a > x]
          in quicksort smallerOrEqual ++ [x] ++ quicksort larger


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

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

and' :: [Bool] -> Bool
and' xs = foldr1 (&&) xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sqrt $ 3 + 4 + 9
-- f = negate . (*3) 
-- f(2) = negate(*3)(2)
-- sum . replicate 5 $ max 6.7 8.9


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- @todo return a map via fromList
createMoveMap :: (Eq a) => [a] -> [(a, Int)]
createMoveMap xs = 
              let indices = zip xs [((length xs) - 1), ((length xs) - 2)..]
              in fromList $ nubBy (\a b -> fst a == fst b) $ reverse indices


-- boyer-moore
-- returns whether a contains b
contains'' :: (Eq a) => [a] -> [a] -> Bool
contains'' [] [] = False
contains'' [] x = False
contains'' x [] = False
contains'' pattern value = 
     let moveMap = createMoveMap pattern
         patternLast = last pattern
         valueAtPatternLast = value !! ((length pattern) - 1)
     in if valueAtPatternLast == patternLast then
           True
        else




-- @param start is the current position, from which to start matching backwards
contains' :: (Eq a) => [a] -> [a] -> Int -> Bool
contains' [] [] _ = False
contains' [] x _ = False
contains' x [] _ = False
contains' pattern value start = 
            let moveMap = createMoveMap pattern
                patternAt = pattern !! start
                valueAt = value !! start
            in if valueAt == patternAt then 
                 True -- @todo implement this; recursively check pattern; return move-ahead int on fail
               else 
                 contains' pattern value (Data.Map.findWithDefault ((length pattern) - 1) valueAt moveMap) -- tail call ftw

contains :: (Eq a) => [a] -> [a] ->  Bool
contains pattern value = contains' pattern value ((length pattern) - 1)
