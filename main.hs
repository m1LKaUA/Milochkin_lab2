safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

mkMultiplier :: Num a => a -> (a -> a)
mkMultiplier n = \x -> x * n

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

avg :: (Fractional a) => [a] -> Maybe a
avg [] = Nothing
avg xs = Just (sum xs / fromIntegral (length xs))

processList :: [Int] -> [Int]
processList = take 3 . map (^2) . filter odd

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex item list = go item list 0
  where
    go _ [] _ = Nothing
    go val (x:xs) idx
        | val == x  = Just idx
        | otherwise = go val xs (idx + 1)

applyAll :: [a -> a] -> a -> a
applyAll [] x     = x
applyAll (f:fs) x = applyAll fs (f x)

binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch xs target = search 0 (length xs - 1)
  where
    search low high
        | low > high = Nothing
        | target == midVal = Just mid
        | target < midVal  = search low (mid - 1)
        | otherwise        = search (mid + 1) high
      where
        mid = low + (high - low) `div` 2
        midVal = xs !! mid

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort greater
  where
    less    = [a | a <- xs, a <= x]
    greater = [a | a <- xs, a > x]

type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a c b = hanoi (n - 1) a b c ++ [(a, c)] ++ hanoi (n - 1) b c a

main :: IO ()
main = do
    print $ safeHead [10, 20, 30]
    print $ safeHead ([] :: [Int])
    print $ safeDiv 10 2
    print $ safeDiv 7 0
    print $ (mkMultiplier 2) 5
    print $ applyTwice (+1) 3
    print $ myMap (*2) [1,2,3]
    print $ myFilter even [1..10]
    print $ avg [1,2,3,4]
    print $ processList [1..10]
    print $ findIndex 3 [1,2,3,4]
    print $ applyAll [(*2), (+1), (^2)] 3
    print $ binarySearch [1,3,5,7,9] 5
    print $ quickSort [3,1,4,1,5,9,2]
    print $ hanoi 2 "A" "C" "B"
