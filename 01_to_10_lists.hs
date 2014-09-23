module Problems_1to10 where
import Data.List(group)

-- 01 Last
last' ::  [a] -> a
last' [] = error "Empty list has no last element"
last' [x] = x
last' (_:xs) = last' xs

last'' ::  [a] -> a
last'' [] = error "Empty list has no last element"
last'' x = x !! (length x - 1)

{-# ANN module "HLint: use last" #-}
last''' ::  [a] -> a
last''' = head . reverse


-- 02 Second Last
secondLast :: [a] -> a
secondLast [] = error "List too small"
secondLast [_] = error "List too small"
secondLast [x,_] = x
secondLast (_:xs) = secondLast xs

secondLast' :: [a] -> a
secondLast' x = reverse x !! 1


-- 03 Element at (starting at 1)
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
elementAt _ _ = error "Index out of bounds."

elementAt' :: [a] -> Int -> a
elementAt' xs n
    | (length xs) < n = error "Index out of bounds"
    | otherwise = fst . last $ zip xs [1..n]


-- 04 Length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = sum . map (const 1)


-- 05 Reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


-- 06 Palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


-- 07 Flatten
data NestedList a = Elem a | List [NestedList a]
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = (flatten' x) ++ (flatten' (List xs))


-- 08 Compress
compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress xs = xs

compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)


-- 09 Pack
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first,rest) = span (== x) xs
               in (x:first) : pack rest


-- 10 Encode
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let (match, rest) = span (== x) xs
                 in (1 + length match, x) : encode (rest)

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group
