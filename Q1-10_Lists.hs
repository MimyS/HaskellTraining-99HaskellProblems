problem1 :: [a] -> a
-- Find the last element of a list.
problem1 [] = error "Empty list"
problem1 [x] = x
problem1 (_:xs) = problem1 xs

problem2 :: [a] -> a
-- Find the last but one element of a list. 
problem2 [] = error "Empty list"
problem2 ( _ : []) = error "List with only one element doesn't have last but one element"
problem2 (x : xs) = 
    if length xs == 1 then x else problem2 xs

problem3 :: [a] -> Integer -> a
-- Find the K'th element of a list. The first element in the list is number 1. 
problem3 [] _ = error "Index out of bounds"
problem3 ( x : xs) i
    | i == 1 = x
    | i < 1 = error "1st element in the list is in position number 1"
    | otherwise = problem3 xs (i-1)

problem4 :: [a] -> Integer
-- Find the number of elements of a list. 
problem4 [] = 0
problem4 (x : []) = 1
problem4 (x : xs) = 1 + problem4 xs

problem5 :: [a] -> [a]
-- Reverse a list.
problem5 [] = []
problem5 (x : xs) = problem5 xs ++ [x]

problem6 :: (Eq a) => [a] -> Bool
-- Find out whether a list is a palindrome.
problem6 x = x == problem5 x

data NestedList a = Elem a | List [NestedList a]
problem7 :: NestedList a -> [a]
-- Flatten a nested list structure. 
problem7 (Elem x) = [x]
problem7 (List x) = concatMap problem7 x

problem8 :: (Eq a) => [a] -> [a]
-- Eliminate consecutive duplicates of list elements. 
problem8 (x : []) = [x]
problem8 (x : xs)
    | x == head xs = problem8 xs
    | otherwise = x : problem8 xs

problem9 :: (Eq a) => [a] -> [[a]]
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists. 
problem9 [] = []
problem9 x = seq : problem9 rest
    where
        (seq, rest) = auxP9 x

auxP9 :: (Eq a) => [a] -> ([a], [a])
auxP9 (x : []) = ([x], [])
auxP9 (x : xs) = 
    if x == head xs
    then 
        let (seq, rest) = auxP9 xs in 
            (x : seq, rest) 
    else ([x], xs)

problem10 :: (Eq a) => [a] -> [(Int, a)]
-- Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 
problem10 x = zip ((mapLength.problem9) x) ((mapChar.problem9) x)

mapLength :: [[a]] -> [Int]
mapLength x = map(\y -> length y) x
mapChar :: [[a]] -> [a]
mapChar x = map(\y -> head y) x