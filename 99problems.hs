-- tests neu

-- 1. Find last elm in a list
myLast :: [a] -> a
myLast (x:xs)
    | null xs   = x
    | otherwise = myLast xs
-- 2. Find 2nd to last elm in a list
myButLast :: [a] -> a
myButLast []  = error "No buts! Empty!"
myButLast [x] = error "No buts! Singleton!"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
myButLast' = last . init
-- 3. Find kth elm in a list
elmAt :: Int -> [a] -> a
elmAt 1 (xs) = head xs
elmAt n (x:xs) = elmAt (n-1) xs
-- 4. Find the number of elements in a list
myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0
-- 5. Reverse a list
gegenwart :: [a] -> [a]
gegenwart [] = []
gegenwart (x:xs) =  gegenwart xs ++ [x]
-- 6. Find whether a list is a palindrome
pal :: (Eq a) => [a] -> Bool
pal [] = True
pal [x] = True
pal (x:xs) = ends && pal (init xs)
    where ends = x == last xs
-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: (NestedList a) -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
-- 8. Eliminate consecutive duplicates of list elements
sieveDupes :: (Eq a) => [a] -> [a]
sieveDupes [] = []
sieveDupes [x] = [x]
sieveDupes (x:y:xs)
    | x == y    = sieveDupes (x:xs)
    | otherwise = sieveDupes (y:xs)
-- 9. Pack consecutive duplicate list elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let repsHead = takeWhile (\n -> x == n)
                  repsTail = dropWhile (\n -> x == n)
              in [x : repsHead xs] ++ pack (repsTail xs)
-- 10. Run-length encoding of a list, using the solution in the previous problem
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = let packedList = pack xs
            in (length $ head $ packedList, (head packedList) !! 0) : encode (concat (tail packedList))