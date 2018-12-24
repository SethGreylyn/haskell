import Data.Char
import Data.Ratio
-- Simple function definitions
doubleMe x = 2 * x
doubleUs x y = doubleMe x + doubleMe y
doubleSmall' x = (doubleSmall x) + 1
-- Basic control flow
doubleSmall x = if x > 100
                    then x
                    else x * 2
-- Apostrophes are valid naming tokens
danO'Brian = "Conan"
-- Lists concatenation with ++ (expensive), per-element prepending with : (cheap)
leftList = [1,2,3]
rightList = [4,5,6]
myList = leftList ++ rightList
myList' = 0:myList
-- List lookup with !! (zero-based). Out-of-index errors possible.
fourthLetter = danO'Brian !! 3
secondNumber = myList' !! 1
-- Lists can be compared
isLeftBigger = leftList > rightList
-- Negation is, incidentally, accomplished with semantic literal (as opposed to a C-type exclamation point)
isLeftSmaller = not isLeftBigger
-- Lists can be chopped
myHead = head myList
myTail = tail myList
myInit = init myList
myLast = last myList
-- These operations error on empty lists; the null function checks if lists are empty (among other data types)
isMyListNull = null myList
-- Lists can also be reversed and pulled apart arbitrarily
tsiLym = reverse myList
firstThree = take 3 myList
lastFour = reverse (take 4 (reverse myList))
allButFirstThree = drop 3 myList
-- Lists can be scanned for max and min, summed, prodded, and evaluated for membership
myMax = maximum myList
myMin = minimum myList
mySum = sum myList
myProd = product myList
isElem = -20 `elem` myList
-- Lists of enumerables can be ranged over
myNumRange = [1..200]
myCharRange = ['a'..'l']
-- Ranges can have defined step length
myThirdRange = [1,3..200]
myThirdCharRange = ['a','d'..'z']
-- Ranges can be reversed with a step, too
myBackRange :: [Int]
myBackRange = [200,199..1]
-- List comprehensions are a thing
meComprende = [doubleMe x | x <- [1,5..78], mod x 3 == 2]
boomBangs ::  [Int] -> [String]
boomBangs xs = [
    if even x 
    then "BOOM"
    else "BANG" 
        | x <- xs
    ]
-- Functions with types ftw
isVowel :: Char -> Bool
isVowel x = if elem (toLower x) ['a','e', 'i', 'o', 'u']
            then True
            else False
removeAlphaChars :: String -> String
removeAlphaChars xs = [x | x <- xs, not ((toUpper x) `elem` ['A'..'Z'])]
-- Tuples can have differently-typed constituents, but must be finite
myAlphabetEnum = zip [0..250] ['A'..'Z']
-- Pattern matching ftw
isLowercaseVowel :: Char -> Bool
isLowercaseVowel 'a' = True
isLowercaseVowel 'e' = True
isLowercaseVowel 'i' = True
isLowercaseVowel 'o' = True
isLowercaseVowel 'u' = True
isLowercaseVowel _ = False
-- Also recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
--The 'as pattern', for keeping a reference to a full list
sum' :: [Int] -> String
sum' list@(x:xs) = "The sum of " ++ show list ++ " is " ++ show (sum xs) ++ ", and the first number is " ++ show x
-- Guards are akin to if-else chains, or pattern-matching functions with general boolean expressions rather than just equality
doYouEven :: (Integral a) => a -> Bool
doYouEven int
    | mod int 2 == 1 = False
    | otherwise      = True
moderateMe :: (Integral a) => a -> a -> Bool
moderateMe l r
    | l == 0 || r == 0 = error "Can't mod zero!"
    | mod l r == 0     = True
    | mod r l == 0     = True
    | otherwise        = False
-- Let and where clauses are two ways of binding local variables. Beware of shadowing.
gravity :: (Eq a, Fractional a) => a -> a -> a -> a
gravity m1 m2 r
    | r == 0    = error "Can't divide by zero!"
    | otherwise = g * invsq
    where invsq = (m1 * m2) / r^2
          g     = 6.674 * 10^^(-11)
gravity' m1 m2 r =
    if r == 0 then error "Can't divide by zero!!!" else
    let invsq = (m1 * m2) / r^2
        g     = 6.674 * 10^^(-11)
    in g * invsq
calcAvgs :: (Fractional p) => [(p,p)] -> [p]
calcAvgs pairs = [avg | (x, y) <- pairs, let avg = (x + y)/2 ]
-- Cases are a bit like switch statements, but awesomer
casetorial :: (Integral i) => i -> i
casetorial n = case n of 0 -> 1
                         n -> n * casetorial (n - 1)
-- Recursion for the awesome
newMax [] = error "Vacuous"
newMax [x] = x
newMax (x:xs)
    | x > maxTail = x
    | otherwise     = maxTail
    where maxTail = newMax xs
fibs :: [Int]
fibs = 1 : fibstail where fibstail = 1 : zipWith (+) (1:fibstail) fibstail
fib n = fibs !! n
atIndex :: [a] -> Int -> a
atIndex [] n   = error "Index out of range"
atIndex xs 0 = head xs
atIndex (x:xs) n = atIndex xs (n-1)
minimum' :: (Ord a) => [a] -> a
minimum' []     = error "Vacuous"
minimum' [x]    = x
minimum' (x:xs) = min x (minimum' xs)
replicate' :: (Num n, Ord n) => n -> t -> [t]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x
take' :: (Num a, Ord a) => a -> [x] -> [x]
take' _ []     = []
take' n _ 
    | n <= 0   = []
take' n (x:xs) = x : take' (n - 1) xs
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs) = i == x || elem' i xs
qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = (qsort' [y | y <- xs, y < x]) ++ [x] ++ (qsort' [z | z <- xs, z > x])
--Some more advanced ideas using extant functions
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (div n 2)
    | otherwise = n : collatz (n * 3 + 1)
-- Lambda expressions start with \, are surrounded with parentheses, and are anonymous functions
numLongChains :: Int
numLongChains = length (filter (\ xs -> length xs > 15) (map collatz [1..100]))
-- Folds accumulate a list into a single value
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- Reimplement map with a left fold, and again with a right fold; right folds on lists are more efficient because
-- of the efficiency of : over ++ for list operations
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\ acc x -> acc ++ [f x]) []
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\ x acc -> f x : acc) []
-- Experimental reimplementation of ++
conc :: [a] -> [a] -> [a]
conc [] ys = ys
conc xs [] = xs
conc xs ys = conc (init xs) ((last xs) : ys)
-- Scans are similar to folds, but they report the interstitial accumulators in lists
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))
-- The 'apply' function is right-associative and lowest precedence, so it can help to remove parentheses
sqrtSums' = length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]
-- This can help map lists of functions as well
funcListMap :: a -> [a -> b] -> [b]
funcListMap x = map ($x)
-- Function composition is also an awesome thing
composeMap :: (b -> c) -> (a -> b) -> [a] -> [c]
composeMap f g = map (f . g)
negSumTail :: (Num a) => [[a]] -> [a]
negSumTail = map (negate . sum . tail)