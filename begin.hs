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