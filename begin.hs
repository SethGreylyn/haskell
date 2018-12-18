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
myBackRange = [200,199..1]
