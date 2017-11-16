
import Data.List
import Data.Char

function12 :: Int -> [Int]
function12 n = f2 1 n
-- 1 2 3 4 5 6 7 8 9 10 11 12 13
f2 :: Int -> Int -> [Int]
f2 x y 
    |x+4 <= y = [x]++[x+1] ++ f2 (x+4) y
    |x+4 > y = if((x+4 - y) == 4 ) 
        then [x]
        else [x] ++ [x+1] 

function34 :: Int -> [Int]
function34 n = [1..n] \\ function12 n

-- 48-57 === 0-9 ASCII representation of digits
-- Convert a list of integer into a String
intToChar :: [Int] -> String
intToChar (x:xs) = if (null xs)
    then [chr (48+x)]
    else [chr (48+x)] ++ intToChar xs
    

-- Splits an integer into his digits in a list
digs :: Int -> [Int]
digs 0 = []
digs x
  | x < 0 = digs ((-1) * x)
  | x > 0 = digs (div x 10) ++ [mod x 10]

-- create a list containing all the pair of number pages to print 
-- ex. printfunction12 10 = ["1", "2", "5", "6", "9", "10"]
printfunction12 :: Int -> String
printfunction12 n = addpattern (map intToChar (map digs (function12 n)))

printfunction34 :: Int -> String
printfunction34 n = addpattern (map intToChar (map digs (function34 n)))


-- adds the pattern for the correct visualization
addpattern :: [String] -> String
addpattern [] = ""
addpattern [x] = x
addpattern (x:y:xs) = x ++ "-" ++ y ++ ", " ++ addpattern xs