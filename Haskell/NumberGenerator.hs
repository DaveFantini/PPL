
import Data.List
import Data.Char

function12 :: Int -> [Int]
function12 n = f2 1 n

f2 :: Int -> Int -> [Int]
f2 x y 
    |x+4 <= y = [x]++[x+1] ++ f2 (x+4) y
    |x+4 > y = if((x+4 - y) == 1 ) 
        then [x]
        else [x] ++ [x+1] 

function34 :: Int -> [Int]
function34 n = [1..n] \\ function12 n

intToChar :: [Int] -> String
intToChar (x:xs) = if (null xs)
    then [chr (48+x)]
    else [chr (48+x)] ++ intToChar xs
    


digs :: Int -> [Int]
digs 0 = []
digs x
  | x < 0 = digs ((-1) * x)
  | x > 0 = digs (div x 10) ++ [mod x 10]
-- 48-57 === 0-9 ASCII representation of digits

printfunction12 :: Int -> [String]
printfunction12 n = map intToChar (map digs (function12 n))


addtratt :: String -> String
addtratt s = foldl  
foldl (++ "-") (printfunction12 n) 