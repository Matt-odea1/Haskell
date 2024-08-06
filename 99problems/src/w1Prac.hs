-- MySum version 1
mySum1 :: [Int] -> Int 
mySum1 [] = 0
mySum1 (n:ns) = n + mySum1 ns

myProduct1 :: [Int] -> Int 
myProduct1 [] = 1
myProduct1 (n:ns) = n * myProduct1 ns 

-- Version 2, using myBinop operator.

myBinop :: (Int -> Int -> Int) -> Int -> ([Int] -> Int)
myBinop f z [] = z
myBinop f z (n:ns) = f n (myBinop f z ns)

mySum2 :: [Int] -> Int 
mySum2 ns = myBinop (+) 0 ns 
myProduct2 :: [Int] -> Int 
myProduct2 ns = myBinop (*) 1 ns

-- Version 3, using foldr (r stands for right associative)

mySum3 :: [Int] -> Int 
mySum3 (n:ns) = foldr (+) n ns
myProduct3 :: [Int] -> Int
myProduct3 (n:ns) = foldr (*) n ns

-- Defining map using foldr

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\x xs -> f x : xs) []

factorial :: Int -> Int
factorial n 
    | n == 0 = 1
    | otherwise = n * factorial (n-1)


-----------------------------------------------------------------

fizzBuzzMult :: Int -> [String]
fizzBuzzMult n = map1 fizzBuzz [1..n]

fizzBuzz :: Int -> String
fizzBuzz n 
    | mod n 15 == 0 = "FizzBuzz"
    | mod n 5 == 0 = "Buzz"
    | mod n 3 == 0 = "Fizz"
    | otherwise = show n