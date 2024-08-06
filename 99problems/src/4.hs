{-

(*) Find the number of elements in a list.Solutions
 
Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13

-}

myLength :: [x] -> Int
myLength [] = 0
myLength (x:xs) = succ ( myLength xs )